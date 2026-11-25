:io.setopts(:standard_io, encoding: :unicode)
:io.setopts(:standard_error, encoding: :unicode)

defmodule Timers do
  def start_link do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def create(name, ms, owner) do
    Agent.get_and_update(__MODULE__, fn map ->
      if Map.has_key?(map, name) do
        {:exists, map}
      else
        {:ok, pid} = GenServer.start(CountdownTimer, {name, ms, owner})
        {:ok, Map.put(map, name, pid)}
      end
    end)
  end

  def find(name), do: Agent.get(__MODULE__, &Map.get(&1, name))
  def remove(name), do: Agent.update(__MODULE__, &Map.delete(&1, name))
  def all, do: Agent.get(__MODULE__, & &1)
end

defmodule CountdownTimer do
  use GenServer

  @impl true
  def init({name, ms, owner}) do
    token = make_ref()
    Process.send_after(self(), {:done, token}, ms)
    Process.monitor(owner)
    state = %{
      name: name,
      ends: now() + ms,
      left: ms,
      status: :running,
      token: token,
      subs: MapSet.new([owner])
    }
    {:ok, state}
  end

  defp now, do: System.monotonic_time(:millisecond)

  defp left(%{status: :running, ends: ends}), do: max(ends - now(), 0)
  defp left(%{left: left}), do: left

  defp notify(state, text) do
    Enum.each(state.subs, &send(&1, {:notify, text}))
  end

  @impl true
  def handle_call({:join, pid}, _from, state) do
    unless MapSet.member?(state.subs, pid), do: Process.monitor(pid)
    {:reply, :ok, %{state | subs: MapSet.put(state.subs, pid)}}
  end

  def handle_call({:leave, pid}, _from, state) do
    {:reply, :ok, %{state | subs: MapSet.delete(state.subs, pid)}}
  end

  def handle_call(:status, _from, state) do
    {:reply, {left(state), state.status}, state}
  end

  def handle_call(:pause, _from, %{status: :running} = state) do
    {:reply, :ok, %{state | status: :paused, left: left(state), token: make_ref()}}
  end

  def handle_call(:pause, _from, state) do
    {:reply, {:error, "timer already paused"}, state}
  end

  def handle_call(:resume, _from, %{status: :paused} = state) do
    token = make_ref()
    Process.send_after(self(), {:done, token}, state.left)
    {:reply, :ok, %{state | status: :running, ends: now() + state.left, token: token}}
  end

  def handle_call(:resume, _from, state) do
    {:reply, {:error, "timer is running"}, state}
  end

  def handle_call(:cancel, _from, state) do
    notify(state, "CANCELLED #{state.name}")
    Timers.remove(state.name)
    {:stop, :normal, :ok, state}
  end

  @impl true
  def handle_info({:done, token}, %{token: token} = state) do
    notify(state, "DONE #{state.name}")
    Timers.remove(state.name)
    {:stop, :normal, state}
  end

  def handle_info({:done, _}, state), do: {:noreply, state}

  def handle_info({:DOWN, _, :process, pid, _}, state) do
    {:noreply, %{state | subs: MapSet.delete(state.subs, pid)}}
  end
end

defmodule Server do
  def fail(msg) do
    IO.puts(:stderr, "Ошибка: " <> msg)
    System.halt(1)
  end

  def start(port) do
    opts = [:binary, packet: :line, active: false, reuseaddr: true]
    case :gen_tcp.listen(port, opts) do
      {:ok, listen} ->
        IO.puts("сервер запущен на порту #{port}")
        accept(listen, 1)

      {:error, reason} ->
        fail("не удалось открыть порт #{port}: #{inspect(reason)}")
    end
  end

  defp accept(listen, n) do
    {:ok, socket} = :gen_tcp.accept(listen)
    pid =
      spawn(fn ->
        receive do
          :go -> :ok
        end
        IO.puts("клиент #{n} подключился")
        :inet.setopts(socket, active: :once)
        loop(socket, n)
      end)
    :gen_tcp.controlling_process(socket, pid)
    send(pid, :go)
    accept(listen, n + 1)
  end

  defp loop(socket, n) do
    receive do
      {:tcp, ^socket, data} ->
        line = String.trim(data)
        if String.upcase(line) == "QUIT" do
          :gen_tcp.send(socket, "BYE\n")
          :gen_tcp.close(socket)
          IO.puts("клиент #{n} отключился")
        else
          reply = handle(line)
          Enum.each(reply, &:gen_tcp.send(socket, &1 <> "\n"))
          IO.puts("клиент #{n}: #{line} -> #{hd(reply)}")
          :inet.setopts(socket, active: :once)
          loop(socket, n)
        end

      {:notify, text} ->
        :gen_tcp.send(socket, text <> "\n")
        IO.puts("клиенту #{n}: #{text}")
        loop(socket, n)

      {:tcp_closed, ^socket} ->
        IO.puts("клиент #{n} отключился")
    end
  end

  defp handle(line) do
    case String.split(line) do
      [cmd | args] -> command(String.upcase(cmd), args)
      [] -> ["ERROR unknown command"]
    end
  end

  defp command("CREATE", [name, secs]) do
    case Integer.parse(secs) do
      {s, ""} when s > 0 ->
        case Timers.create(name, s * 1000, self()) do
          :ok -> ["OK"]
          :exists -> ["ERROR timer exists"]
        end

      _ ->
        ["ERROR invalid time"]
    end
  end

  defp command("JOIN", [name]), do: call(name, {:join, self()})
  defp command("LEAVE", [name]), do: call(name, {:leave, self()})
  defp command("STATUS", [name]), do: call(name, :status)
  defp command("PAUSE", [name]), do: call(name, :pause)
  defp command("RESUME", [name]), do: call(name, :resume)
  defp command("CANCEL", [name]), do: call(name, :cancel)

  defp command("LIST", []) do
    lines =
      Timers.all()
      |> Enum.sort()
      |> Enum.flat_map(fn {name, _} ->
        case call(name, :status) do
          ["ERROR" <> _] -> []
          line -> line
        end
      end)
    if lines == [], do: ["NO TIMERS", "END"], else: lines ++ ["END"]
  end

  defp command(_, _), do: ["ERROR unknown command"]

  defp call(name, msg) do
    case Timers.find(name) do
      nil ->
        ["ERROR no such timer"]

      pid ->
        try do
          case GenServer.call(pid, msg) do
            :ok -> ["OK"]
            {:error, text} -> ["ERROR " <> text]
            {left, status} -> ["#{name} #{div(left + 999, 1000)}s #{status}"]
          end
        catch
          :exit, _ -> ["ERROR no such timer"]
        end
    end
  end
end

case System.argv() do
  [text] ->
    case Integer.parse(text) do
      {port, ""} when port in 1..65535 ->
        Timers.start_link()
        Server.start(port)

      _ ->
        Server.fail("порт должен быть числом от 1 до 65535")
    end

  _ ->
    Server.fail("использование: elixir timer_server.exs <порт>")
end
