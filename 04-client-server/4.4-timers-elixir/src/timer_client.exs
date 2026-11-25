:io.setopts(:standard_io, encoding: :unicode)
:io.setopts(:standard_error, encoding: :unicode)

defmodule Client do
  def fail(msg) do
    IO.puts(:stderr, "Ошибка: " <> msg)
    System.halt(1)
  end

  def reader(socket, prefix, parent) do
    receive do
      {:tcp, ^socket, data} ->
        line = String.trim_trailing(data)
        if line != "END", do: IO.puts(prefix <> line)
        reader(socket, prefix, parent)

      {:tcp_closed, ^socket} ->
        send(parent, :closed)
    end
  end

  def input(socket, prefix, tty) do
    case IO.gets(if tty, do: prefix <> "> ", else: "") do
      :eof -> finish(socket)
      {:error, _} -> finish(socket)
      line -> command(socket, prefix, tty, String.trim(line))
    end
  end

  defp command(socket, prefix, tty, cmd) do
    cond do
      cmd == "" ->
        input(socket, prefix, tty)

      String.starts_with?(cmd, "sleep ") ->
        case Float.parse(String.trim_leading(cmd, "sleep ")) do
          {secs, _} when secs >= 0 -> Process.sleep(round(secs * 1000))
          _ -> IO.puts(:stderr, "пауза должна быть числом секунд")
        end
        input(socket, prefix, tty)

      true ->
        unless tty, do: IO.puts(prefix <> "> " <> cmd)
        :gen_tcp.send(socket, cmd <> "\n")
        Process.sleep(300)
        if String.upcase(cmd) == "QUIT", do: wait_close(), else: input(socket, prefix, tty)
    end
  end

  defp finish(socket) do
    :gen_tcp.send(socket, "QUIT\n")
    wait_close()
  end

  defp wait_close do
    receive do
      :closed -> :ok
    after
      2000 -> :ok
    end
  end
end

{host, port, name} =
  case System.argv() do
    [h, p | rest] ->
      case Integer.parse(p) do
        {n, ""} when n in 1..65535 -> {h, n, List.first(rest)}
        _ -> Client.fail("порт должен быть числом от 1 до 65535")
      end

    _ ->
      Client.fail("использование: elixir timer_client.exs <хост> <порт> [имя]")
  end

prefix = if name, do: "[#{name}] ", else: ""

case :gen_tcp.connect(String.to_charlist(host), port, [:binary, packet: :line, active: false], 5000) do
  {:ok, socket} ->
    IO.puts(prefix <> "подключено к #{host}:#{port}")
    parent = self()
    pid = spawn(fn -> Client.reader(socket, prefix, parent) end)
    :gen_tcp.controlling_process(socket, pid)
    :inet.setopts(socket, active: true)
    tty = :io.getopts(:standard_io)[:terminal] == true
    Client.input(socket, prefix, tty)

  {:error, reason} ->
    Client.fail("не удалось подключиться к #{host}:#{port}: #{inspect(reason)}")
end
