function fail(msg)
    println(stderr, "Ошибка: ", msg)
    exit(1)
end

function usage()
    println(stderr, "Использование:")
    println(stderr, "  julia -t <потоки> localmax.jl <файл> [-o выходной_файл]")
    println(stderr, "  julia localmax.jl gen <строки> <столбцы>")
    exit(1)
end

function read_matrix(path)
    isfile(path) || fail("не удалось открыть файл $path")
    rows = Vector{Vector{Float64}}()
    for (i, line) in enumerate(eachline(path))
        parts = split(line)
        isempty(parts) && continue
        row = Float64[]
        for p in parts
            x = tryparse(Float64, p)
            x === nothing && fail("строка $i: не число \"$p\"")
            push!(row, x)
        end
        if !isempty(rows) && length(row) != length(rows[1])
            fail("строка $i: количество чисел $(length(row)), ожидалось $(length(rows[1]))")
        end
        push!(rows, row)
    end
    isempty(rows) && fail("матрица пуста")
    m = Matrix{Float64}(undef, length(rows), length(rows[1]))
    for i in eachindex(rows)
        m[i, :] = rows[i]
    end
    m
end

function ismax(m, i, j)
    n, k = size(m)
    x = m[i, j]
    for di in -1:1, dj in -1:1
        if di == 0 && dj == 0
            continue
        end
        a = i + di
        b = j + dj
        if 1 <= a <= n && 1 <= b <= k && m[a, b] >= x
            return false
        end
    end
    true
end

function find_seq(m)
    result = Tuple{Int,Int}[]
    n, k = size(m)
    for i in 1:n, j in 1:k
        ismax(m, i, j) && push!(result, (i, j))
    end
    result
end

function find_par(m, t)
    n, k = size(m)
    tasks = map(1:t) do p
        first = div((p - 1) * n, t) + 1
        last = div(p * n, t)
        Threads.@spawn begin
            part = Tuple{Int,Int}[]
            for i in first:last, j in 1:k
                ismax(m, i, j) && push!(part, (i, j))
            end
            part
        end
    end
    sort(reduce(vcat, fetch.(tasks)))
end

function best(f)
    minimum(@elapsed(f()) for _ in 1:5)
end

value(x) = isinteger(x) && abs(x) < 1e15 ? string(Int(x)) : string(x)

ms(t) = string(round(t * 1000, digits = 2))

function generate(args)
    length(args) == 3 || usage()
    n = tryparse(Int, args[2])
    k = tryparse(Int, args[3])
    if n === nothing || k === nothing || n <= 0 || k <= 0
        fail("размеры должны быть положительными целыми числами")
    end
    for _ in 1:n
        println(join(rand(0:999, k), " "))
    end
end

function main()
    isempty(ARGS) && usage()
    if ARGS[1] == "gen"
        generate(ARGS)
        return
    end

    path = ""
    out = ""
    i = 1
    while i <= length(ARGS)
        if ARGS[i] == "-o"
            i == length(ARGS) && usage()
            out = ARGS[i + 1]
            i += 2
        else
            path = ARGS[i]
            i += 1
        end
    end
    isempty(path) && usage()

    m = read_matrix(path)
    t = Threads.nthreads()
    first = find_seq(m)
    second = find_par(m, t)
    time1 = best(() -> find_seq(m))
    time2 = best(() -> find_par(m, t))

    if !isempty(out)
        try
            open(out, "w") do f
                for (a, b) in second
                    println(f, a, " ", b, " ", value(m[a, b]))
                end
            end
        catch
            fail("не удалось записать файл $out")
        end
    end

    println("матрица: $(size(m, 1)) x $(size(m, 2))")
    println("найдено: $(length(second))")
    if !isempty(out)
        println("список записан в $out")
    elseif length(second) <= 50
        for (a, b) in second
            println("($a, $b) = $(value(m[a, b]))")
        end
    else
        println("максимумов слишком много для консоли, укажите -o <файл>")
    end
    println("последовательно: $(ms(time1)) мс")
    println("параллельно, потоков $t: $(ms(time2)) мс")
    println("ускорение: $(round(time1 / time2, digits = 2))")
    println("результаты совпадают: $(first == second ? "да" : "нет")")
end

main()
