open System
open System.IO
open System.Text
open System.Text.RegularExpressions

exception InputError of string

let fail (msg: string) =
    eprintfn "Ошибка: %s" msg
    exit 1

let usage () =
    eprintfn "Использование: dotnet fsi MdToLatex.fsx <файл.md> [--table] [--caption подпись] [-o выходной_файл]"
    exit 1

let escape (text: string) =
    let sb = StringBuilder()
    for c in text do
        match c with
        | '\\' -> sb.Append(@"\textbackslash{}") |> ignore
        | '&' | '%' | '$' | '#' | '_' | '{' | '}' -> sb.Append('\\').Append(c) |> ignore
        | '~' -> sb.Append(@"\textasciitilde{}") |> ignore
        | '^' -> sb.Append(@"\textasciicircum{}") |> ignore
        | '|' -> sb.Append(@"\textbar{}") |> ignore
        | '<' -> sb.Append(@"\textless{}") |> ignore
        | '>' -> sb.Append(@"\textgreater{}") |> ignore
        | _ -> sb.Append(c) |> ignore
    sb.ToString()

let inlinePattern = Regex(@"`([^`]+)`|\*\*([^*]+)\*\*|\*([^*]+)\*")

let convertInline (text: string) =
    let sb = StringBuilder()
    let mutable last = 0
    for m in inlinePattern.Matches(text) do
        sb.Append(escape (text.Substring(last, m.Index - last))) |> ignore
        if m.Groups.[1].Success then
            sb.Append(@"\texttt{").Append(escape m.Groups.[1].Value).Append("}") |> ignore
        elif m.Groups.[2].Success then
            sb.Append(@"\textbf{").Append(escape m.Groups.[2].Value).Append("}") |> ignore
        else
            sb.Append(@"\textit{").Append(escape m.Groups.[3].Value).Append("}") |> ignore
        last <- m.Index + m.Length
    sb.Append(escape (text.Substring(last))) |> ignore
    sb.ToString()

let splitCells (line: string) =
    let t = line.Trim()
    let cells = ResizeArray<string>()
    let cur = StringBuilder()
    let mutable i = 0
    while i < t.Length do
        if t.[i] = '\\' && i + 1 < t.Length && t.[i + 1] = '|' then
            cur.Append('|') |> ignore
            i <- i + 2
        elif t.[i] = '|' then
            cells.Add(cur.ToString())
            cur.Clear() |> ignore
            i <- i + 1
        else
            cur.Append(t.[i]) |> ignore
            i <- i + 1
    cells.Add(cur.ToString())
    let list = cells |> Seq.map (fun c -> c.Trim()) |> List.ofSeq
    let list = if t.StartsWith("|") then List.tail list else list
    let list = if t.EndsWith("|") && not (t.EndsWith(@"\|")) then List.take (list.Length - 1) list else list
    list

let separator = Regex(@"^:?-+:?$")

let alignment (cell: string) =
    if cell.StartsWith(":") && cell.EndsWith(":") then "c"
    elif cell.EndsWith(":") then "r"
    else "l"

let convertTable (rows: (int * string) list) (wrap: bool) (caption: string) =
    let (headNum, headLine) = rows.Head
    let header = splitCells headLine
    if rows.Length < 2 then
        raise (InputError(sprintf "строка %d: ожидалась строка-разделитель" (headNum + 1)))
    let (sepNum, sepLine) = rows.[1]
    let seps = splitCells sepLine
    if not (seps |> List.forall (fun c -> separator.IsMatch(c))) then
        raise (InputError(sprintf "строка %d: ожидалась строка-разделитель" sepNum))
    if seps.Length <> header.Length then
        raise (InputError(sprintf "строка %d: ячеек %d, ожидалось %d" sepNum seps.Length header.Length))
    let body =
        rows
        |> List.skip 2
        |> List.map (fun (num, line) ->
            let cells = splitCells line
            if cells.Length <> header.Length then
                raise (InputError(sprintf "строка %d: ячеек %d, ожидалось %d" num cells.Length header.Length))
            cells)
    let spec = "|" + (seps |> List.map alignment |> String.concat "|") + "|"
    let row (cells: string list) = (cells |> String.concat " & ") + @" \\"
    let lines = ResizeArray<string>()
    if wrap then
        lines.Add(@"\begin{table}[h]")
        lines.Add(@"\centering")
        if caption <> "" then
            lines.Add(@"\caption{" + escape caption + "}")
    lines.Add(@"\begin{tabular}{" + spec + "}")
    lines.Add(@"\hline")
    lines.Add(row (header |> List.map (fun c -> @"\textbf{" + convertInline c + "}")))
    lines.Add(@"\hline")
    for cells in body do
        lines.Add(row (cells |> List.map convertInline))
    lines.Add(@"\hline")
    lines.Add(@"\end{tabular}")
    if wrap then
        lines.Add(@"\end{table}")
    String.Join("\n", lines)

let findTables (lines: string[]) =
    let blocks = ResizeArray<(int * string) list>()
    let cur = ResizeArray<int * string>()
    let flush () =
        if cur.Count >= 2 || (cur.Count = 1 && cur.[0] |> snd |> fun l -> l.Trim().StartsWith("|")) then
            blocks.Add(List.ofSeq cur)
        cur.Clear()
    lines
    |> Array.iteri (fun i line ->
        if line.Trim() <> "" && line.Contains("|") then cur.Add((i + 1, line)) else flush ())
    flush ()
    List.ofSeq blocks

let args = fsi.CommandLineArgs |> Array.skip 1 |> List.ofArray

let rec parse (rest: string list) path wrap caption out =
    match rest with
    | [] -> (path, wrap, caption, out)
    | "--table" :: tail -> parse tail path true caption out
    | "--caption" :: value :: tail -> parse tail path true value out
    | "-o" :: value :: tail -> parse tail path wrap caption (Some value)
    | flag :: _ when flag.StartsWith("-") -> fail (sprintf "неизвестный параметр %s" flag)
    | file :: tail ->
        if path <> "" then usage ()
        parse tail file wrap caption out

let (path, wrap, caption, out) = parse args "" false "" None
if path = "" then usage ()

let lines =
    try
        File.ReadAllLines(path, Encoding.UTF8)
    with _ ->
        fail (sprintf "не удалось открыть файл %s" path)

let tables = findTables lines
if tables.IsEmpty then fail "в файле нет таблиц"

let result =
    try
        tables |> List.map (fun t -> convertTable t wrap caption) |> String.concat "\n\n"
    with InputError msg ->
        fail msg

match out with
| Some file ->
    try
        File.WriteAllText(file, result + "\n", UTF8Encoding(false))
    with _ ->
        fail (sprintf "не удалось записать файл %s" file)
    printfn "результат записан в %s" file
| None ->
    printfn "%s" result

eprintfn "таблиц: %d" tables.Length
