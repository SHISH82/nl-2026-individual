import std/[os, strutils, strformat]

type
  Op = object
    kind: char
    text: string
    i, j: int

proc fail(msg: string) {.noreturn.} =
  stderr.writeLine("Ошибка: " & msg)
  quit(1)

proc usage() {.noreturn.} =
  stderr.writeLine("Использование:")
  stderr.writeLine("  patch diff <старый_файл> <новый_файл> [-c контекст]")
  stderr.writeLine("  patch apply <старый_файл> <патч>")
  quit(1)

proc loadLines(path: string): seq[string] =
  var text: string
  try:
    text = readFile(path)
  except IOError:
    fail("не удалось открыть файл " & path)
  if text.len == 0:
    return @[]
  result = text.splitLines()
  if text.endsWith("\n") and result.len > 0 and result[^1] == "":
    result.setLen(result.len - 1)

proc makeOps(a, b: seq[string]): seq[Op] =
  let n = a.len
  let m = b.len
  var dp = newSeq[seq[int]](n + 1)
  for i in 0 .. n:
    dp[i] = newSeq[int](m + 1)
  for i in countdown(n - 1, 0):
    for j in countdown(m - 1, 0):
      if a[i] == b[j]:
        dp[i][j] = dp[i + 1][j + 1] + 1
      else:
        dp[i][j] = max(dp[i + 1][j], dp[i][j + 1])
  var i = 0
  var j = 0
  while i < n or j < m:
    if i < n and j < m and a[i] == b[j]:
      result.add(Op(kind: ' ', text: a[i], i: i, j: j))
      inc i
      inc j
    elif j >= m or (i < n and dp[i + 1][j] >= dp[i][j + 1]):
      result.add(Op(kind: '-', text: a[i], i: i, j: j))
      inc i
    else:
      result.add(Op(kind: '+', text: b[j], i: i, j: j))
      inc j

proc makeHunks(ops: seq[Op], c: int): seq[(int, int)] =
  var k = 0
  var lastEnd = 0
  while k < ops.len:
    if ops[k].kind == ' ':
      inc k
      continue
    let first = max(lastEnd, k - c)
    var p = k
    var q = k
    while true:
      while p < ops.len and ops[p].kind != ' ':
        inc p
      q = p
      while q < ops.len and ops[q].kind == ' ':
        inc q
      if q < ops.len and q - p <= 2 * c:
        p = q
      else:
        break
    let last = min(p + c, q)
    result.add((first, last))
    lastEnd = last
    k = last

proc header(ops: seq[Op], first, last: int): string =
  var oldCount = 0
  var newCount = 0
  for k in first ..< last:
    if ops[k].kind != '+':
      inc oldCount
    if ops[k].kind != '-':
      inc newCount
  var oldStart = ops[first].i + 1
  var newStart = ops[first].j + 1
  if oldCount == 0:
    oldStart = ops[first].i
  if newCount == 0:
    newStart = ops[first].j
  &"@@ -{oldStart},{oldCount} +{newStart},{newCount} @@"

proc parsePair(s: string, num: int): (int, int) =
  let xy = s[1 .. ^1].split(',')
  try:
    if xy.len == 1:
      return (parseInt(xy[0]), 1)
    if xy.len == 2:
      return (parseInt(xy[0]), parseInt(xy[1]))
  except ValueError:
    discard
  fail(&"неверный формат патча в строке {num}")

proc parseHeader(line: string, num: int): (int, int, int, int) =
  let parts = line.split(' ')
  if parts.len < 4 or parts[0] != "@@" or parts[3] != "@@" or
      not parts[1].startsWith("-") or not parts[2].startsWith("+"):
    fail(&"неверный формат патча в строке {num}")
  let (a, b) = parsePair(parts[1], num)
  let (c, d) = parsePair(parts[2], num)
  (a, b, c, d)

proc apply(old, patch: seq[string]): (seq[string], int) =
  var res: seq[string]
  var pos = 0
  var hunks = 0
  var k = 0
  while k < patch.len:
    let line = patch[k]
    if line.startsWith("---") or line.startsWith("+++") or line.len == 0:
      inc k
      continue
    if not line.startsWith("@@"):
      fail(&"неверный формат патча в строке {k + 1}")
    let (a, b, _, d) = parseHeader(line, k + 1)
    let start = if b == 0: a else: a - 1
    if start < pos or start > old.len:
      fail(&"блок в строке {k + 1} не подходит к файлу")
    for t in pos ..< start:
      res.add(old[t])
    pos = start
    inc k
    var seenOld = 0
    var seenNew = 0
    while k < patch.len and (seenOld < b or seenNew < d):
      let h = patch[k]
      let kind = if h.len == 0: ' ' else: h[0]
      let text = if h.len <= 1: "" else: h[1 .. ^1]
      case kind
      of ' ', '-':
        if pos >= old.len or old[pos] != text:
          let found = if pos < old.len: old[pos] else: "конец файла"
          fail(&"строка {pos + 1}: ожидалось \"{text}\", найдено \"{found}\"")
        if kind == ' ':
          res.add(text)
          inc seenNew
        inc pos
        inc seenOld
      of '+':
        res.add(text)
        inc seenNew
      else:
        fail(&"неверный формат патча в строке {k + 1}")
      inc k
    if seenOld != b or seenNew != d:
      fail("патч оборван")
    inc hunks
  for t in pos ..< old.len:
    res.add(old[t])
  (res, hunks)

proc main() =
  let args = commandLineParams()
  if args.len == 0:
    usage()
  case args[0]
  of "diff":
    var c = 3
    var files: seq[string]
    var k = 1
    while k < args.len:
      if args[k] == "-c":
        if k + 1 >= args.len:
          usage()
        try:
          c = parseInt(args[k + 1])
        except ValueError:
          fail("контекст должен быть неотрицательным целым числом")
        if c < 0:
          fail("контекст должен быть неотрицательным целым числом")
        k += 2
      else:
        files.add(args[k])
        inc k
    if files.len != 2:
      usage()
    let a = loadLines(files[0])
    let b = loadLines(files[1])
    let ops = makeOps(a, b)
    let hunks = makeHunks(ops, c)
    if hunks.len == 0:
      stderr.writeLine("файлы совпадают")
      return
    echo "--- " & files[0]
    echo "+++ " & files[1]
    var added = 0
    var removed = 0
    for (first, last) in hunks:
      echo header(ops, first, last)
      for t in first ..< last:
        echo $ops[t].kind & ops[t].text
        if ops[t].kind == '+':
          inc added
        if ops[t].kind == '-':
          inc removed
    stderr.writeLine(&"блоков: {hunks.len}, добавлено строк: {added}, удалено строк: {removed}")
  of "apply":
    if args.len != 3:
      usage()
    let old = loadLines(args[1])
    let patch = loadLines(args[2])
    let (res, hunks) = apply(old, patch)
    for line in res:
      echo line
    stderr.writeLine(&"применено блоков: {hunks}")
  else:
    usage()

main()
