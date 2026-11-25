local function fail(msg)
  io.stderr:write("Ошибка: ", msg, "\n")
  os.exit(1)
end

local function usage()
  io.stderr:write("Использование: lua interleave.lua <файл1> <файл2> [-m строк1] [-n строк2] [-o выходной_файл]\n")
  os.exit(1)
end

local function read_lines(path)
  local f = io.open(path, "r")
  if not f then
    fail("не удалось открыть файл " .. path)
  end
  local lines = {}
  for line in f:lines() do
    lines[#lines + 1] = (line:gsub("\r$", ""))
  end
  f:close()
  return lines
end

local function to_count(text)
  local n = tonumber(text)
  if not n or n < 1 or n ~= math.floor(n) then
    fail("размер блока должен быть положительным целым числом")
  end
  return math.floor(n)
end

local files = {}
local m, n, out = 1, 1, nil
local i = 1
while i <= #arg do
  local a = arg[i]
  if a == "-m" or a == "-n" or a == "-o" then
    if i == #arg then
      usage()
    end
    local value = arg[i + 1]
    if a == "-m" then
      m = to_count(value)
    elseif a == "-n" then
      n = to_count(value)
    else
      out = value
    end
    i = i + 2
  elseif a:sub(1, 1) == "-" then
    fail("неизвестный параметр " .. a)
  else
    files[#files + 1] = a
    i = i + 1
  end
end

if #files ~= 2 then
  usage()
end
if out and (out == files[1] or out == files[2]) then
  fail("выходной файл совпадает с входным")
end

local first = read_lines(files[1])
local second = read_lines(files[2])

local result = {}
local x, y = 1, 1
while x <= #first or y <= #second do
  for _ = 1, m do
    if x <= #first then
      result[#result + 1] = first[x]
      x = x + 1
    end
  end
  for _ = 1, n do
    if y <= #second then
      result[#result + 1] = second[y]
      y = y + 1
    end
  end
end

local text = table.concat(result, "\n")
if #result > 0 then
  text = text .. "\n"
end

if out then
  local f = io.open(out, "w")
  if not f then
    fail("не удалось записать файл " .. out)
  end
  f:write(text)
  f:close()
  print("результат записан в " .. out)
else
  io.write(text)
end
io.stdout:flush()

io.stderr:write(string.format("строк из %s: %d, из %s: %d, всего: %d\n",
  files[1], #first, files[2], #second, #result))
