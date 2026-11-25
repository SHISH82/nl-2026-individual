ASCII = {
  tl: '+', tm: '+', tr: '+', ml: '+', mm: '+', mr: '+',
  bl: '+', bm: '+', br: '+', h: '-', v: '|'
}.freeze

UNICODE = {
  tl: '┌', tm: '┬', tr: '┐', ml: '├', mm: '┼', mr: '┤',
  bl: '└', bm: '┴', br: '┘', h: '─', v: '│'
}.freeze

def fail(msg)
  warn "Ошибка: #{msg}"
  exit 1
end

def usage
  warn 'Использование: ruby tsv2table.rb <файл.tsv> [--unicode] [-o выходной_файл]'
  exit 1
end

def unescape(text)
  text.gsub(/\\(.)/) do
    case Regexp.last_match(1)
    when 't' then "\t"
    when 'n' then "\n"
    when 'r' then ''
    when '\\' then '\\'
    else '\\' + Regexp.last_match(1)
    end
  end
end

def number?(text)
  text.match?(/\A[-+]?\d+([.,]\d+)?\z/)
end

def cell_lines(text)
  lines = text.gsub("\t", '    ').split("\n", -1)
  lines.empty? ? [''] : lines
end

def border(widths, box, left, middle, right)
  box[left] + widths.map { |w| box[:h] * (w + 2) }.join(box[middle]) + box[right]
end

def row_lines(cells, widths, box, header)
  parts = cells.map { |c| cell_lines(c) }
  height = parts.map(&:size).max
  (0...height).map do |k|
    texts = parts.each_with_index.map do |lines, i|
      text = lines[k] || ''
      right = !header && number?(cells[i])
      ' ' + (right ? text.rjust(widths[i]) : text.ljust(widths[i])) + ' '
    end
    box[:v] + texts.join(box[:v]) + box[:v]
  end
end

path = nil
out = nil
box = ASCII
args = ARGV.dup
until args.empty?
  arg = args.shift
  case arg
  when '--unicode' then box = UNICODE
  when '--ascii' then box = ASCII
  when '-o'
    usage if args.empty?
    out = args.shift
  else
    fail("неизвестный параметр #{arg}") if arg.start_with?('-')
    usage if path
    path = arg
  end
end
usage unless path

begin
  text = File.read(path, encoding: 'UTF-8')
rescue SystemCallError
  fail("не удалось открыть файл #{path}")
end
fail("файл #{path} не в кодировке UTF-8") unless text.valid_encoding?

lines = text.split("\n").map { |l| l.chomp("\r") }
lines.pop while !lines.empty? && lines.last.empty?
fail('файл пуст') if lines.empty?

rows = lines.map { |l| l.split("\t", -1).map { |c| unescape(c) } }
header = rows.first
count = header.size
rows.each_with_index do |row, i|
  if row.size > count
    warn "Предупреждение: строка #{i + 1}: полей больше, чем в заголовке (#{row.size} вместо #{count}), лишние отброшены"
    row.slice!(count..-1)
  end
  row.fill('', row.size...count)
end

widths = (0...count).map do |i|
  rows.map { |r| cell_lines(r[i]).map(&:length).max }.max
end

result = []
result << border(widths, box, :tl, :tm, :tr)
result.concat(row_lines(header, widths, box, true))
result << border(widths, box, :ml, :mm, :mr)
rows.drop(1).each { |r| result.concat(row_lines(r, widths, box, false)) }
result << border(widths, box, :bl, :bm, :br)
table = result.join("\n") + "\n"

if out
  begin
    File.write(out, table)
  rescue SystemCallError
    fail("не удалось записать файл #{out}")
  end
  puts "таблица записана в #{out}"
else
  print table
end
$stdout.flush
warn "строк: #{rows.size - 1}, столбцов: #{count}"
