#if canImport(Darwin)
import Darwin
#else
import Glibc
#endif

func fail(_ msg: String) -> Never {
    fputs("Ошибка: \(msg)\n", stderr)
    exit(1)
}

func usage() -> Never {
    fputs("Использование: ppm2pgm <вход.ppm> <выход.pgm> [--ascii|--binary] [--method luma|avg|light]\n", stderr)
    exit(1)
}

func readFile(_ path: String) -> [UInt8] {
    guard let f = fopen(path, "rb") else { fail("не удалось открыть файл \(path)") }
    defer { fclose(f) }
    fseek(f, 0, SEEK_END)
    let size = ftell(f)
    fseek(f, 0, SEEK_SET)
    var data = [UInt8](repeating: 0, count: max(size, 0))
    if size > 0 && fread(&data, 1, data.count, f) != data.count {
        fail("ошибка чтения файла \(path)")
    }
    return data
}

func writeFile(_ path: String, _ data: [UInt8]) {
    guard let f = fopen(path, "wb") else { fail("не удалось записать файл \(path)") }
    defer { fclose(f) }
    if fwrite(data, 1, data.count, f) != data.count {
        fail("не удалось записать файл \(path)")
    }
}

func isSpace(_ c: UInt8) -> Bool {
    return c == 32 || c == 9 || c == 10 || c == 13 || c == 11 || c == 12
}

let hash: UInt8 = 35

struct Reader {
    let data: [UInt8]
    var pos = 0

    mutating func skipSpace() {
        while pos < data.count {
            if data[pos] == hash {
                while pos < data.count && data[pos] != 10 {
                    pos += 1
                }
            } else if isSpace(data[pos]) {
                pos += 1
            } else {
                return
            }
        }
    }

    mutating func token() -> String? {
        skipSpace()
        let start = pos
        while pos < data.count && !isSpace(data[pos]) && data[pos] != hash {
            pos += 1
        }
        if start == pos {
            return nil
        }
        return String(decoding: data[start..<pos], as: UTF8.self)
    }

    mutating func number() -> Int? {
        guard let t = token(), let n = Int(t), n >= 0 else { return nil }
        return n
    }
}

func gray(_ r: Int, _ g: Int, _ b: Int, _ method: String) -> Int {
    switch method {
    case "avg":
        return Int((Double(r + g + b) / 3).rounded())
    case "light":
        return Int((Double(max(r, g, b) + min(r, g, b)) / 2).rounded())
    default:
        return Int((0.299 * Double(r) + 0.587 * Double(g) + 0.114 * Double(b)).rounded())
    }
}

var files: [String] = []
var format = ""
var method = "luma"
var args = Array(CommandLine.arguments.dropFirst())
while !args.isEmpty {
    let a = args.removeFirst()
    switch a {
    case "--ascii":
        format = "P2"
    case "--binary":
        format = "P5"
    case "--method":
        if args.isEmpty { usage() }
        method = args.removeFirst()
        if !["luma", "avg", "light"].contains(method) {
            fail("способ должен быть luma, avg или light")
        }
    default:
        if a.hasPrefix("-") { fail("неизвестный параметр \(a)") }
        files.append(a)
    }
}
if files.count != 2 { usage() }

var reader = Reader(data: readFile(files[0]))
guard let magic = reader.token(), magic == "P3" || magic == "P6" else {
    fail("ожидался PPM (P3 или P6)")
}
guard let width = reader.number(), let height = reader.number(), let maxval = reader.number(),
      width > 0, height > 0, maxval > 0, maxval <= 65535 else {
    fail("некорректный заголовок PPM")
}

let count = width * height
var pixels = [Int](repeating: 0, count: count * 3)
if magic == "P3" {
    for i in 0..<count * 3 {
        guard let v = reader.number() else { fail("недостаточно данных пикселей") }
        if v > maxval { fail("значение \(v) больше maxval \(maxval)") }
        pixels[i] = v
    }
} else {
    reader.pos += 1
    let size = maxval < 256 ? 1 : 2
    if reader.data.count - reader.pos < count * 3 * size {
        fail("недостаточно данных пикселей")
    }
    for i in 0..<count * 3 {
        let p = reader.pos + i * size
        let v = size == 1 ? Int(reader.data[p]) : Int(reader.data[p]) << 8 | Int(reader.data[p + 1])
        if v > maxval { fail("значение \(v) больше maxval \(maxval)") }
        pixels[i] = v
    }
}

var result = [Int](repeating: 0, count: count)
for i in 0..<count {
    result[i] = min(maxval, gray(pixels[3 * i], pixels[3 * i + 1], pixels[3 * i + 2], method))
}

if format.isEmpty {
    format = magic == "P3" ? "P2" : "P5"
}
var out = Array("\(format)\n\(width) \(height)\n\(maxval)\n".utf8)
if format == "P2" {
    var line: [String] = []
    for v in result {
        line.append(String(v))
        if line.count == 12 {
            out += Array((line.joined(separator: " ") + "\n").utf8)
            line = []
        }
    }
    if !line.isEmpty {
        out += Array((line.joined(separator: " ") + "\n").utf8)
    }
} else {
    for v in result {
        if maxval < 256 {
            out.append(UInt8(v))
        } else {
            out.append(UInt8(v >> 8))
            out.append(UInt8(v & 255))
        }
    }
}
writeFile(files[1], out)
print("\(files[0]): PPM \(magic), \(width)x\(height), maxval \(maxval)")
print("\(files[1]): PGM \(format), способ: \(method), байт: \(out.count)")
