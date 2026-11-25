import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicInteger

System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out), true, "UTF-8"))
System.setErr(new PrintStream(new FileOutputStream(FileDescriptor.err), true, "UTF-8"))

class JsonError extends Exception {
    JsonError(String msg) { super(msg) }
}

class Parser {
    String s
    int pos = 0

    Parser(String s) { this.s = s }

    Object parse() {
        def value = parseValue()
        skip()
        if (pos < s.length()) {
            error("лишние символы после документа")
        }
        value
    }

    void error(String msg) {
        throw new JsonError("позиция ${pos + 1}: ${msg}")
    }

    void skip() {
        while (pos < s.length() && Character.isWhitespace(s.charAt(pos))) {
            pos++
        }
    }

    Object parseValue() {
        skip()
        if (pos >= s.length()) {
            error("неожиданный конец документа")
        }
        char c = s.charAt(pos)
        if (c == '{' as char) return parseObject()
        if (c == '[' as char) return parseArray()
        if (c == '"' as char) return parseString()
        if (c == '-' as char || Character.isDigit(c)) return parseNumber()
        if (s.startsWith("true", pos)) { pos += 4; return true }
        if (s.startsWith("false", pos)) { pos += 5; return false }
        if (s.startsWith("null", pos)) { pos += 4; return null }
        error("неожиданный символ '${c}'")
    }

    Map parseObject() {
        def result = new LinkedHashMap()
        pos++
        skip()
        if (pos < s.length() && s.charAt(pos) == '}' as char) {
            pos++
            return result
        }
        while (true) {
            skip()
            if (pos >= s.length() || s.charAt(pos) != '"' as char) {
                error("ожидалось имя поля в кавычках")
            }
            def key = parseString()
            skip()
            if (pos >= s.length() || s.charAt(pos) != ':' as char) {
                error("ожидалось ':'")
            }
            pos++
            result[key] = parseValue()
            skip()
            if (pos < s.length() && s.charAt(pos) == ',' as char) {
                pos++
            } else if (pos < s.length() && s.charAt(pos) == '}' as char) {
                pos++
                return result
            } else {
                error("ожидалось ',' или '}'")
            }
        }
    }

    List parseArray() {
        def result = []
        pos++
        skip()
        if (pos < s.length() && s.charAt(pos) == ']' as char) {
            pos++
            return result
        }
        while (true) {
            result << parseValue()
            skip()
            if (pos < s.length() && s.charAt(pos) == ',' as char) {
                pos++
            } else if (pos < s.length() && s.charAt(pos) == ']' as char) {
                pos++
                return result
            } else {
                error("ожидалось ',' или ']'")
            }
        }
    }

    String parseString() {
        def sb = new StringBuilder()
        pos++
        while (true) {
            if (pos >= s.length()) {
                error("незакрытая строка")
            }
            char c = s.charAt(pos++)
            if (c == '"' as char) {
                return sb.toString()
            }
            if (c != '\\' as char) {
                sb.append(c)
                continue
            }
            if (pos >= s.length()) {
                error("незакрытая строка")
            }
            char e = s.charAt(pos++)
            switch (e) {
                case '"': sb.append('"'); break
                case '\\': sb.append('\\'); break
                case '/': sb.append('/'); break
                case 'b': sb.append('\b'); break
                case 'f': sb.append('\f'); break
                case 'n': sb.append('\n'); break
                case 'r': sb.append('\r'); break
                case 't': sb.append('\t'); break
                case 'u':
                    if (pos + 4 > s.length()) {
                        error("неверная последовательность \\u")
                    }
                    try {
                        sb.append((char) Integer.parseInt(s.substring(pos, pos + 4), 16))
                    } catch (NumberFormatException ignored) {
                        error("неверная последовательность \\u")
                    }
                    pos += 4
                    break
                default:
                    error("неверная escape-последовательность \\${e}")
            }
        }
    }

    BigDecimal parseNumber() {
        int start = pos
        if (s.charAt(pos) == '-' as char) pos++
        while (pos < s.length() && "0123456789.eE+-".indexOf((int) s.charAt(pos)) >= 0) {
            pos++
        }
        try {
            return new BigDecimal(s.substring(start, pos))
        } catch (NumberFormatException ignored) {
            pos = start
            error("неверное число")
        }
    }
}

static String toJson(Object v) {
    if (v == null) return "null"
    if (v instanceof Boolean) return v.toString()
    if (v instanceof BigDecimal) return v.toPlainString()
    if (v instanceof String) {
        def sb = new StringBuilder('"')
        for (char c : v.toCharArray()) {
            switch (c) {
                case '"': sb.append('\\"'); break
                case '\\': sb.append('\\\\'); break
                case '\n': sb.append('\\n'); break
                case '\r': sb.append('\\r'); break
                case '\t': sb.append('\\t'); break
                default:
                    if (c < (' ' as char)) {
                        sb.append(String.format("\\u%04x", (int) c))
                    } else {
                        sb.append(c)
                    }
            }
        }
        return sb.append('"').toString()
    }
    if (v instanceof List) return "[" + v.collect { toJson(it) }.join(",") + "]"
    if (v instanceof Map) return "{" + v.collect { k, x -> toJson(k) + ":" + toJson(x) }.join(",") + "}"
    throw new IllegalArgumentException("неизвестный тип")
}

static String typeName(Object v) {
    if (v == null) return "null"
    if (v instanceof Boolean) return "boolean"
    if (v instanceof BigDecimal) return "number"
    if (v instanceof String) return "string"
    if (v instanceof List) return "array"
    "object"
}

static void diff(Object a, Object b, String path, List out) {
    if (typeName(a) != typeName(b)) {
        out << "TYPE ${path}: ${typeName(a)} -> ${typeName(b)}"
        return
    }
    if (a instanceof Map) {
        a.each { k, v ->
            if (!b.containsKey(k)) {
                out << "REMOVED ${path}.${k}"
            } else {
                diff(v, b[k], "${path}.${k}", out)
            }
        }
        b.each { k, v ->
            if (!a.containsKey(k)) {
                out << "ADDED ${path}.${k}: ${toJson(v)}"
            }
        }
    } else if (a instanceof List) {
        int n = Math.max(a.size(), b.size())
        for (int i = 0; i < n; i++) {
            if (i >= b.size()) {
                out << "REMOVED ${path}[${i}]"
            } else if (i >= a.size()) {
                out << "ADDED ${path}[${i}]: ${toJson(b[i])}"
            } else {
                diff(a[i], b[i], "${path}[${i}]", out)
            }
        }
    } else if (a instanceof BigDecimal) {
        if (a.compareTo(b) != 0) {
            out << "CHANGED ${path}: ${toJson(a)} -> ${toJson(b)}"
        }
    } else if (a != b) {
        out << "CHANGED ${path}: ${toJson(a)} -> ${toJson(b)}"
    }
}

static List compare(String first, String second) {
    def docs = []
    [first, second].eachWithIndex { text, i ->
        try {
            docs << new Parser(text ?: "").parse()
        } catch (JsonError e) {
            throw new JsonError("документ ${i + 1}: ${e.message}")
        }
    }
    def out = []
    diff(docs[0], docs[1], '$', out)
    out
}

static void fail(String msg) {
    System.err.println("Ошибка: " + msg)
    System.exit(1)
}

static int toPort(String text) {
    try {
        int p = Integer.parseInt(text)
        if (p > 0 && p < 65536) return p
    } catch (NumberFormatException ignored) {
    }
    fail("порт должен быть числом от 1 до 65535")
    0
}

static void runServer(int port) {
    ServerSocket server
    try {
        server = new ServerSocket(port)
    } catch (IOException e) {
        fail("не удалось открыть порт ${port}: ${e.message}")
    }
    println "сервер запущен на порту ${port}"
    def counter = new AtomicInteger(0)
    while (true) {
        def socket = server.accept()
        int n = counter.incrementAndGet()
        Thread.start {
            println "клиент ${n} подключился"
            socket.withCloseable {
                def reader = new BufferedReader(new InputStreamReader(socket.inputStream, StandardCharsets.UTF_8))
                def writer = new PrintWriter(new OutputStreamWriter(socket.outputStream, StandardCharsets.UTF_8), true)
                def first = reader.readLine()
                def second = reader.readLine()
                try {
                    def out = compare(first, second)
                    if (out.isEmpty()) {
                        writer.println("EQUAL")
                    } else {
                        out.each { writer.println(it) }
                    }
                    println "клиент ${n}: различий ${out.size()}"
                } catch (JsonError e) {
                    writer.println("ERROR " + e.message)
                    println "клиент ${n}: ошибка разбора (${e.message})"
                }
                writer.println("END")
            }
            println "клиент ${n} отключился"
        }
    }
}

static void runClient(String host, int port, String fileA, String fileB) {
    def lines = []
    [fileA, fileB].each { path ->
        def f = new File(path)
        if (!f.isFile()) {
            fail("не удалось открыть файл ${path}")
        }
        try {
            lines << toJson(new Parser(f.getText("UTF-8")).parse())
        } catch (JsonError e) {
            fail("файл ${path}: ${e.message}")
        }
    }
    Socket socket
    try {
        socket = new Socket(host, port)
    } catch (IOException e) {
        fail("не удалось подключиться к ${host}:${port}")
    }
    socket.withCloseable {
        def reader = new BufferedReader(new InputStreamReader(socket.inputStream, StandardCharsets.UTF_8))
        def writer = new PrintWriter(new OutputStreamWriter(socket.outputStream, StandardCharsets.UTF_8), true)
        writer.println(lines[0])
        writer.println(lines[1])
        println "сравнение ${fileA} и ${fileB}:"
        while (true) {
            def line = reader.readLine()
            if (line == null || line == "END") break
            println line
        }
    }
}

if (args.length == 2 && args[0] == "server") {
    runServer(toPort(args[1]))
} else if (args.length == 5 && args[0] == "client") {
    runClient(args[1], toPort(args[2]), args[3], args[4])
} else {
    System.err.println("Использование:")
    System.err.println("  groovy jsondiff.groovy server <порт>")
    System.err.println("  groovy jsondiff.groovy client <хост> <порт> <файл1.json> <файл2.json>")
    System.exit(1)
}
