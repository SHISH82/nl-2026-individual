import java.io.BufferedReader
import java.io.FileDescriptor
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.io.PrintStream
import java.io.PrintWriter
import java.math.BigDecimal
import java.math.MathContext
import java.net.ServerSocket
import java.net.Socket
import java.util.concurrent.atomic.AtomicInteger
import kotlin.concurrent.thread
import kotlin.system.exitProcess

data class Measure(val category: String, val factor: Double)

val units = linkedMapOf(
    "mm" to Measure("length", 0.001),
    "cm" to Measure("length", 0.01),
    "m" to Measure("length", 1.0),
    "km" to Measure("length", 1000.0),
    "in" to Measure("length", 0.0254),
    "ft" to Measure("length", 0.3048),
    "mi" to Measure("length", 1609.344),
    "g" to Measure("mass", 0.001),
    "kg" to Measure("mass", 1.0),
    "t" to Measure("mass", 1000.0),
    "oz" to Measure("mass", 0.028349523125),
    "lb" to Measure("mass", 0.45359237),
    "C" to Measure("temperature", 1.0),
    "F" to Measure("temperature", 1.0),
    "K" to Measure("temperature", 1.0),
    "s" to Measure("time", 1.0),
    "min" to Measure("time", 60.0),
    "h" to Measure("time", 3600.0),
    "day" to Measure("time", 86400.0),
)

fun fail(msg: String): Nothing {
    System.err.println("Ошибка: $msg")
    exitProcess(1)
}

fun toKelvin(v: Double, unit: String) = when (unit) {
    "C" -> v + 273.15
    "F" -> (v - 32) * 5 / 9 + 273.15
    else -> v
}

fun fromKelvin(k: Double, unit: String) = when (unit) {
    "C" -> k - 273.15
    "F" -> (k - 273.15) * 9 / 5 + 32
    else -> k
}

fun format(x: Double): String =
    BigDecimal(x).round(MathContext(6)).stripTrailingZeros().toPlainString()

fun convert(parts: List<String>): String {
    if (parts.size != 4) return "ERROR usage: CONVERT <value> <from> <to>"
    val value = parts[1].replace(',', '.').toDoubleOrNull()
    if (value == null || !value.isFinite()) return "ERROR invalid number"
    val from = units[parts[2]] ?: return "ERROR unknown unit: ${parts[2]}"
    val to = units[parts[3]] ?: return "ERROR unknown unit: ${parts[3]}"
    if (from.category != to.category) {
        return "ERROR incompatible units: ${from.category} and ${to.category}"
    }
    val result = if (from.category == "temperature") {
        val k = toKelvin(value, parts[2])
        if (k < 0) return "ERROR below absolute zero"
        fromKelvin(k, parts[3])
    } else {
        value * from.factor / to.factor
    }
    return "OK ${format(result)} ${parts[3]}"
}

fun answer(line: String): List<String> {
    val parts = line.trim().split(Regex("\\s+"))
    return when (parts[0].uppercase()) {
        "CONVERT" -> listOf(convert(parts))
        "LIST" -> units.values.map { it.category }.distinct().map { c ->
            "$c: " + units.filterValues { it.category == c }.keys.joinToString(" ")
        } + "END"
        else -> listOf("ERROR unknown command")
    }
}

fun runServer(port: Int) {
    val server = try {
        ServerSocket(port)
    } catch (e: IOException) {
        fail("не удалось открыть порт $port: ${e.message}")
    }
    println("сервер запущен на порту $port")
    val counter = AtomicInteger(0)
    while (true) {
        val socket = server.accept()
        val n = counter.incrementAndGet()
        thread {
            println("клиент $n подключился")
            socket.use {
                val reader = BufferedReader(InputStreamReader(socket.getInputStream(), Charsets.UTF_8))
                val writer = PrintWriter(OutputStreamWriter(socket.getOutputStream(), Charsets.UTF_8), true)
                while (true) {
                    val line = reader.readLine() ?: break
                    val cmd = line.trim()
                    if (cmd.isEmpty()) continue
                    if (cmd.uppercase() == "QUIT") {
                        writer.println("BYE")
                        break
                    }
                    val result = answer(cmd)
                    result.forEach { writer.println(it) }
                    println("клиент $n: $cmd -> ${result.first()}")
                }
            }
            println("клиент $n отключился")
        }
    }
}

fun runClient(host: String, port: Int) {
    val socket = try {
        Socket(host, port)
    } catch (e: IOException) {
        fail("не удалось подключиться к $host:$port")
    }
    println("подключено к $host:$port")
    val tty = System.console() != null
    val input = BufferedReader(InputStreamReader(System.`in`, Charsets.UTF_8))
    socket.use {
        val reader = BufferedReader(InputStreamReader(socket.getInputStream(), Charsets.UTF_8))
        val writer = PrintWriter(OutputStreamWriter(socket.getOutputStream(), Charsets.UTF_8), true)
        while (true) {
            if (tty) {
                print("> ")
                System.out.flush()
            }
            val line = input.readLine()
            if (line == null) {
                writer.println("QUIT")
                reader.readLine()
                break
            }
            val cmd = line.trim()
            if (cmd.isEmpty()) continue
            if (!tty) println("> $cmd")
            writer.println(cmd)
            if (cmd.uppercase() == "LIST") {
                while (true) {
                    val r = reader.readLine() ?: break
                    if (r == "END") break
                    println(r)
                }
            } else {
                println(reader.readLine() ?: fail("сервер закрыл соединение"))
            }
            if (cmd.uppercase() == "QUIT") break
        }
    }
}

fun toPort(text: String): Int {
    val p = text.toIntOrNull()
    if (p == null || p !in 1..65535) fail("порт должен быть числом от 1 до 65535")
    return p
}

fun main(args: Array<String>) {
    System.setOut(PrintStream(FileOutputStream(FileDescriptor.out), true, "UTF-8"))
    System.setErr(PrintStream(FileOutputStream(FileDescriptor.err), true, "UTF-8"))
    when {
        args.size == 2 && args[0] == "server" -> runServer(toPort(args[1]))
        args.size == 3 && args[0] == "client" -> runClient(args[1], toPort(args[2]))
        else -> {
            System.err.println("Использование:")
            System.err.println("  java -jar units.jar server <порт>")
            System.err.println("  java -jar units.jar client <хост> <порт>")
            exitProcess(1)
        }
    }
}
