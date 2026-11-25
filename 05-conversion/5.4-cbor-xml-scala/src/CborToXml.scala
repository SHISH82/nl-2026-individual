import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.Base64

enum Value:
  case IntV(n: BigInt)
  case FloatV(text: String)
  case TextV(s: String)
  case BytesV(b: Array[Byte])
  case ArrayV(items: List[Value], indefinite: Boolean)
  case MapV(entries: List[(Value, Value)], indefinite: Boolean)
  case BoolV(b: Boolean)
  case NullV
  case UndefinedV
  case SimpleV(n: Int)
  case TagV(n: BigInt, v: Value)

import Value.*

class ParseError(msg: String) extends Exception(msg)

class Parser(s: String):
  var pos = 0

  def error(msg: String): Nothing =
    var p = math.min(pos, s.length)
    if p == s.length then
      while p > 0 && s(p - 1).isWhitespace do p -= 1
    val before = s.substring(0, p)
    val line = before.count(_ == '\n') + 1
    val col = before.length - before.lastIndexOf('\n')
    throw ParseError(s"строка $line, позиция $col: $msg")

  def peek: Char = if pos < s.length then s(pos) else '\u0000'

  def skip(): Unit =
    var again = true
    while again do
      while pos < s.length && s(pos).isWhitespace do pos += 1
      if peek == '/' then
        val end = s.indexOf('/', pos + 1)
        if end < 0 then error("незакрытый комментарий")
        pos = end + 1
      else again = false

  def expect(c: Char): Unit =
    skip()
    if peek != c then error(s"ожидалось '$c'")
    pos += 1

  def parseAll(): List[Value] =
    skip()
    if pos >= s.length then error("пустой документ")
    val items = scala.collection.mutable.ListBuffer(value())
    skip()
    while pos < s.length do
      if peek == ',' then pos += 1
      items += value()
      skip()
    items.toList

  def value(): Value =
    skip()
    peek match
      case '[' => array()
      case '{' => map()
      case '"' => TextV(text())
      case 'h' if s.startsWith("h'", pos) => hexBytes()
      case 'b' if s.startsWith("b64'", pos) => base64Bytes()
      case c if c.isDigit || c == '-' => numberOrTag()
      case '\u0000' => error("неожиданный конец документа")
      case _ => word()

  def array(): Value =
    pos += 1
    skip()
    val indefinite = peek == '_'
    if indefinite then pos += 1
    skip()
    val items = scala.collection.mutable.ListBuffer[Value]()
    if peek == ']' then pos += 1
    else
      items += value()
      skip()
      while peek == ',' do
        pos += 1
        items += value()
        skip()
      expect(']')
    ArrayV(items.toList, indefinite)

  def map(): Value =
    pos += 1
    skip()
    val indefinite = peek == '_'
    if indefinite then pos += 1
    skip()
    val entries = scala.collection.mutable.ListBuffer[(Value, Value)]()
    def entry(): Unit =
      val k = value()
      expect(':')
      entries += ((k, value()))
      skip()
    if peek == '}' then pos += 1
    else
      entry()
      while peek == ',' do
        pos += 1
        entry()
      expect('}')
    MapV(entries.toList, indefinite)

  def text(): String =
    val sb = StringBuilder()
    pos += 1
    while peek != '"' do
      if pos >= s.length then error("незакрытая строка")
      val c = s(pos)
      pos += 1
      if c == '\\' then
        if pos >= s.length then error("незакрытая строка")
        val e = s(pos)
        pos += 1
        e match
          case '"' => sb += '"'
          case '\\' => sb += '\\'
          case '/' => sb += '/'
          case 'n' => sb += '\n'
          case 't' => sb += '\t'
          case 'r' => sb += '\r'
          case 'b' => sb += '\b'
          case 'f' => sb += '\f'
          case 'u' =>
            if pos + 4 > s.length then error("неверная последовательность \\u")
            val code = s.substring(pos, pos + 4)
            if !code.forall(ch => Character.digit(ch, 16) >= 0) then error("неверная последовательность \\u")
            sb += Integer.parseInt(code, 16).toChar
            pos += 4
          case other => error(s"неверная escape-последовательность \\$other")
      else sb += c
    pos += 1
    sb.toString

  def quoted(start: Int): String =
    pos = start
    val end = s.indexOf('\'', pos)
    if end < 0 then error("незакрытая байтовая строка")
    val body = s.substring(pos, end)
    pos = end + 1
    body

  def hexBytes(): Value =
    val at = pos
    val body = quoted(pos + 2).filterNot(_.isWhitespace)
    if body.length % 2 != 0 || !body.forall(c => Character.digit(c, 16) >= 0) then
      pos = at
      error("некорректные hex-данные")
    BytesV(body.grouped(2).map(h => Integer.parseInt(h, 16).toByte).toArray)

  def base64Bytes(): Value =
    val at = pos
    val body = quoted(pos + 4).filterNot(_.isWhitespace)
    val padded = body + "=" * ((4 - body.length % 4) % 4)
    try
      val decoder = if body.exists(c => c == '-' || c == '_') then Base64.getUrlDecoder else Base64.getDecoder
      BytesV(decoder.decode(padded))
    catch
      case _: IllegalArgumentException =>
        pos = at
        error("некорректные base64-данные")

  def numberOrTag(): Value =
    if s.startsWith("-Infinity", pos) then
      pos += 9
      return FloatV("-Infinity")
    val start = pos
    if peek == '-' then pos += 1
    while pos < s.length && "0123456789.eE+-".contains(s(pos)) do pos += 1
    val token = s.substring(start, pos)
    if token.exists(c => c == '.' || c == 'e' || c == 'E') then
      if token.toDoubleOption.isEmpty then
        pos = start
        error(s"неверное число '$token'")
      FloatV(token)
    else
      val n = try BigInt(token)
      catch
        case _: NumberFormatException =>
          pos = start
          error(s"неверное число '$token'")
      skip()
      if peek == '(' then
        if n < 0 then error("номер тега не может быть отрицательным")
        pos += 1
        val inner = value()
        expect(')')
        TagV(n, inner)
      else IntV(n)

  def word(): Value =
    val start = pos
    while pos < s.length && s(pos).isLetter do pos += 1
    s.substring(start, pos) match
      case "true" => BoolV(true)
      case "false" => BoolV(false)
      case "null" => NullV
      case "undefined" => UndefinedV
      case "NaN" => FloatV("NaN")
      case "Infinity" => FloatV("Infinity")
      case "simple" =>
        expect('(')
        skip()
        val digits = pos
        while pos < s.length && s(pos).isDigit do pos += 1
        val n = s.substring(digits, pos).toIntOption.getOrElse(-1)
        if n < 0 || n > 255 then error("значение simple должно быть от 0 до 255")
        expect(')')
        SimpleV(n)
      case "" =>
        error(s"неожиданный символ '${peek}'")
      case other =>
        pos = start
        error(s"неизвестное слово '$other'")

def esc(s: String): String =
  s.flatMap {
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '&' => "&amp;"
    case '"' => "&quot;"
    case '\'' => "&apos;"
    case c => c.toString
  }

def hex(b: Array[Byte]): String = b.map(x => f"${x & 0xff}%02x").mkString

def scalar(v: Value): Option[String] = v match
  case IntV(n) => Some(s"<int>$n</int>")
  case FloatV(t) => Some(s"<float>$t</float>")
  case TextV(s) => Some(s"<text>${esc(s)}</text>")
  case BytesV(b) => Some(s"""<bytes encoding="hex">${hex(b)}</bytes>""")
  case BoolV(b) => Some(s"<bool>$b</bool>")
  case NullV => Some("<null/>")
  case UndefinedV => Some("<undefined/>")
  case SimpleV(n) => Some(s"""<simple value="$n"/>""")
  case ArrayV(Nil, false) => Some("<array/>")
  case MapV(Nil, false) => Some("<map/>")
  case TagV(n, x) => scalar(x).map(inner => s"""<tag number="$n">$inner</tag>""")
  case _ => None

def xml(v: Value, depth: Int, out: StringBuilder): Unit =
  val pad = "  " * depth
  scalar(v) match
    case Some(line) => out ++= pad ++= line ++= "\n"
    case None =>
      v match
        case ArrayV(items, indefinite) =>
          out ++= pad ++= (if indefinite then """<array indefinite="true">""" else "<array>") ++= "\n"
          items.foreach(xml(_, depth + 1, out))
          out ++= pad ++= "</array>\n"
        case MapV(entries, indefinite) =>
          out ++= pad ++= (if indefinite then """<map indefinite="true">""" else "<map>") ++= "\n"
          entries.foreach((k, x) => entry(k, x, depth + 1, out))
          out ++= pad ++= "</map>\n"
        case TagV(n, x) =>
          out ++= pad ++= s"""<tag number="$n">""" ++= "\n"
          xml(x, depth + 1, out)
          out ++= pad ++= "</tag>\n"
        case _ =>

def entry(k: Value, v: Value, depth: Int, out: StringBuilder): Unit =
  val pad = "  " * depth
  (scalar(k), scalar(v)) match
    case (Some(a), Some(b)) =>
      out ++= pad ++= s"<entry><key>$a</key><value>$b</value></entry>\n"
    case _ =>
      out ++= pad ++= "<entry>\n"
      for (name, x) <- List(("key", k), ("value", v)) do
        scalar(x) match
          case Some(line) => out ++= pad ++= s"  <$name>$line</$name>\n"
          case None =>
            out ++= pad ++= s"  <$name>\n"
            xml(x, depth + 2, out)
            out ++= pad ++= s"  </$name>\n"
      out ++= pad ++= "</entry>\n"

def fail(msg: String): Nothing =
  System.err.println("Ошибка: " + msg)
  sys.exit(1)

@main def run(args: String*): Unit =
  System.setOut(java.io.PrintStream(java.io.FileOutputStream(java.io.FileDescriptor.out), true, "UTF-8"))
  System.setErr(java.io.PrintStream(java.io.FileOutputStream(java.io.FileDescriptor.err), true, "UTF-8"))
  val (files, out) = args.toList match
    case List(in) => (in, None)
    case List(in, "-o", o) => (in, Some(o))
    case _ =>
      System.err.println("Использование: java -jar cbor2xml.jar <файл.diag> [-o выходной_файл.xml]")
      sys.exit(1)
  val text =
    try String(Files.readAllBytes(Paths.get(files)), StandardCharsets.UTF_8)
    catch case _: Exception => fail(s"не удалось открыть файл $files")
  val items =
    try Parser(text).parseAll()
    catch case e: ParseError => fail(e.getMessage)
  val sb = StringBuilder()
  sb ++= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<cbor>\n"
  items.foreach(xml(_, 1, sb))
  sb ++= "</cbor>\n"
  out match
    case Some(o) =>
      try Files.write(Paths.get(o), sb.toString.getBytes(StandardCharsets.UTF_8))
      catch case _: Exception => fail(s"не удалось записать файл $o")
      println(s"XML записан в $o, значений: ${items.length}")
    case None => print(sb.toString)
