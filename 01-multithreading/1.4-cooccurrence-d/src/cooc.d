import std.stdio;
import std.file : readText, writeFile = write, FileException;
import std.utf : UTFException;
import std.uni : isAlpha, toLower;
import std.algorithm : sort, uniq, map, min, max, swap;
import std.array : array, appender, replicate;
import std.conv : to, ConvException;
import std.range : iota, walkLength;
import std.parallelism : TaskPool, totalCPUs;
import std.datetime.stopwatch : StopWatch, AutoStart;
import std.random : uniform;
import std.format : format;
import core.stdc.stdlib : exit;

void fail(string msg)
{
    stderr.writeln("Ошибка: ", msg);
    exit(1);
}

void usage()
{
    stderr.writeln("Использование:");
    stderr.writeln("  cooc <файл> [-k окно] [-t потоки] [-top N] [-o выходной_файл]");
    stderr.writeln("  cooc gen <количество_слов>");
    exit(1);
}

size_t number(string s, string msg)
{
    size_t n;
    try
        n = s.to!size_t;
    catch (ConvException)
        fail(msg);
    if (n == 0)
        fail(msg);
    return n;
}

string next(string[] args, ref size_t i)
{
    if (i + 1 >= args.length)
        usage();
    i++;
    return args[i];
}

string[] splitWords(string text)
{
    string[] words;
    dchar[] cur;
    foreach (dchar c; text)
    {
        if (isAlpha(c))
            cur ~= toLower(c);
        else if (cur.length > 0)
        {
            words ~= cur.to!string;
            cur.length = 0;
            cur.assumeSafeAppend();
        }
    }
    if (cur.length > 0)
        words ~= cur.to!string;
    return words;
}

uint[ulong] countPart(const(int)[] ids, size_t first, size_t last, size_t k, size_t v)
{
    uint[ulong] result;
    foreach (i; first .. last)
    {
        foreach (j; i + 1 .. min(i + k + 1, ids.length))
        {
            ulong a = ids[i];
            ulong b = ids[j];
            if (a > b)
                swap(a, b);
            result[a * v + b]++;
        }
    }
    return result;
}

uint[ulong] countParallel(TaskPool pool, const(int)[] ids, size_t k, size_t v, size_t t)
{
    auto parts = new uint[ulong][](t);
    foreach (p; pool.parallel(iota(t), 1))
    {
        size_t first = ids.length * p / t;
        size_t last = ids.length * (p + 1) / t;
        parts[p] = countPart(ids, first, last, k, v);
    }
    uint[ulong] total;
    foreach (part; parts)
        foreach (key, c; part)
            total[key] += c;
    return total;
}

string pad(string s, size_t w, bool right = false)
{
    auto n = s.walkLength;
    if (n >= w)
        return s;
    auto space = replicate(" ", w - n);
    return right ? space ~ s : s ~ space;
}

string table(string[] vocab, uint[ulong] total)
{
    size_t v = vocab.length;
    auto m = new uint[][](v, v);
    uint top = 0;
    foreach (key, c; total)
    {
        size_t a = cast(size_t)(key / v);
        size_t b = cast(size_t)(key % v);
        m[a][b] = c;
        m[b][a] = c;
        top = max(top, c);
    }
    size_t label = 0;
    foreach (w; vocab)
        label = max(label, w.walkLength);
    size_t cell = max(label, top.to!string.length);

    auto buf = appender!string;
    buf.put(replicate(" ", label));
    foreach (w; vocab)
        buf.put(" " ~ pad(w, cell, true));
    buf.put("\n");
    foreach (a; 0 .. v)
    {
        buf.put(pad(vocab[a], label));
        foreach (b; 0 .. v)
            buf.put(" " ~ pad(m[a][b].to!string, cell, true));
        buf.put("\n");
    }
    return buf.data;
}

struct Pair
{
    string a;
    string b;
    uint c;
}

void generate(string[] args)
{
    if (args.length != 3)
        usage();
    auto n = number(args[2], "количество слов должно быть положительным целым числом");
    immutable string[] dict = [
        "кот", "пёс", "дом", "лес", "река", "солнце", "небо", "город", "дорога",
        "книга", "окно", "стол", "школа", "друг", "мама", "папа", "утро", "вечер",
        "зима", "лето", "море", "поле", "гора", "сад", "мост", "ветер", "дождь",
        "снег", "звезда", "луна", "птица", "рыба", "хлеб", "вода", "чай", "огонь",
        "путь", "сказка", "песня", "время"
    ];
    auto buf = appender!string;
    foreach (i; 0 .. n)
    {
        auto x = min(uniform(0, dict.length), uniform(0, dict.length));
        buf.put(dict[x]);
        buf.put((i + 1) % 15 == 0 ? "\n" : " ");
    }
    write(buf.data);
}

void main(string[] args)
{
    if (args.length < 2)
        usage();
    if (args[1] == "gen")
    {
        generate(args);
        return;
    }

    string path;
    string outPath;
    size_t k = 2;
    size_t t = totalCPUs;
    size_t top = 10;
    for (size_t i = 1; i < args.length; i++)
    {
        switch (args[i])
        {
        case "-k":
            k = number(next(args, i), "размер окна должен быть положительным целым числом");
            break;
        case "-t":
            t = number(next(args, i), "число потоков должно быть положительным целым числом");
            break;
        case "-top":
            top = number(next(args, i), "число пар должно быть положительным целым числом");
            break;
        case "-o":
            outPath = next(args, i);
            break;
        default:
            path = args[i];
        }
    }
    if (path.length == 0)
        usage();

    string text;
    try
        text = readText(path);
    catch (FileException)
        fail("не удалось открыть файл " ~ path);
    catch (UTFException)
        fail("файл " ~ path ~ " не в кодировке UTF-8");

    auto words = splitWords(text);
    if (words.length == 0)
        fail("в тексте нет слов");

    string[] vocab = words.dup.sort.uniq.array;
    int[string] index;
    foreach (i, w; vocab)
        index[w] = cast(int) i;
    auto ids = words.map!(w => index[w]).array;
    size_t v = vocab.length;

    auto pool = new TaskPool(t - 1);
    scope (exit)
        pool.finish(true);

    auto sw = StopWatch(AutoStart.yes);
    auto first = countPart(ids, 0, ids.length, k, v);
    auto time1 = sw.peek.total!"usecs" / 1000.0;

    sw.reset();
    auto second = countParallel(pool, ids, k, v, t);
    auto time2 = sw.peek.total!"usecs" / 1000.0;

    if (outPath.length > 0)
    {
        if (v > 2000)
            fail(format("словарь слишком большой для матрицы: %s слов", v));
        try
            writeFile(outPath, table(vocab, second));
        catch (FileException)
            fail("не удалось записать файл " ~ outPath);
    }

    writeln(format("слов: %s, различных: %s, окно: %s", words.length, v, k));
    if (outPath.length > 0)
        writeln("матрица ", v, "x", v, " записана в ", outPath);
    else if (v <= 12)
        write(table(vocab, second));
    else
        writeln("матрица ", v, "x", v, " не выводится в консоль, укажите -o <файл>");

    Pair[] pairs;
    foreach (key, c; second)
        pairs ~= Pair(vocab[cast(size_t)(key / v)], vocab[cast(size_t)(key % v)], c);
    pairs.sort!((x, y) => x.c != y.c ? x.c > y.c : (x.a != y.a ? x.a < y.a : x.b < y.b));
    writeln("самые частые пары:");
    foreach (p; pairs[0 .. min(top, pairs.length)])
        writeln(format("  (%s, %s) = %s", p.a, p.b, p.c));

    writeln(format("последовательно: %.2f мс", time1));
    writeln(format("параллельно, потоков %s: %.2f мс", t, time2));
    writeln(format("ускорение: %.2f", time1 / time2));
    writeln("результаты совпадают: ", first == second ? "да" : "нет");
}
