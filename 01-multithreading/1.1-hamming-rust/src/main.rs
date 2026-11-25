use std::env;
use std::fs;
use std::process;
use std::thread;
use std::time::{Instant, SystemTime, UNIX_EPOCH};

fn fail(msg: &str) -> ! {
    eprintln!("Ошибка: {}", msg);
    process::exit(1);
}

fn usage() -> ! {
    eprintln!("Использование:");
    eprintln!("  hamming <файл> [-t потоки] [-o выходной_файл]");
    eprintln!("  hamming gen <количество> <длина>");
    process::exit(1);
}

fn read_words(path: &str) -> Vec<Vec<char>> {
    let text = match fs::read_to_string(path) {
        Ok(text) => text,
        Err(_) => fail(&format!("не удалось открыть файл {}", path)),
    };
    let mut words: Vec<Vec<char>> = Vec::new();
    for (i, line) in text.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let word: Vec<char> = line.chars().collect();
        if !words.is_empty() && word.len() != words[0].len() {
            fail(&format!(
                "строка {}: длина {}, ожидалось {}",
                i + 1,
                word.len(),
                words[0].len()
            ));
        }
        words.push(word);
    }
    if words.is_empty() {
        fail("в файле нет строк");
    }
    words
}

fn distance(a: &[char], b: &[char]) -> u32 {
    let mut d = 0;
    for i in 0..a.len() {
        if a[i] != b[i] {
            d += 1;
        }
    }
    d
}

fn sequential(words: &[Vec<char>]) -> Vec<u32> {
    let n = words.len();
    let mut matrix = vec![0; n * n];
    for i in 0..n {
        for j in 0..n {
            matrix[i * n + j] = distance(&words[i], &words[j]);
        }
    }
    matrix
}

fn parallel(words: &[Vec<char>], threads: usize) -> Vec<u32> {
    let n = words.len();
    let mut matrix = vec![0; n * n];
    let rows = (n + threads - 1) / threads;
    thread::scope(|s| {
        for (k, part) in matrix.chunks_mut(rows * n).enumerate() {
            s.spawn(move || {
                for (r, row) in part.chunks_mut(n).enumerate() {
                    let i = k * rows + r;
                    for j in 0..n {
                        row[j] = distance(&words[i], &words[j]);
                    }
                }
            });
        }
    });
    matrix
}

fn format_matrix(matrix: &[u32], n: usize) -> String {
    let max = matrix.iter().max().unwrap_or(&0);
    let width = max.to_string().len();
    let mut result = String::new();
    for i in 0..n {
        let row: Vec<String> = (0..n)
            .map(|j| format!("{:>width$}", matrix[i * n + j], width = width))
            .collect();
        result.push_str(&row.join(" "));
        result.push('\n');
    }
    result
}

fn parse_number(text: &str, name: &str) -> usize {
    match text.parse::<usize>() {
        Ok(n) if n > 0 => n,
        _ => fail(&format!("{} должно быть положительным целым числом", name)),
    }
}

fn generate(args: &[String]) {
    if args.len() != 3 {
        usage();
    }
    let count = parse_number(&args[1], "количество");
    let len = parse_number(&args[2], "длина");
    let mut seed = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(1)
        | 1;
    let mut text = String::with_capacity(count * (len + 1));
    for _ in 0..count {
        for _ in 0..len {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            text.push(if seed % 2 == 0 { '0' } else { '1' });
        }
        text.push('\n');
    }
    print!("{}", text);
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.is_empty() {
        usage();
    }
    if args[0] == "gen" {
        generate(&args);
        return;
    }

    let mut path: Option<String> = None;
    let mut out: Option<String> = None;
    let mut threads = thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "-t" => {
                i += 1;
                if i >= args.len() {
                    usage();
                }
                threads = parse_number(&args[i], "число потоков");
            }
            "-o" => {
                i += 1;
                if i >= args.len() {
                    usage();
                }
                out = Some(args[i].clone());
            }
            _ => path = Some(args[i].clone()),
        }
        i += 1;
    }
    let path = match path {
        Some(p) => p,
        None => usage(),
    };

    let words = read_words(&path);
    let n = words.len();
    let threads = threads.min(n);

    let start = Instant::now();
    let first = sequential(&words);
    let time1 = start.elapsed().as_secs_f64() * 1000.0;

    let start = Instant::now();
    let second = parallel(&words, threads);
    let time2 = start.elapsed().as_secs_f64() * 1000.0;

    let text = format_matrix(&second, n);
    match out {
        Some(file) => {
            if fs::write(&file, &text).is_err() {
                fail(&format!("не удалось записать файл {}", file));
            }
            println!("матрица {}x{} записана в {}", n, n, file);
        }
        None if n <= 20 => print!("{}", text),
        None => println!("матрица {}x{} слишком большая для консоли, укажите -o <файл>", n, n),
    }

    println!("строк: {}, длина: {}", n, words[0].len());
    println!("последовательно: {:.2} мс", time1);
    println!("параллельно, потоков {}: {:.2} мс", threads, time2);
    println!("ускорение: {:.2}", time1 / time2);
    println!("результаты совпадают: {}", if first == second { "да" } else { "нет" });
}
