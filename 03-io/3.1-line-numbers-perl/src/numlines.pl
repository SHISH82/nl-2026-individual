use strict;
use warnings;
use utf8;
use open qw(:std :encoding(UTF-8));
use Encode qw(decode);

sub fail {
    print STDERR "Ошибка: $_[0]\n";
    exit 1;
}

sub usage {
    print STDERR "Использование: perl numlines.pl [-s начало] [-i шаг] [-w ширина] [-d разделитель] [-b] [-o выходной_файл] [файл]\n";
    exit 1;
}

my @args = map { decode('UTF-8', $_) } @ARGV;
my ($start, $step, $width, $sep, $blank, $out) = (1, 1, 6, "\t", 0, '');
my @files;

while (@args) {
    my $arg = shift @args;
    if ($arg eq '-b') {
        $blank = 1;
    }
    elsif ($arg =~ /^-([siwdo])$/) {
        my $key = $1;
        fail("после $arg нужно значение") unless @args;
        my $value = shift @args;
        $start = $value if $key eq 's';
        $step  = $value if $key eq 'i';
        $width = $value if $key eq 'w';
        $sep   = $value if $key eq 'd';
        $out   = $value if $key eq 'o';
    }
    elsif ($arg =~ /^-./) {
        fail("неизвестный параметр $arg");
    }
    else {
        push @files, $arg;
    }
}

usage() if @files > 1;
fail('начальный номер должен быть целым числом') unless $start =~ /^-?\d+$/;
fail('шаг должен быть ненулевым целым числом') unless $step =~ /^-?\d+$/ && $step != 0;
fail('ширина должна быть целым числом от 1 до 20') unless $width =~ /^\d+$/ && $width >= 1 && $width <= 20;

my $in;
if (@files) {
    open($in, '<', $files[0]) or fail("не удалось открыть файл $files[0]");
}
else {
    $in = \*STDIN;
}

my $fh = \*STDOUT;
if ($out ne '') {
    fail('выходной файл совпадает с входным') if @files && $out eq $files[0];
    open($fh, '>', $out) or fail("не удалось записать файл $out");
}

my $n = $start;
my ($total, $numbered) = (0, 0);
while (my $line = <$in>) {
    chomp $line;
    $line =~ s/\r$//;
    $total++;
    if ($blank && $line =~ /^\s*$/) {
        my $pad = (' ' x $width) . $sep;
        $pad =~ s/\s+$//;
        print $fh "$pad\n";
    }
    else {
        printf $fh "%*d%s%s\n", $width, $n, $sep, $line;
        $n += $step;
        $numbered++;
    }
}

close($fh) if $out ne '';
print STDERR "строк: $total, пронумеровано: $numbered\n";
