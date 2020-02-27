unit module reader;
use types;

class Reader {
  has @.tokens;
  has $!position = 0;
  method peek { @.tokens[$!position] }
  method next { @.tokens[$!position++] }
}

sub read_form ($rdr) {
  given $rdr.peek {
    when "'"  { $rdr.next; MalList([MalSymbol('quote'), read_form($rdr)]) }
    when '`'  { $rdr.next; MalList([MalSymbol('quasiquote'), read_form($rdr)]) }
    when '~'  { $rdr.next; MalList([MalSymbol('unquote'), read_form($rdr)]) }
    when '~@' { $rdr.next; MalList([MalSymbol('splice-unquote'), read_form($rdr)]) }
    when '@'  { $rdr.next; MalList([MalSymbol('deref'), read_form($rdr)]) }
    when '^'  {
      $rdr.next;
      my $meta = read_form($rdr);
      MalList([MalSymbol('with-meta'), read_form($rdr), $meta]);
    }
    when ')'|']'|'}' { die X::MalUnexpected.new(token => $_) }
    when '(' { MalList(read_list($rdr, ')')) }
    when '[' { MalVector(read_list($rdr, ']')) }
    when '{' { MalHashMap(read_list($rdr, '}').map({ $^a.val => $^b }).Hash) }
    default  { read_atom($rdr) }
  }
}

sub read_list ($rdr, $end) {
  my @list;
  my $token = $rdr.next;

  loop {
    $token = $rdr.peek;
    die X::MalIncomplete.new(end => $end) if !$token.defined;
    last if $token eq $end;
    @list.push(read_form($rdr));
  }
  $rdr.next;

  return @list;
}

sub read_atom ($rdr) {
  my $atom = $rdr.next;
  given $atom {
    when /^'"' [ \\. || <-[\"\\]> ]* '"'$/ {
      s:g/^\"|\"$//;
      MalString(.trans(/\\\"/ => '"', /\\n/ => "\n", /\\\\/ => '\\'));
    }
    when /^\"/ {
      die X::MalIncomplete.new(end => '"');
    }
    when /^\:(.*)/ { MalString("\x29E$0") }
    when /^'-'? <[0..9]>+$/ { MalNumber($_) }
    when 'nil' { $NIL }
    when 'true' { $TRUE }
    when 'false' { $FALSE }
    default { MalSymbol($_) }
  }
}

my regex mal {
  [
    <[\s,]>*                          # whitespace/commas
    $<token>=(
    || '~@'                           # ~@
    || <[\[\]{}()'`~^@]>              # special single-char tokens
    || '"' [ \\. || <-[\"\\]> ]* '"'? # double-quoted strings
    || ';'<-[\n]>*                    # comments
    || <-[\s\[\]{}('"`,;)]>+          # symbols
    )
  ]+
}

sub tokenizer ($str) {
  return [] if !$str.match(/^<mal>/);
  return grep { ! /^\;/ }, $<mal><token>.map({~$_});
}

sub read_str ($str) is export {
  my @tokens = tokenizer($str);
  die X::MalNoTokens.new if !@tokens;
  return read_form(Reader.new(tokens => @tokens));
}
