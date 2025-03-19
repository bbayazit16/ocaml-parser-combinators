type 'a parser = string -> ('a * string, string) result

val satisfy : (char -> bool) -> string parser
val many0 : string parser -> string parser
val many1 : string parser -> string parser
val opt : 'a parser -> 'a option parser
val pair : 'a parser -> 'b parser -> ('a * 'b) parser
val preceded : 'a parser -> 'b parser -> 'b parser
val terminated : 'a parser -> 'b parser -> 'a parser
val p_or : 'a parser -> 'a parser -> 'a parser
val char : char -> string parser
val tag : string -> string parser
val map : 'a parser -> ('a -> 'b) -> 'b parser
val delimited : 'a parser -> 'b parser -> 'c parser -> 'b parser
val take_while1 : (char -> bool) -> string parser
val take_while : (char -> bool) -> string parser
val separated_list0 : 'a parser -> 'b parser -> 'b list parser
val digits : string parser
val decimal_no_sign : string parser
val decimal : string parser
val characters : string parser
val string : string parser
val whitespace : string parser
val is_alpha : char -> bool

(* Make a parser ignore the trailing and leading whitespaces *)
val w : 'a parser -> 'a parser
