type 'a parser = string -> ('a * string, string) result

(*
  Namings are based on nom, since I am used to it :)
  https://docs.rs/nom/latest/nom/character/complete/fn.satisfy.html
*)
let satisfy predicate str =
  match String.length str with
  | 0 -> Error "found empty string"
  | _ ->
      let first_chr = String.get str 0 in
      if predicate first_chr then
        let chr_as_str = String.make 1 first_chr in
        let remaining = String.sub str 1 (String.length str - 1) in
        Ok (chr_as_str, remaining)
      else Error (Printf.sprintf "found: %c" first_chr)

let rec many0 parser str =
  match parser str with
  | Error _ -> Ok (String.empty, str)
  | Ok (first, rest) -> (
      match many0 parser rest with
      | Error _ -> Ok (first, rest)
      | Ok (second, second_rest) -> Ok (first ^ second, second_rest))

let rec many1 parser str =
  match parser str with
  | Error err -> Error err
  | Ok (first, rest) -> (
      match many1 parser rest with
      | Error _ -> Ok (first, rest)
      | Ok (second, second_rest) -> Ok (first ^ second, second_rest))

let opt parser =
 fun str ->
  match parser str with
  | Ok (result, rest) -> Ok (Some result, rest)
  | Error _ -> Ok (None, str)

(*  Previous dfn.
let optional parser str =
  match parser str with
  | Ok (result, rest) -> Ok (result, rest)
  | Error _ -> Ok (String.empty, str)
*)

(* https://docs.rs/nom/latest/nom/sequence/fn.pair.html *)
let pair parser1 parser2 str =
  match parser1 str with
  | Ok (result1, remaining) -> (
      match parser2 remaining with
      | Ok (result2, remaining) -> Ok ((result1, result2), remaining)
      | Error e -> Error e)
  | Error e -> Error e

let map parser f =
 fun str ->
  match parser str with
  | Ok (res, rest) -> Ok (f res, rest)
  | Error e -> Error e

(* Discards the first
  https://docs.rs/nom/latest/nom/sequence/fn.preceded.html
*)
let preceded parser1 parser2 = map (pair parser1 parser2) snd

(*
 * Discards the second, returns the first
 * https://docs.rs/nom/latest/nom/sequence/fn.terminated.html
 *)
let terminated parser1 parser2 = map (pair parser1 parser2) fst

(* `or` is invalid, `p_or` -> parser_or *)
let p_or parser1 parser2 str =
  match parser1 str with
  | Ok (result, remaining) -> Ok (result, remaining)
  | Error _ -> parser2 str

let char c = satisfy (fun chr -> c = chr)

let rec tag to_match str =
  match String.length to_match with
  | 0 -> Ok (String.empty, str)
  | to_match_len -> (
      match char (String.get to_match 0) str with
      | Error e -> Error e
      | Ok (matched_char, remaining) -> (
          let to_match_rest = String.sub to_match 1 (to_match_len - 1) in
          match tag to_match_rest remaining with
          | Error e -> Error e
          | Ok (matched_char', remaining) ->
              Ok (matched_char ^ matched_char', remaining)))

(*
  https://docs.rs/nom/latest/nom/sequence/fn.delimited.html
*)
let delimited parser_start parser_mid parser_end =
  preceded parser_start (terminated parser_mid parser_end)

let take_while1 predicate = many1 (satisfy predicate)
let take_while predicate = many0 (satisfy predicate)
let is_digit = function '0' .. '9' -> true | _ -> false
let digits = take_while1 is_digit

let decimal_no_sign =
  map
    (pair digits (opt (pair (char '.') digits)))
    (fun (int_part, frac_opt) ->
      match frac_opt with
      | None -> int_part
      | Some (dot, frac) -> int_part ^ dot ^ frac)

let decimal =
  map
    (pair (opt (char '-')) decimal_no_sign)
    (fun (sign_opt, num) ->
      match sign_opt with None -> num | Some sign -> sign ^ num)

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let characters = take_while1 is_alpha
let unescaped_char = satisfy (fun c -> c <> '"' && c <> '\\')

let escape_char =
  let escaped_value =
    p_or (tag "\"")
      (p_or (char '\\')
         (p_or (char '/')
            (p_or (char 'b')
               (p_or (char 'f') (p_or (char 'n') (p_or (char 'r') (char 't')))))))
  in
  (* '\\' is equal to single backslash => \
   * So this consumes the backslash, returns one of the escaped values
   *)
  preceded (char '\\') escaped_value

let string =
  delimited (char '"') (many0 (p_or unescaped_char escape_char)) (char '"')

(* https://docs.rs/nom/latest/nom/multi/fn.separated_list0.html *)
let separated_list0 sep parser str =
  let rec aux acc str =
    match sep str with
    | Ok (_, str') -> (
        match parser str' with
        | Ok (x, str'') -> aux (x :: acc) str''
        | Error _ -> Ok (List.rev acc, str))
    | Error _ -> Ok (List.rev acc, str)
  in
  match parser str with
  | Ok (x, str') -> aux [ x ] str'
  | Error _ -> Ok ([], str)

(* Previous definition that ignored backslashes *)
(* let string = delimited (char '"') (take_while (fun c -> c <> '"')) (char '"') *)

let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false
let whitespace = take_while is_whitespace
let w parser = preceded whitespace (terminated parser whitespace)
