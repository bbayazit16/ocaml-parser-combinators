open Combinators

(*
  json ::= value

  value ::= object
            | array
            | string
            | number
            | "true"
            | "false"
            | "null"

  object ::= "{" [ members ] "}"
  members ::= pair [ "," members ]
  pair ::= string ":" value

  array ::= "[" [ elements ] "]"
  elements ::= value [ "," elements ]
*)

type json =
  | JObject of (string * json) list
  | JArray of json list
  | JString of string
  | JNumber of float
  | JBool of bool
  | JNull

let rec string_of_json = function
  | JObject props ->
      let props_str =
        props
        |> List.map (fun (key, value) ->
               Printf.sprintf "\"%s\": %s" key (string_of_json value))
        |> String.concat ", "
      in
      Printf.sprintf "{ %s }" props_str
  | JArray elements ->
      let elements_str =
        elements |> List.map string_of_json |> String.concat ", "
      in
      Printf.sprintf "[ %s ]" elements_str
  | JString s -> Printf.sprintf "\"%s\"" s
  | JNumber n -> Printf.sprintf "%g" n
  | JBool b -> string_of_bool b
  | JNull -> "null"

let parse_true = map (w (tag "true")) (fun _ -> JBool true)
let parse_false = map (w (tag "false")) (fun _ -> JBool false)
let parse_null = map (w (tag "null")) (fun _ -> JNull)

let hex_digit =
  satisfy (fun c ->
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))

(* See comment in parse_exp *)
let parse_hex =
  map
    (preceded (char '0') (preceded (char 'x') (many1 hex_digit)))
    (fun hex -> string_of_int @@ int_of_string ("0x" ^ hex))

(* exp ::= ("e" | "E") [ "+" | "-" ] digits *)
let json_exp = p_or (char 'e') (char 'E')
let signs = p_or (char '+') (char '-')

let parse_exp_number_no_sign =
  map
    (pair json_exp (pair (opt signs) digits))
    (fun (_, (sign, num)) ->
      let multiplier = if sign = Some "+" then 1.0 else -1.0 in
      let exp_value = Float.pow 10.0 (float_of_string num) in
      multiplier *. exp_value)

(* Note: this is kinda inefficient, because we're converting the exponent to number, then back to string.
   I noticed my mistake afterwards and didn't want to change.
   But the fix is to change the function in parse_string to handle exponent calculation.
   Same applies to hex.
*)
let parse_exp_number =
  map
    (pair (opt (char '-')) parse_exp_number_no_sign)
    (fun (sign_opt, num) ->
      match sign_opt with
      | None -> string_of_float num
      | Some _ -> string_of_float @@ (-1.0 *. num))

(* number ::= [ "-" ] int [ frac ] [ exp ] *)
let parse_number =
  map
    (p_or (p_or (w parse_hex) (w parse_exp_number)) (w decimal))
    (fun s -> JNumber (float_of_string s))

let parse_string = map string (fun s -> JString s)

(* elements ::= value [ "," elements ] *)
let rec parse_elements str = separated_list0 (w (char ',')) parse_value str

and parse_value str =
  p_or parse_object
    (p_or parse_array
       (p_or parse_string
          (p_or parse_number (p_or parse_true (p_or parse_false parse_null)))))
    str

(* array ::= "[" [ elements ] "]" *)
and parse_array str =
  map
    (delimited (w (char '[')) parse_elements (w (char ']')))
    (fun lst -> JArray lst)
    str

(* object ::= "{" [ members ] "}" *)
and parse_object str =
  map
    (delimited (w (char '{')) (opt parse_members) (w (char '}')))
    (fun m ->
      let pairs = match m with None -> [] | Some lst -> lst in
      JObject pairs)
    str

(* members ::= pair [ "," members ] *)
and parse_members str = separated_list0 (w (char ',')) parse_pair str

(* pair ::= string ":" value *)
and parse_pair str = pair (w string) (preceded (w (char ':')) parse_value) str

(* json ::= value *)
let parse_json str = parse_value str
