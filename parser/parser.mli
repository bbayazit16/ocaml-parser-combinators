type json =
  | JObject of (string * json) list
  | JArray of json list
  | JString of string
  | JNumber of float
  | JBool of bool
  | JNull

val parse_json : string -> (json * string, string) result
val string_of_json : json -> string
