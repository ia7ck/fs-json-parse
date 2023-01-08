module JsonParse

open System
open FParsec


[<RequireQualifiedAccessAttribute>]
type Value =
    | True
    | False
    | Null
    | Object of Map<string, Value>
    | Array of Value list
    | Number of float
    | String of string

let private skipComment = pstring "//" >>. skipRestOfLine false
let private skip = skipMany (spaces1 <|> skipComment)

let private pEscape =
    choice
        [ stringReturn "\\\"" "\"" // \" -> "
          stringReturn """\\""" """\"""
          stringReturn """\/""" "/"
          stringReturn """\b""" "\b"
          stringReturn """\f""" "\f"
          stringReturn """\n""" "\n"
          stringReturn """\r""" "\r"
          stringReturn """\t""" "\t" ]

let private pUnicodeEscape =
    pstring """\u""" >>. tuple4 hex hex hex hex
    |>> (fun (h3, h2, h1, h0) -> Convert.ToUInt16($"{h3}{h2}{h1}{h0}", 16) |> char |> string)

let private pStringLiteral =
    (pchar '"')
    >>. manyStrings ((noneOf [ '"'; '\\' ] |>> string) <|> pEscape <|> pUnicodeEscape)
    .>> (pchar '"')

let private pJsonValue, pJsonValueRef = createParserForwardedToRef<Value, unit> ()

let private pJsonTrue = stringReturn "true" Value.True
let private pJsonFalse = stringReturn "false" Value.False
let private pJsonNull = stringReturn "null" Value.Null

let private pKeyValue =
    pStringLiteral .>>. (skip >>. (pchar ':') >>. skip >>. pJsonValue)

let private pJsonObject =
    (pchar '{') >>. sepBy (skip >>. pKeyValue .>> skip) (pchar ',') .>> (pchar '}')
    |>> Map.ofList
    |>> Value.Object

let private pJsonArray =
    (pchar '[') >>. sepBy (skip >>. pJsonValue .>> skip) (pchar ',') .>> (pchar ']')
    |>> Value.Array

let private pJsonNumber = pfloat |>> Value.Number
let private pJsonString = pStringLiteral |>> Value.String

pJsonValueRef.Value <-
    choice
        [ pJsonTrue
          pJsonFalse
          pJsonObject
          pJsonNull
          pJsonArray
          pJsonNumber
          pJsonString ]

let parse str =
    match run (skip >>. pJsonValue .>> skip .>> eof) str with
    | Success(value, _, _) -> Result.Ok value
    | Failure(message, _, _) -> Result.Error message
