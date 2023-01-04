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

module private Parser =
    let skipComment = pstring "//" >>. skipRestOfLine false
    let skip = skipMany (spaces1 <|> skipComment)

    let pEscape =
        choice
            [ stringReturn "\\\"" "\"" // \" -> "
              stringReturn """\\""" """\"""
              stringReturn """\/""" "/"
              stringReturn """\b""" "\b"
              stringReturn """\f""" "\f"
              stringReturn """\n""" "\n"
              stringReturn """\r""" "\r"
              stringReturn """\t""" "\t" ]

    let pUnicodeEscape =
        pstring """\u""" >>. tuple4 hex hex hex hex
        |>> (fun (h3, h2, h1, h0) -> Convert.ToUInt16($"{h3}{h2}{h1}{h0}", 16) |> char |> string)

    let pStringLiteral =
        (pchar '"')
        >>. manyStrings ((noneOf [ '"'; '\\' ] |>> string) <|> pEscape <|> pUnicodeEscape)
        .>> (pchar '"')

    let pJsonValue, pJsonValueRef = createParserForwardedToRef<Value, unit> ()

    let pJsonTrue = stringReturn "true" Value.True
    let pJsonFalse = stringReturn "false" Value.False
    let pJsonNull = stringReturn "null" Value.Null

    let pKeyValue = pStringLiteral .>>. (skip >>. (pchar ':') >>. skip >>. pJsonValue)

    let pJsonObject =
        (pchar '{') >>. sepBy (skip >>. pKeyValue .>> skip) (pchar ',') .>> (pchar '}')
        |>> Map.ofList
        |>> Value.Object

    let pJsonArray =
        (pchar '[') >>. sepBy (skip >>. pJsonValue .>> skip) (pchar ',') .>> (pchar ']')
        |>> Value.Array

    let pJsonNumber = pfloat |>> Value.Number
    let pJsonString = pStringLiteral |>> Value.String

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
    match run (Parser.skip >>. Parser.pJsonValue .>> Parser.skip .>> eof) str with
    | Success(value, _, _) -> Result.Ok value
    | Failure(message, _, _) -> Result.Error message
