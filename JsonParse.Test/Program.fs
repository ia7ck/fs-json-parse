open JsonParse

let test str expected =
    match parse str with
    | Ok(value) when value = expected -> Ok()
    | Ok(actual) -> Error $"Expected:\n{expected}\nActual:\n{actual}"
    | Error(message) -> Error $"Parse error: {message}"

[<EntryPoint>]
let main _ =
    [ "true", Value.True
      "false", Value.False
      "null", Value.Null
      "-1.23", Value.Number -1.23
      """ "" """, Value.String ""
      """ " , " """, Value.String " , "
      """ "abracadabra" """, Value.String "abracadabra"
      """ " \" " """, Value.String """ " """
      """ " \\ " """, Value.String """ \ """
      """ " / " """, Value.String """ / """
      """ " \/ " """, Value.String """ / """
      """ " \b " """, Value.String " \b "
      """ " \f " """, Value.String " \f "
      """ " \n " """, Value.String " \n "
      """ " \r " """, Value.String " \r "
      """ " \t " """, Value.String " \t "
      """ "line1\u000aline2" """, Value.String "line1\nline2"
      """ "あ" """, Value.String "あ"
      """ "\u3042" """, Value.String "あ"
      """ "\u3044\u2b50" """, Value.String "い⭐"
      """ "\uD83C\uDF00" """, Value.String "🌀"
      """ "\u26C4\uD83C\uDF00\u23F3" """, Value.String "⛄🌀⏳"
      "{}", Value.Object(Map [])
      """ { "k" : "v" } """, Value.Object(Map [ "k", Value.String "v" ])
      "[]", Value.Array []
      """ [ -1.23 , "abracadabra" , { "\ud83d\udd11" : null } , []] """,
      Value.Array
          [ Value.Number -1.23
            Value.String "abracadabra"
            Value.Object(Map [ "🔑", Value.Null ])
            Value.Array [] ]
      "true // comment", Value.True
      """ "comment//inside//string" """, Value.String "comment//inside//string"
      """[
        true, // c1
        // c2
        false // c3
        , // c4
        null
      ]""",
      Value.Array [ Value.True; Value.False; Value.Null ] ]
    |> List.indexed
    |> List.choose (fun (i, (input, expected)) ->
        (input, expected)
        ||> test
        |> function
            | Ok(()) -> None
            | Error(err) -> Some($"testcase #{i + 1}\n{err}"))
    |> function
        | [] -> 0
        | errors ->
            printfn "%s" (String.concat "\n\n" errors)
            1
