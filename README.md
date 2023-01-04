# fs-json-parse

FParsec tutorial http://www.quanttec.com/fparsec/tutorial.html#parsing-json をもとにした JSON パーサーです。

# Example

```
$ dotnet fsi
> #r "nuget: FParsec";;
> open FParsec;;
> #load "JsonParse/Library.fs";;
> open JsonParse;;
> parse "[true, false, null, \"abracadabra\", {\"key\": [\"nested\", {}]}]";;
val it: Result<Value,string> =
  Ok
    (Array
       [True; False; Null; String "abracadabra";
        Object (map [("key", Array [String "nested"; Object (map [])])])])
```

# Test

```
$ dotnet run --project JsonParse.Test
```
