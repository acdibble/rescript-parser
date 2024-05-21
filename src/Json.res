type rec json =
  | Null
  | Bool(bool)
  | String(string)
  | Number(float)
  | Object(Map.t<string, json>)
  | Array(array<json>)

let jsonNull = P.string("null")->P.map(_ => Null)
let jsonTrue = P.string("true")->P.map(_ => Bool(true))
let jsonFalse = P.string("false")->P.map(_ => Bool(false))

let unescapedChar = P.satisfy(ch => ch != "\\" && ch != "\"")
let escapedChar =
  P.char("\\")
  ->P.andThen(
    P.anyOf(["\"", "\\", "/", "b", "f", "n", "r", "t"])->P.map(result =>
      switch result {
      | "\"" => "\""
      | "\\" => "\\"
      | "/" => "/"
      | "b" => "\b"
      | "f" => "\f"
      | "n" => "\n"
      | "r" => "\r"
      | "t" => "\t"
      | _ => panic("unreachable")
      }
    ),
  )
  ->P.map(((_, b)) => b)

let digit = "0123456789"->String.split("")->P.anyOf->P.label("digit")

let unicodeChar =
  P.string("\\u")
  ->P.andThen(P.sequence([digit, digit, digit, digit]))
  ->P.map(((_, chars)) => {
    let code = chars->List.toArray->Array.join("")->Int.fromString(~radix=16)->Option.getExn
    String.fromCharCode(code)
  })

let unquotedString =
  P.choice([unescapedChar, escapedChar, unicodeChar])
  ->P.many
  ->P.map(chars => chars->List.toArray->Array.join(""))

let quote = P.char("\"")

let jsonString = P.between(quote, unquotedString, quote)->P.map(v => String(v))

let signPart = P.char("-")

let zero = P.char("0")
let oneThruNine = "123456789"->String.split("")->P.anyOf

let zeroOrMoreDigits = P.many(digit)

let integerPart = P.choice([
  zero,
  oneThruNine
  ->P.andThen(zeroOrMoreDigits)
  ->P.map(((first, rest)) => first ++ rest->List.toArray->Array.join("")),
])

let fractionalPart =
  P.char(".")
  ->P.andThen(zeroOrMoreDigits)
  ->P.map(((_, rest)) => "." ++ rest->List.toArray->Array.join(""))

let jsonOpt = p => P.opt(p)->P.map(v => Option.getOr(v, ""))

let exponentPart =
  P.anyOf(["e", "E"])
  ->P.andThen(jsonOpt(P.anyOf(["+", "-"])))
  ->P.andThen(zeroOrMoreDigits)
  ->P.map((((e, sign), rest)) => e ++ sign ++ rest->List.toArray->Array.join(""))

let jsonNumber =
  P.sequence([
    jsonOpt(signPart),
    integerPart,
    jsonOpt(fractionalPart),
    jsonOpt(exponentPart),
  ])->P.map(v => v->List.toArray->Array.join("")->Float.fromString->Option.getExn->Number)

let whitespace = P.many(P.anyOf([" ", "\t", "\n", "\r"]))

let betweenWhitespace = P.between(whitespace, _, whitespace)

let jsonValue = P.makeRecursive(p => {
  let jsonObject = P.between(
    P.char("{"),
    P.sepBy(
      P.sequence([jsonString->betweenWhitespace, P.char(":")->P.map(_ => Null), p])->P.map(v =>
        switch v {
        | list{key, _, value} => (key, value)
        | _ => panic("unreachable")
        }
      ),
      betweenWhitespace(P.char(",")),
    ),
    P.char("}"),
  )->P.map(v => {
    Object(
      List.reduce(
        v,
        Map.make(),
        (acc, (key, value)) => {
          switch key {
          | String(key) => Map.set(acc, key, value)
          | _ => panic("unreachable")
          }
          acc
        },
      ),
    )
  })

  let jsonArray = P.between(
    P.char("["),
    P.sepBy(p, betweenWhitespace(P.char(","))),
    P.char("]"),
  )->P.map(v => {
    Array(v->List.toArray)
  })

  P.choice([
    jsonNull,
    jsonTrue,
    jsonFalse,
    jsonString,
    jsonNumber,
    jsonObject,
    jsonArray,
  ])->betweenWhitespace
})

let rec toString = json =>
  switch json {
  | Null => "null"
  | Bool(true) => "true"
  | Bool(false) => "false"
  | String(value) => "\"" ++ value ++ "\""
  | Number(value) => Float.toString(value)
  | Object(map) =>
    "{" ++
    Map.entries(map)
    ->Array.fromIterator
    ->Array.map(((key, value)) => String(key)->toString ++ ": " ++ toString(value))
    ->Array.join(", ") ++ "}"
  | Array(arr) => "[" ++ Array.map(arr, toString)->Array.join(", ") ++ "]"
  }
