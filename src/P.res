type parser<'a> = Parser(string => result<('a, string), string>)

let reduce = (list, fn) => {
  let rest = Array.sliceToEnd(list, ~start=1)

  let first = list[0]->Option.getExn

  Array.reduce(rest, first, fn)
}

let run = (parser, input) => {
  let Parser(inner) = parser
  inner(input)
}

let label = (parser, label) => Parser(
  input => run(parser, input)->Result.mapError(_ => "Expected " ++ label),
)

let char = v => Parser(
  string =>
    switch String.get(string, 0) {
    | None => Error("Unexpected end of input")
    | Some(ch) if v == ch => Ok((v, String.sliceToEnd(string, ~start=1)))
    | _ => Error("Expected " ++ v)
    },
)

let andThen = (parser1, parser2) => Parser(
  input => {
    switch run(parser1, input) {
    | Ok((value1, rest)) =>
      switch run(parser2, rest) {
      | Ok((value2, rest)) => Ok(((value1, value2), rest))
      | Error(_) as error => error
      }
    | Error(_) as error => error
    }
  },
)

let orElse = (parser1, parser2) => Parser(
  input => {
    switch run(parser1, input) {
    | Ok(_) as result => result
    | Error(_) => run(parser2, input)
    }
  },
)

let map = (parser, fn) => Parser(
  input => {
    switch run(parser, input) {
    | Ok((value, rest)) => Ok((fn(value), rest))
    | Error(_) as error => error
    }
  },
)

let choice = parsers => reduce(parsers, orElse)

let anyOf = chars => chars->Array.map(char)->choice

let sequence = parsers => {
  let concatResults = (p1, p2) => andThen(p1, p2)->map(((a, b)) => List.concat(a, b))

  parsers->Array.map(p => p->map(x => list{x}))->reduce(concatResults)
}

let many = parser => {
  let rec inner = input => {
    switch parser->run(input) {
    | Error(_) => (list{}, input)
    | Ok((value, rest)) =>
      let (values, rest) = inner(rest)
      (list{value, ...values}, rest)
    }
  }

  Parser(input => Ok(inner(input)))
}

let many1 = parser =>
  parser->map(v => list{v})->andThen(many(parser))->map(((a, b)) => List.concat(a, b))

let opt = parser => {
  let rec inner = input => {
    switch run(parser, input) {
    | Ok((value, rest)) =>
      switch inner(rest) {
      | Ok((None, _)) | Error(_) => Ok(Some(value), rest)
      | Ok((Some(_), _)) => Error("Expected value")
      }
    | Error(_) => Ok(None, input)
    }
  }

  Parser(inner)
}

let keepLeft = (parser1, parser2) => andThen(parser1, parser2)->map(((a, _)) => a)

let keepRight = (parser1, parser2) => andThen(parser1, parser2)->map(((_, b)) => b)

let between = (parser1, parser2, parser3) => keepLeft(keepRight(parser1, parser2), parser3)

let sepBy1 = (parser, sep) => Parser(
  input =>
    switch run(parser, input) {
    | Ok((value, rest)) => many(keepRight(sep, parser))->map(List.concat(list{value}, _))->run(rest)
    | Error(_) as error => error
    },
)

let sepBy = (parser, sep) => sepBy1(opt(parser), sep)->map(List.filterMap(_, x => x))

let satisfy = fn => Parser(
  input => {
    switch String.get(input, 0) {
    | None => Error("Unexpected end of input")
    | Some(ch) =>
      if fn(ch) {
        Ok((ch, String.sliceToEnd(input, ~start=1)))
      } else {
        Error("Unexpected character " ++ ch)
      }
    }
  },
)

let string = str =>
  String.split(str, "")
  ->Array.map(char)
  ->sequence
  ->map(v => List.toArray(v))
  ->map(v => Array.join(v, ""))

let makeRecursive = fn => {
  let parserRef = ref(Parser(_ => failwith("Not implemented")))

  parserRef := fn(Parser(input => run(parserRef.contents, input)))

  parserRef.contents
}
