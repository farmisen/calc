type token =
  | Number(int)
  | Plus
  | Minus
  | Multiply
  | Divide
  | OpenParens
  | CloseParens
  | EOF;

let eof = 26;
let zero = Char.code('0');

let toString = token =>
  switch (token) {
  | Number(value) => "Number(" ++ string_of_int(value) ++ ")"
  | Plus => "Plus"
  | Minus => "Minus"
  | Multiply => "Multiply"
  | Divide => "Divide"
  | OpenParens => "OpenParens"
  | CloseParens => "CloseParens"
  | EOF => "EOF"
  };

open Belt.Result;

let tokensToString = tokens => {
  tokens |> List.map(token => token |> toString) |> StringUtils.join(", ");
};

let tokenize = input => {
  let rec doTokenize = (current, tokens, input) => {
    switch (input) {
    | [] =>
      Ok(
        List.rev(
          switch (current) {
          | None => tokens
          | Some(token) => [token, ...tokens]
          },
        ),
      )
    | _ =>
      let (head, tail) =
        switch (input) {
        | [h, ...t] => (h, t)
        | [] => (' ', [])
        };

      let next = (current, value) => {
        doTokenize(
          value,
          switch (current) {
          | None => tokens
          | Some(token) => [token, ...tokens]
          },
          tail,
        );
      };

      switch (current, head) {
      /* State: Number */
      | (Some(Number(n)), '0'..'9' as c) =>
        next(None, Some(Number(n * 10 + Char.code(c) - zero)))

      | (_, ' ') => next(current, None)
      | (_, '0'..'9' as c) =>
        next(current, Some(Number(Char.code(c) - zero)))
      | (_, '+') => next(current, Some(Plus))
      | (_, '-') => next(current, Some(Minus))
      | (_, '*') => next(current, Some(Multiply))
      | (_, '/') => next(current, Some(Divide))
      | (_, '(') => next(current, Some(OpenParens))
      | (_, ')') => next(current, Some(CloseParens))

      | (_, c) => Error("unexpected character " ++ Char.escaped(c))
      };
    };
  };

  Exploder.explode(input) |> doTokenize(None, []);
};
