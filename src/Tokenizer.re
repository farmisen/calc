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

let tokenize = input => {
  let rec doTokenize = (input, current, tokens) => {
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
      let next = doTokenize(tail);
      switch (current, head, tokens) {
      /* State: None */
      | (None, ' ', t) => next(None, t)
      | (None, '0'..'9' as c, t) =>
        next(Some(Number(Char.code(c) - zero)), t)
      | (None, '+', t) => next(Some(Plus), t)
      | (None, '-', t) => next(Some(Minus), t)
      | (None, '*', t) => next(Some(Multiply), t)
      | (None, '/', t) => next(Some(Divide), t)
      | (None, '(', t) => next(Some(OpenParens), t)
      | (None, ')', t) => next(Some(CloseParens), t)

      /* State: Number */
      | (Some(Number(n)), '0'..'9' as c, t) =>
        next(Some(Number(n * 10 + Char.code(c) - zero)), t)

      /* State: Some(token) */
      | (Some(token), ' ', t) => next(None, [token, ...t])
      | (Some(token), '0'..'9' as c, t) =>
        next(Some(Number(Char.code(c) - zero)), [token, ...t])
      | (Some(token), '+', t) => next(Some(Plus), [token, ...t])
      | (Some(token), '-', t) => next(Some(Minus), [token, ...t])
      | (Some(token), '*', t) => next(Some(Multiply), [token, ...t])
      | (Some(token), '/', t) => next(Some(Divide), [token, ...t])
      | (Some(token), '(', t) => next(Some(OpenParens), [token, ...t])
      | (Some(token), ')', t) => next(Some(CloseParens), [token, ...t])

      | (_, c, _) => Error("unexpected character " ++ Char.escaped(c))
      };
    };
  };

  Exploder.explode(input)->doTokenize(None, []);
};
