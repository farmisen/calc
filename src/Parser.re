// expression
//          ::= operand ( ( PLUS | MINUS | MULTIPLY | DIVIDE ) operand )*

// operand  ::= NUMBER
//            | OPEN_PARENS expression CLOSE_PARENS

open Belt.Result;
open Ast;

exception ParsingError(string);

let parse = tokens => {
  let rec doParse = tokens => {
    switch (tokens) {
    | [] => Empty
    | [Tokenizer.Number(value), ...tail] => tail |> processNumber(value)
    | [Tokenizer.OpenParens, ...tail] => processExpression(tail)
    | _ => raise(ParsingError("unexpected token"))
    };
  }
  and processNumber = (value, tokens) => {
    switch (tokens) {
    | [] => Node(Number(value), Empty, Empty)
    | [Tokenizer.Plus, ...tail] =>
      Node(Plus, Node(Number(value), Empty, Empty), doParse(tail))
    | [Tokenizer.Minus, ...tail] =>
      Node(Minus, Node(Number(value), Empty, Empty), doParse(tail))
    | [Tokenizer.Multiply, ...tail] =>
      Node(Multiply, Node(Number(value), Empty, Empty), doParse(tail))
    | [Tokenizer.Divide, ...tail] =>
      Node(Divide, Node(Number(value), Empty, Empty), doParse(tail))
    | _ => raise(ParsingError("unexpected token"))
    };
  }
  and processExpression = tokens => {
    let rec extractExpression = (tokens, nesting, expression) => {
      switch (tokens) {
      | [] => (List.rev(expression), [])
      | [head, ...tail] =>
        switch (head) {
        | Tokenizer.OpenParens =>
          extractExpression(tail, nesting + 1, [head, ...expression])
        | Tokenizer.CloseParens =>
          switch (nesting) {
          | 1 => (List.rev(expression), tail)
          | _ => extractExpression(tail, nesting - 1, [head, ...expression])
          }

        | _ => extractExpression(tail, nesting, [head, ...expression])
        }
      };
    };

    let (expression, tail) = extractExpression(tokens, 1, []);

    let parsed = expression |> doParse;
    switch (tail) {
    | [] => parsed
    | [Tokenizer.Plus, ...tail] => Node(Plus, parsed, doParse(tail))
    | [Tokenizer.Minus, ...tail] => Node(Minus, parsed, doParse(tail))
    | [Tokenizer.Multiply, ...tail] => Node(Multiply, parsed, doParse(tail))
    | [Tokenizer.Divide, ...tail] => Node(Divide, parsed, doParse(tail))
    | _ => raise(ParsingError("unexpected token"))
    };
  };

  try(Ok(doParse(tokens))) {
  | ParsingError(msg) => Error(msg)
  };
};
