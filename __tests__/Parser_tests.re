open Jest;
open Parser;
open Ast;
open Belt.Result;

describe("parse", () => {
  open Expect;

  test("number", () => {
    switch (Tokenizer.tokenize("123")) {
    | Ok(tokens) =>
      expect(tokens |> parse)
      |> toEqual(Ok(Node(Number(123), Empty, Empty)))
    | Error(err) => failwith(err)
    }
  });

  test("numbers", () => {
    switch (Tokenizer.tokenize("123 456")) {
    | Ok(tokens) =>
      expect(tokens |> parse) |> toEqual(Error("unexpected token"))
    | Error(err) => failwith(err)
    }
  });

  test("adding 2 numbers", () => {
    switch (Tokenizer.tokenize("1 + 3")) {
    | Ok(tokens) =>
      expect(tokens |> parse)
      |> toEqual(
           Ok(
             Node(
               Plus,
               Node(Number(1), Empty, Empty),
               Node(Number(3), Empty, Empty),
             ),
           ),
         )
    | Error(err) => failwith(err)
    }
  });

  test("adding 3 numbers", () => {
    switch (Tokenizer.tokenize("1 + 3 + 4")) {
    | Ok(tokens) =>
      expect(tokens |> parse)
      |> toEqual(
           Ok(
             Node(
               Plus,
               Node(Number(1), Empty, Empty),
               Node(
                 Plus,
                 Node(Number(3), Empty, Empty),
                 Node(Number(4), Empty, Empty),
               ),
             ),
           ),
         )
    | Error(err) => failwith(err)
    }
  });

  test("subbing", () => {
    switch (Tokenizer.tokenize("1 - 3")) {
    | Ok(tokens) =>
      expect(tokens |> parse)
      |> toEqual(
           Ok(
             Node(
               Minus,
               Node(Number(1), Empty, Empty),
               Node(Number(3), Empty, Empty),
             ),
           ),
         )
    | Error(err) => failwith(err)
    }
  });

  test("multiplying", () => {
    switch (Tokenizer.tokenize("1 * 3")) {
    | Ok(tokens) =>
      expect(tokens |> parse)
      |> toEqual(
           Ok(
             Node(
               Multiply,
               Node(Number(1), Empty, Empty),
               Node(Number(3), Empty, Empty),
             ),
           ),
         )
    | Error(err) => failwith(err)
    }
  });

  test("dividing", () => {
    switch (Tokenizer.tokenize("1 / 3")) {
    | Ok(tokens) =>
      expect(tokens |> parse)
      |> toEqual(
           Ok(
             Node(
               Divide,
               Node(Number(1), Empty, Empty),
               Node(Number(3), Empty, Empty),
             ),
           ),
         )
    | Error(err) => failwith(err)
    }
  });

  test("parens", () => {
    switch (Tokenizer.tokenize("(1 + 3)")) {
    | Ok(tokens) =>
      expect(tokens |> parse)
      |> toEqual(
           Ok(
             Node(
               Plus,
               Node(Number(1), Empty, Empty),
               Node(Number(3), Empty, Empty),
             ),
           ),
         )
    | Error(err) => failwith(err)
    }
  });

  test("expression", () => {
    switch (Tokenizer.tokenize("(1 + 3) * 4")) {
    | Ok(tokens) =>
      expect(tokens |> parse)
      |> toEqual(
           Ok(
             Node(
               Multiply,
               Node(
                 Plus,
                 Node(Number(1), Empty, Empty),
                 Node(Number(3), Empty, Empty),
               ),
               Node(Number(4), Empty, Empty),
             ),
           ),
         )
    | Error(err) => failwith(err)
    }
  });

  test("nested", () => {
    switch (Tokenizer.tokenize("(1 + (2 - 3)) * 4")) {
    | Ok(tokens) =>
      expect(tokens |> parse)
      |> toEqual(
           Ok(
             Node(
               Multiply,
               Node(
                 Plus,
                 Node(Number(1), Empty, Empty),
                 Node(
                   Minus,
                   Node(Number(2), Empty, Empty),
                   Node(Number(3), Empty, Empty),
                 ),
               ),
               Node(Number(4), Empty, Empty),
             ),
           ),
         )
    | Error(err) => failwith(err)
    }
  });
});
