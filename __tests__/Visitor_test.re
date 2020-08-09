open Jest;
open Visitor;
open Ast;
open Belt.Result;

describe("visit", () => {
  open Expect;

  test("empty", () => {
    switch (visit(Empty)) {
    | Ok(value) => expect(value) |> toEqual(0)
    | Error(err) => failwith(err)
    }
  });

  test("number", () => {
    switch (visit(Node(Number(123), Empty, Empty))) {
    | Ok(value) => expect(value) |> toEqual(123)
    | Error(err) => failwith(err)
    }
  });

  test("adding 2 numbers", () => {
    switch (
      visit(
        Node(
          Plus,
          Node(Number(1), Empty, Empty),
          Node(Number(3), Empty, Empty),
        ),
      )
    ) {
    | Ok(value) => expect(value) |> toEqual(4)
    | Error(err) => failwith(err)
    }
  });

  test("adding 3 numbers", () => {
    switch (
      visit(
        Node(
          Plus,
          Node(Number(1), Empty, Empty),
          Node(
            Plus,
            Node(Number(3), Empty, Empty),
            Node(Number(4), Empty, Empty),
          ),
        ),
      )
    ) {
    | Ok(value) => expect(value) |> toEqual(8)
    | Error(err) => failwith(err)
    }
  });

  test("subbing 2 numbers", () => {
    switch (
      visit(
        Node(
          Minus,
          Node(Number(1), Empty, Empty),
          Node(Number(3), Empty, Empty),
        ),
      )
    ) {
    | Ok(value) => expect(value) |> toEqual(-2)
    | Error(err) => failwith(err)
    }
  });

  test("multiplying 2 numbers", () => {
    switch (
      visit(
        Node(
          Multiply,
          Node(Number(2), Empty, Empty),
          Node(Number(3), Empty, Empty),
        ),
      )
    ) {
    | Ok(value) => expect(value) |> toEqual(6)
    | Error(err) => failwith(err)
    }
  });

  test("dividing 2 numbers", () => {
    switch (
      visit(
        Node(
          Divide,
          Node(Number(9), Empty, Empty),
          Node(Number(2), Empty, Empty),
        ),
      )
    ) {
    | Ok(value) => expect(value) |> toEqual(4)
    | Error(err) => failwith(err)
    }
  });

  test("expression", () => {
    switch (
      visit(
        Node(
          Multiply,
          Node(
            Plus,
            Node(Number(5), Empty, Empty),
            Node(
              Minus,
              Node(Number(2), Empty, Empty),
              Node(Number(3), Empty, Empty),
            ),
          ),
          Node(Number(4), Empty, Empty),
        ),
      )
    ) {
    | Ok(value) => expect(value) |> toEqual(16)
    | Error(err) => failwith(err)
    }
  });
});
