open Jest;
open Tokenizer;
open Belt.Result;

describe("tokenize", () => {
  open Expect;

  test("space", () => {
    expect(tokenize("")) |> toEqual(Ok([]))
  });

  test("spaces", () => {
    expect(tokenize("    ")) |> toEqual(Ok([]))
  });

  test("number", () => {
    expect(tokenize("17")) |> toEqual(Ok([Number(17)]))
  });

  test("explicitly positive number", () => {
    let res = tokenize("+12");
    expect(res) |> toEqual(Ok([Plus, Number(12)]));
  });

  test("negative number", () => {
    let res = tokenize("-12");
    expect(res) |> toEqual(Ok([Minus, Number(12)]));
  });

  test("mixed numbers", () => {
    let res = tokenize("17 0 123 -1");
    expect(res)
    |> toEqual(
         Ok([Number(17), Number(0), Number(123), Minus, Number(1)]),
       );
  });

  test("operators", () => {
    expect(tokenize("+ - * /"))
    |> toEqual(Ok([Plus, Minus, Multiply, Divide]))
  });

  test("parens", () => {
    expect(tokenize("( )")) |> toEqual(Ok([OpenParens, CloseParens]))
  });

  test("sticky parens", () => {
    expect(tokenize("(())"))
    |> toEqual(Ok([OpenParens, OpenParens, CloseParens, CloseParens]))
  });

  test("expression", () => {
    expect(tokenize("1 + 3"))
    |> toEqual(Ok([Number(1), Plus, Number(3)]))
  });

  test("compact expression", () => {
    expect(tokenize("1+3")) |> toEqual(Ok([Number(1), Plus, Number(3)]))
  });

  test("bad char", () => {
    expect(tokenize("1*{3-2}")) |> toEqual(Error("unexpected character {"))
  });
});
