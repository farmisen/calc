open Jest;
open CalcLexer;

describe("tokenize", () => {
  open Expect;
  test("number", () => {
    expect(tokenize("17")) |> toEqual([Number(17)])
  });

  test("explicitly positive number", () => {
    let res = tokenize("+12");
    expect(res) |> toEqual([Plus, Number(12)]);
  });

  test("negative number", () => {
    let res = tokenize("-12");
    expect(res) |> toEqual([Minus, Number(12)]);
  });

  test("mixed numbers", () => {
    let res = tokenize("17 0 123 -1");
    expect(res)
    |> toEqual([Number(17), Number(0), Number(123), Minus, Number(1)]);
  });

  test("operators", () => {
    expect(tokenize("+ - * /")) |> toEqual([Plus, Minus, Multiply, Divide])
  });

  test("parens", () => {
    expect(tokenize("( )")) |> toEqual([OpenParens, CloseParens])
  });

  test("sticky parens", () => {
    expect(tokenize("(())"))
    |> toEqual([OpenParens, OpenParens, CloseParens, CloseParens])
  });

  test("expression", () => {
    expect(tokenize("1 + 3")) |> toEqual([Number(1), Plus, Number(3)])
  });

  test("compact expression", () => {
    expect(tokenize("1+3")) |> toEqual([Number(1), Plus, Number(3)])
  });
});
