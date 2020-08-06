open Jest;

describe("explode", () => {
  Expect.(
    test("return a list of char for a non empty string", () =>
      expect(Exploder.explode("The fox."))
      |> toEqual(['T', 'h', 'e', ' ', 'f', 'o', 'x', '.'])
    )
  );

  Expect.(
    test("return an empty list  char for an empty string", () =>
      expect(Exploder.explode("")) |> toEqual([])
    )
  );
});
