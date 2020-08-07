type terminalToken =
  | Plus
  | Minus
  | Multiply
  | Divide
  | Number(int);

type ast =
  | Empty
  | Node(terminalToken, ast, ast);
open StringUtils;

let rec toString = n =>
  switch (n) {
  | Empty => "()"
  | Node(t, a, b) =>
    "("
    ++ (
      [
        switch (t) {
        | Plus => "+"
        | Minus => "-"
        | Multiply => "*"
        | Divide => "/"
        | Number(v) => string_of_int(v)
        },
        a |> toString,
        b |> toString,
      ]
      |> join(" ")
    )
    ++ ")"
  };
