open Belt.Result;

open Ast;

exception VisitingError(string);

let visit = node => {
  let rec doVisit = node => {
    switch (node) {
    | Empty => 0
    | Node(Number(value), Empty, Empty) => value
    | Node(Number(_), _, _) => raise(VisitingError("malformed Ast node"))
    | Node(Plus, left, right) => doVisit(left) + doVisit(right)
    | Node(Minus, left, right) => doVisit(left) - doVisit(right)
    | Node(Multiply, left, right) => doVisit(left) * doVisit(right)
    | Node(Divide, left, right) => doVisit(left) / doVisit(right)
    };
  };

  try(Ok(doVisit(node))) {
  | VisitingError(message) => Error(message)
  };
};
