let join = (sep, array) => {
  let rec doJoin = (a, accu) => {
    switch (a) {
    | [] => accu
    | [head, ...tail] => doJoin(tail, accu ++ sep ++ head)
    };
  };

  doJoin(array, "");
};
