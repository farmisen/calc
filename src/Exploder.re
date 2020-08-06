let explode = str => {
  let rec doExplode = (index, chars) =>
    if (index < 0) {
      chars;
    } else {
      doExplode(index - 1, [str.[index], ...chars]);
    };
  doExplode(String.length(str) - 1, []);
};
