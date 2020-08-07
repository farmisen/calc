// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';


function join(sep, array) {
  var _a = array;
  var _accu = "";
  while(true) {
    var accu = _accu;
    var a = _a;
    if (!a) {
      return accu;
    }
    _accu = accu + (sep + a.hd);
    _a = a.tl;
    continue ;
  };
}

exports.join = join;
/* No side effect */
