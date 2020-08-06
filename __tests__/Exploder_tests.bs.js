// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Exploder$Calc = require("../src/Exploder.bs.js");

Jest.describe("explode", (function (param) {
        Jest.test("return a list of char for a non empty string", (function (param) {
                return Jest.Expect.toEqual({
                            hd: /* "T" */84,
                            tl: {
                              hd: /* "h" */104,
                              tl: {
                                hd: /* "e" */101,
                                tl: {
                                  hd: /* " " */32,
                                  tl: {
                                    hd: /* "f" */102,
                                    tl: {
                                      hd: /* "o" */111,
                                      tl: {
                                        hd: /* "x" */120,
                                        tl: {
                                          hd: /* "." */46,
                                          tl: /* [] */0
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }, Jest.Expect.expect(Exploder$Calc.explode("The fox.")));
              }));
        return Jest.test("return an empty list  char for an empty string", (function (param) {
                      return Jest.Expect.toEqual(/* [] */0, Jest.Expect.expect(Exploder$Calc.explode("")));
                    }));
      }));

/*  Not a pure module */
