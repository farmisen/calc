// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Curry = require("bs-platform/lib/js/curry.js");

Jest.describe("Expect", (function (param) {
        return Jest.test("toBe", (function (param) {
                      return Jest.Expect.toBe(3, Jest.Expect.expect(3));
                    }));
      }));

Jest.describe("Expect.Operators", (function (param) {
        return Jest.test("==", (function (param) {
                      return Curry._2(Jest.Expect.Operators.$eq$eq, Jest.Expect.expect(3), 3);
                    }));
      }));

/*  Not a pure module */
