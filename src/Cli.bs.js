// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Enquirer = require("enquirer");
var Belt_Result = require("bs-platform/lib/js/belt_Result.js");
var Parser$Calc = require("./Parser.bs.js");
var Visitor$Calc = require("./Visitor.bs.js");
var Tokenizer$Calc = require("./Tokenizer.bs.js");

var question = ({
  type: "input",
  name: "expression",
  message: () => "",
});

function mapFlat(cb, value) {
  return Belt_Result.flatMap(value, cb);
}

function loop(param) {
  return Enquirer.prompt(question).then(function (answer) {
              var expression = answer.expression;
              var result = Belt_Result.flatMap(Tokenizer$Calc.tokenize(expression), (function (tokens) {
                      return Belt_Result.flatMap(Parser$Calc.parse(tokens), Visitor$Calc.visit);
                    }));
              if (result.TAG) {
                console.log("error: " + result._0);
              } else {
                console.log(result._0);
              }
              return loop(undefined);
            });
}

function cli(param) {
  return loop(undefined);
}

exports.question = question;
exports.mapFlat = mapFlat;
exports.loop = loop;
exports.cli = cli;
/* question Not a pure module */
