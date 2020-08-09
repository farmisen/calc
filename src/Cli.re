open Js.Promise;
open Belt.Result;
open Tokenizer;
open Parser;
open Visitor;

let question = [%bs.raw
  {|{
  type: "input",
  name: "expression",
  message: () => "",
}|}
];

let mapFlat = (cb, value) => flatMap(value, cb);

let rec loop = () => {
  Enquirer.prompt(question)
  |> then_(answer => {
       let expression = answer##expression;

       let result =
         expression
         |> tokenize
         |> mapFlat(tokens =>
              tokens |> parse |> mapFlat(nodes => nodes |> visit)
            );

       switch (result) {
       | Ok(value) => Js.log(value)
       | Error(message) => Js.log("error: " ++ message)
       };

       loop();
     });
};

let cli = () => {
  loop();
};
