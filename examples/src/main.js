#!/usr/bin/env node
let program = require("./elm.js").Elm.Main.init({
  flags: { argv: process.argv, version: "1.2.3" }
});
XMLHttpRequest = require("xhr2");

program.ports.print.subscribe(message => console.log(message));
program.ports.printAndExitFailure.subscribe(message => {
  console.log(message);
  process.exit(1);
});
program.ports.printAndExitSuccess.subscribe(message => {
  console.log(message);
  process.exit(0);
});
