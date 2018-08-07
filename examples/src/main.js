#!/usr/bin/env node
let program = require('./elm.js').Main.worker(process.argv)
XMLHttpRequest = require('xhr2')

program.ports.print.subscribe(message => console.log(message))
program.ports.printAndExitFailure.subscribe(message => {
  console.log(message)
  process.exit(1)
})
program.ports.printAndExitSuccess.subscribe(message => {
  console.log(message)
  process.exit(0)
})
