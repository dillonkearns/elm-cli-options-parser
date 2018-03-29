#!/usr/bin/env node

let program = require('./elm.js').Graphqelm.worker(process.argv)

program.ports.print.subscribe(message => console.log(message))
