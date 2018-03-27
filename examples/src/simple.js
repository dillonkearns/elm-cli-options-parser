#!/usr/bin/env node

let program = require('./elm.js').Simple.worker(process.argv)

program.ports.print.subscribe(message => console.log(message))
