#! /bin/bash

set -o errexit;
set -o nounset;

cd examples

if [ ! -f elm.js ]; then
    printf "Missing elm.js, run \`npm run build-examples\` to generate\n"
    exit 1
fi

set -o verbose;



./elm-test
./git init
./graphqelm https://example.com
./simple --name Steve
./validation --name Steve
./curl https://example.com
echo "a\nb\nc" | ./grep  b
