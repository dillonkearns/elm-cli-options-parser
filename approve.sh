#!/usr/bin/env bash

set -o errexit;
set -o nounset;

npm run build-examples
cd examples

set -o verbose;

./elm-test
./git init
./graphqelm https://example.com
./simple --name Steve
./validation --name Steve
./curl https://example.com
echo "a\nb\nc" | ./grep  b
