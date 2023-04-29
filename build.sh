#!/bin/sh

mkdir -p dist
elm make src/Main.elm --optimize --output=dist/elm.js
cp assets/* dist/
