#!/bin/bash
set -x # echo on

# Webpack
npm run build

# Styles
mkdir -p dist/static/styles/
cp web/css/* dist/static/styles
cp web/bootstrap/bootstrap.css dist/static/styles

# Haskell
stack build
cp $(stack path --dist-dir)/build/Austerity-exe/Austerity-exe.exe dist/Austerity.exe

stack exec Austerity-exe