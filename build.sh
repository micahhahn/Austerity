#!/bin/bash
set -x # echo on

# Haskell
stack build
cp $(stack path --dist-dir)/build/Austerity-exe/Austerity-exe.exe dist/Austerity.exe

# Build Typescript Bridge
stack exec TypescriptBridge $(stack path project-root)/build/Endpoints.ts

# Webpack
npm run build

# Styles
mkdir -p dist/static/styles/
cp web/css/* dist/static/styles
cp web/bootstrap/bootstrap.css dist/static/styles

stack exec Austerity-exe