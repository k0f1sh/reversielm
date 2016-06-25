#!/bin/sh

set -e

elm-make ElmTests.elm --output tests.js
node tests.js
