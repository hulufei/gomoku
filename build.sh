#!/usr/bin/env bash

elm make Main.elm --output static/elm.js
cp style.css static/