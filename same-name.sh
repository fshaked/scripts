#!/bin/bash

TPATH="$1"
NAME="$2"

find "$TPATH" -name "$NAME" | sed 's|\(.*/\)\([^/]*\)|\1 \2|' | sort -k2 | awk 'name == $2 { print toprint $1 $2; toprint = ""; next; } { name = $2; toprint = $1 $2 "\n"; }'
