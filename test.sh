#!/bin/bash
set -e

echo "=== Building programs ==="
dune build example/main.c.exe cas/main.c.exe sudoku/main.c.exe

echo ""
echo "=== Running example (fib) ==="
echo "Claude" | ./_build/default/example/main.c.exe

echo ""
echo "=== Running CAS ==="
printf "2x^2 + 3x + 1\nD x\nQ\n" | ./_build/default/cas/main.c.exe

echo ""
echo "=== Running Sudoku ==="
printf "530070000\n600195000\n098000060\n800060003\n400803001\n700020006\n060000280\n000419005\n000080079\n" | ./_build/default/sudoku/main.c.exe

echo ""
echo "=== All tests passed ==="
