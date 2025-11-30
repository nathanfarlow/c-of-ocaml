#!/bin/bash
set -e

echo "=== Building programs ==="
dune build examples/fib/main.c.exe examples/cas/main.c.exe examples/sudoku/main.c.exe examples/tuple_test/main.c.exe

echo ""
echo "=== Running fib ==="
echo "Claude" | ./_build/default/examples/fib/main.c.exe

echo ""
echo "=== Running CAS ==="
# Test 1: Simple polynomial derivative
printf "2x^2 + 3x + 1\nD x\nQ\n" | ./_build/default/examples/cas/main.c.exe
echo "---"
# Test 2: Higher degree polynomial with repeated derivatives
printf "x^5 + 2x^4 + 3x^3 + 4x^2 + 5x + 6\nD x\nD x\nD x\nQ\n" | ./_build/default/examples/cas/main.c.exe
echo "---"
# Test 3: Large coefficients
printf "100x^3 + 50x^2 + 25x + 12\nD x\nD x\nQ\n" | ./_build/default/examples/cas/main.c.exe
echo "---"
# Test 4: Negative coefficients
printf "x^4 - 3x^3 + 2x^2 - x + 1\nD x\nD x\nQ\n" | ./_build/default/examples/cas/main.c.exe
echo "---"
# Test 5: Many repeated derivatives (should eventually reach constant/zero)
printf "x^6\nD x\nD x\nD x\nD x\nD x\nD x\nD x\nQ\n" | ./_build/default/examples/cas/main.c.exe

echo ""
echo "=== Running Sudoku ==="
printf "530070000\n600195000\n098000060\n800060003\n400803001\n700020006\n060000280\n000419005\n000080079\n" | ./_build/default/examples/sudoku/main.c.exe

echo ""
echo "=== Running Tuple Test ==="
./_build/default/examples/tuple_test/main.c.exe

echo ""
echo "=== All tests passed ==="
