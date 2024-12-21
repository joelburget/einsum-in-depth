# Einsum Tutorial

Explaining and exploring tensor operations.

## Building and running

```
opam pin brr_svg git+https://gitlab.com/schutm/brr_svg
opam install ppx_expect js_of_ocaml menhir note fmt brr
cd frontend
pnpm tailwindcss -i input.css -o output.css
cd ..
dune build -w
cd _build/default/frontend
pnpm i
pnpm start
```

You should see "Server running at http://localhost:1234".
