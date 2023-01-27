# CoBruh
Let's make a language with the readability of Python and the type safety of C!

In order to generate IR from a input file, run `dune exec -- CoBruh -c <input_path> > main.ll` from the root directory. This will output the IR in a file called `main.ll`. In order to execute this code, run `lli main.ll`. 

To run the unit tests, either run `dune test` from the root diretory or `python3 testall.py` from the `/test` directory.

Check out [this video](https://www.youtube.com/watch?v=0W9PEvtSxKI) for an introduction to CoBruh, its architecture, and demos.
