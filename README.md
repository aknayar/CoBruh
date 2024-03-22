# CoBruh

Let's make a language with the readability of Python and the type safety of C!

To generate IR from a input file, run `dune exec -- CoBruh -c <input_path> > main.ll` in the `src` directory. This will output the IR in a file called `main.ll`. To execute this code, run `lli main.ll`. 

To run the unit tests, either run `dune test` in `src` or `python3 testall.py` in `src/test`.

Our LRM and writeup are included in the repo. The LRM gives a detailed description of the language, and the writeup contains our final reflections. Also, check out [this video](https://www.youtube.com/watch?v=0W9PEvtSxKI) for an introduction to CoBruh, its architecture, and demos.
