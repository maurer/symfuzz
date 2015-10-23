SymFuzz
=======

This is an input-bit dependence inference prototype. See our Oakland 2015 paper
for more details.

Install
-------

We recommend using OPAM for building SymFuzz.

1. Install OPAM(https://opam.ocaml.org/) with OCaml version *4.02.1* or higher.

2. Install libBIL (https://github.com/sangkilc/libbil)

   git clone https://github.com/sangkilc/libbil.git
   cd libbil; make install; cd ..

3. Install libInput (https://github.com/sangkilc/libinput)

   git clone https://github.com/sangkilc/libinput.git
   cd libinput; make install; cd ..

4. Build SymFuzz

   1. Download and unpack pin

     ./fetch-pin.sh

   2. Build SymFuzz

     make
