SymFuzz
=======

This is an input-bit dependence inference prototype. See our Oakland 2015 paper
for more details.

The paper used 3 components:

1. The software contained in this repository (`symfuzz`).
2. [`ofuzz`](https://github.com/sangkilc/ofuzz)
3. A [patch](https://github.com/maurer/nixlocal/blob/master/pkgs/afl-symfuzz/symfuzz.patch) to `afl-fuzz`

Install
=======

VirtualBox
-------
Probably the easiest way to try `symfuzz` is to use our VirtualBox [appliance](https://mirrors.aegis.cylab.cmu.edu/aegis/symfuzz.ova).

Nix-based
-------
If using Nix on your system, feel free to use my [repository](https://github.com/maurer/nixlocal/) used in the production of the VirtualBox image.

Manual
-------

We recommend using OPAM for building SymFuzz.

1. Install [OPAM](https://opam.ocaml.org/) with OCaml version *4.02.1* or higher.

2. Install [libBIL](https://github.com/sangkilc/libbil)
   ```
   git clone https://github.com/sangkilc/libbil.git
   cd libbil; make install; cd ..
   ```

3. Install [libInput](https://github.com/sangkilc/libinput)
   ```
   git clone https://github.com/sangkilc/libinput.git
   cd libinput; make install; cd ..
   ```

4. Build SymFuzz
   ```
   ./fetch-pin.sh
   make
   ```

Usage
======

Use of this tool is largely undocumented.
If you want to provide documentation, feel free to write a wiki entry or submit
a pull request.
