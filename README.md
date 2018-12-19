PLT FALL 2018

The LLVM toolchain is necessary for the source code to compile, and for the compiler to compile any code.
```
sudo apt install llvm-6.0
```
**Notice: please install ppx_deriving and llvm-ocaml to make the code compile. **
```
opam install ppx_deriving
opam install llvm
```

To compile the main program, please run "make" in the root directory of the project.
Before trying to test any .cst file, please run "make" in stdlib directory.

After completing the above steps, you can run "./test.sh tests/[TESTNAME]" to compile the tests. The compiled binary is called "out", which can be run by doing "./out"
**You might need to edit test.sh to change llc-6.0 to the path of the llc executable on your system.**
