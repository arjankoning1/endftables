# ENDFTABLES

ENDFTABLES is a software package for processing ENDF-6 formatted nuclear data libraries into x-y(-dy) tables, with one output file per reaction channel.

## Documentation and reference

A description of the code and its options can be found in the [ENDFTABLES tutorial (pdf)](https://github.com/arjankoning1/endftables/blob/main/doc/tools.pdf).

The reference to be used for ENDFTABLES is:

A.J. Koning, D. Rochman, J.-Ch. Sublet, N. Dzysiuk, M. Fleming, and S. van der Marck, *TENDL: Complete Nuclear Data Library for innovative Nuclear Science and Technology*, Nuclear Data Sheets 155, 1 (2019).

## Installation

### Prerequisites

The following are the prerequisites for compiling ENDFTABLES:

- git (only if the package is downloaded via GitHub)
- GNU make
- a recent Fortran compiler, such as GNU Fortran (gfortran)

ENDFTABLES does not require an external runtime database. The ENDF-6 file to be processed is supplied directly on the command line.

### Downloads

#### 1. Download the tar file (frozen version ENDFTABLES-2.2)

```bash
curl -LO https://nds.iaea.org/talys/codes/endftables.tar
tar zxf endftables.tar
```

#### 2. Using git (latest beta version)

```bash
git clone https://github.com/arjankoning1/endftables.git
```

### Installation instructions

#### 1. For the tar file (frozen version ENDFTABLES-2.2)

```bash
cd endftables
./install_endftables.bash
```

An alternative is:

```bash
cd endftables/source
make
```

The frozen distribution retains its own installation scripts and settings.

#### 2. For the git version (latest beta version)

```bash
cd endftables
./install_endftables.bash
```

which automatically executes the `Makefile` in `endftables/source`.

An alternative is:

```bash
cd endftables/source
make
```

For the git version, the default compiler is `gfortran`. When `gfortran` is used and no `FFLAGS` are supplied, the Makefile uses:

```text
-w -O3 -ffp-contract=off
```

For other compilers, no default compiler flags are imposed.

Compiler and compilation options can be passed through `install_endftables.bash`, for example:

```bash
./install_endftables.bash FC=gfortran FFLAGS="-O3 -ffp-contract=off"
./install_endftables.bash FC=ifx FFLAGS="-O3"
```

The executable is installed as:

```text
endftables/bin/endftables
```

If you want to run `endftables` from anywhere, add its `bin` directory to `PATH`, for example:

```bash
export PATH="/path/to/endftables/bin:$PATH"
```

To include your name in generated output files, set:

```bash
export ENDFTABLES_USER="Your Name"
```

The `user` input keyword can override `ENDFTABLES_USER` for an individual run.

These lines can be added to `~/.zshrc` or `~/.profile`.

Unlike TALYS, TEFAL, TASMAN, ISOTOPIA and RESONANCETABLES, ENDFTABLES does **not** need an `ENDFTABLES_DIR` runtime environment variable or a `machine.f90`: it reads the input ENDF-6 file directly from the command line and has no package-relative runtime data files.

For the modern git version, `code_build.bash` is no longer required and can be removed after adopting the new installer and Makefile. The stale reference to `path_change.bash` in the old installer is also removed.

## Sample case

A successful installation can be verified with the supplied U-238 ENDF/B-VIII.1 sample:

```bash
cd samples
./verify
```

From the top-level ENDFTABLES directory, the same check can be started with:

```bash
make -C source check
```

The current sample verifier processes:

```text
samples/new/n-U238.endfb8.1
```

The sample run takes only a few seconds.

ENDFTABLES can be run on another ENDF-6 file as:

```bash
endftables myfile.endf
```

The program writes the generated tables in the current working directory.

## The ENDFTABLES package

The `endftables/` directory contains:

- `README.md` this README file
- `LICENSE` the license file
- `install_endftables.bash` installation script
- `source/` the Fortran source code and Makefile
- `bin/` the executable after successful installation
- `doc/` the tutorial
- `samples/` the sample ENDF-6 file and generated output

In total, about 70 MB of free disk space is required to install ENDFTABLES.

## License and Copyright

This software is distributed and copyrighted according to the [LICENSE](LICENSE) file.
