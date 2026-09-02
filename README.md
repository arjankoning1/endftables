# ENDFTABLES

ENDFTABLES is a software package for processing ENDF-6 formatted nuclear data libraries into x-y(-dy) tables, with one output file per reaction channel.

## Documentation and reference

A description of the code and its options can be found in the [ENDFTABLES tutorial (pdf)](https://github.com/arjankoning1/endftables/blob/main/doc/tools.pdf).

The reference to be used for ENDFTABLES is:

A.J. Koning, D. Rochman, J.-Ch. Sublet, N. Dzysiuk, M. Fleming, and S. van der Marck, *TENDL: Complete Nuclear Data Library for innovative Nuclear Science and Technology*, Nuclear Data Sheets 155, 1 (2019).

## Installation

### Prerequisites

The following are the prerequisites for compiling ENDFTABLES:

- GNU make
- a recent Fortran compiler, such as GNU Fortran (gfortran)
- git, only when ENDFTABLES is downloaded using `git clone`

ENDFTABLES does not require an external runtime database. The ENDF-6 file to be processed is supplied directly on the command line.

### Downloads

ENDFTABLES can be downloaded in one of the following ways.

#### 1. Frozen version ENDFTABLES-2.2 (December 2025)

The frozen ENDFTABLES-2.2 distribution is available from the [TALYS page](https://nds.iaea.org/talys/). It can be retrieved by clicking on the download link or with

```bash
curl -LO https://nds.iaea.org/talys/codes/endftables.tar
tar zxf endftables.tar
```

This version is fixed and will not change.

#### 2. Latest beta version without git

Users who do not have git can download a snapshot of the current `main` branch directly from GitHub:

```bash
curl -L \
  -o endftables-main.tar.gz \
  https://github.com/arjankoning1/endftables/archive/refs/heads/main.tar.gz

tar zxf endftables-main.tar.gz
mv endftables-main endftables
```

This produces the same `endftables/` directory structure as the git version, but without the git history.

The downloaded snapshot contains the latest version of the `main` branch at the time of download. To obtain a newer version later, download the snapshot again.

#### 3. Latest beta version using git

Users with git can clone the repository with

```bash
git clone https://github.com/arjankoning1/endftables.git
```

The advantage of this method is that the local ENDFTABLES installation can subsequently be updated with

```bash
cd endftables
git pull --ff-only
```

### Installation instructions

#### 1. Frozen version ENDFTABLES-2.2

For the frozen tar distribution:

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

#### 2. Latest beta version

The installation procedure is identical whether the latest beta version was obtained as a GitHub tar snapshot or using `git clone`.

From the `endftables/` directory, run

```bash
./install_endftables.bash
```

which automatically executes the `Makefile` in `endftables/source`.

An alternative is:

```bash
cd endftables/source
make
```

The executable is installed as

```text
endftables/bin/endftables
```

For the latest beta version, the default compiler is `gfortran`. When `gfortran` is used and no `FFLAGS` are supplied, the Makefile uses:

```text
-w -O3 -ffp-contract=off
```

For other compilers, no default compiler flags are imposed.

Compiler and compilation options can be passed through `install_endftables.bash`, for example:

```bash
# GNU Fortran
./install_endftables.bash FC=gfortran FFLAGS="-O3 -ffp-contract=off"

# Intel Fortran
./install_endftables.bash FC=ifx FFLAGS="-O3"
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
