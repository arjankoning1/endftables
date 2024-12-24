
# ENDFTABLES
ENDFTABLES is a software package for the processing ENDF-6 format nuclear data libraries into x-y(-dy) tables, with one file per reaction channel.

## Documentation and reference
A description of the code and its options can be found in the [ENDFTABLES tutorial (pdf)](https://github.com/arjankoning1/endftables/blob/main/doc/tools.pdf).
The reference to be used for ENDFTABLES is

A.J. Koning, D. Rochman, J.-Ch. Sublet, N. Dzysiuk, M. Fleming, and S. van der Marck, *TENDL: Complete Nuclear Data Library for innovative Nuclear Science and Technology*, Nuclear Data Sheets 155,1 (2019).

## Installation

### Prerequisites:

The following are the prerequisites for compiling ENDFTABLES:
  - git (only if the package is downloaded via Github)
  - a recent Fortran compiler such as gcc (gfortran)

### Downloads:

To download ENDFTABLES, you can use one of the following options:
#### 1. Download the tar file (frozen version):
```
https://nds.iaea.org/talys/endftables.tar
tar zxf endftables.tar
```

#### 2. Using git (latest beta version):
```
git clone https://github.com/arjankoning1/endftables.git
```
The ENDFTABLES sample case does not fall under the git repository. For that you need to download:
```
https://nds.iaea.org/talys/samples/endftables_samples.tar
tar zxf endftables_samples.tar
```
The resulting *samples/* directory should be moved inside the *endftables/* directory.
### Installation instructions :

To install ENDFTABLES, you can use one of the following options:
#### 1. Using make:
```
cd endftables/source
make
```
#### 2. Using the Install_endftables.bash script:
```
cd endftables
Install_endftables.bash 
```

The above will produce a *endftables* executable in the *endftables/bin* directory.
The compiler and its flags can be set in either the *source/Makefile* or in *code_build.bash*.

## Sample cases

A successful installation can be verified by running the sample case. For this sample case, the results are written to a subdirectory *new/*, which can then be compared with the output provided in the subdirectory *org/*. The sample case will take a few seconds.
```
cd samples/new
../../bin/endftables n-U238.endfb8.0
```

## The ENDFTABLES package

The *endftables/* directory contains the following directories and files:

+ `README.md` is this README file
+ `LICENSE` is the License file
+ `install_endftables.bash` and `code_build.bash` are installation scripts
+ `source/` contains the Fortran source code of ENDFTABLES and the Makefile
+ `bin/` contains the executable after successful installation
+ `doc/` contains the tutorial in pdf format
+ `samples/` contains the input and output files of the sample case

In total, you will need about 70 Mb of free disk space to install ENDFTABLES.

## License and Copyright
This software is distributed and copyrighted according to the [LICENSE](LICENSE) file.
