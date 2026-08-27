# ENDFTABLES Makefile
#
# Examples:
#
#   make
#   make FC=gfortran FFLAGS="-w -O3 -ffp-contract=off"
#   make FC=ifx FFLAGS="-O3"
#   make clean
#   make check

# Compiler-specific flags can be supplied by the user.
ifeq ($(origin FC), default)
  FC = gfortran
endif

ifeq ($(origin FFLAGS), undefined)
  ifneq ($(findstring gfortran,$(FC)),)
    FFLAGS = -w -O3 -ffp-contract=off
  else
    FFLAGS =
  endif
endif

# Installation directories.

ENDFTABLES_DIR := $(abspath ..)
BIN_DIR        := $(ENDFTABLES_DIR)/bin
TARGET         := $(BIN_DIR)/endftables

# ENDFTABLES source files.

SOURCES := $(sort $(wildcard *.f) $(wildcard *.f90))

.PHONY: all install clean check

all install: $(TARGET)

$(TARGET): $(SOURCES)
	@echo "Compiler: $(FC)"
	@echo "Flags:    $(FFLAGS)"
	@mkdir -p "$(BIN_DIR)"
	$(FC) $(FFLAGS) -o "$(TARGET)" $(SOURCES)
	@rm -f *.o *.mod
	@echo "Installed: $(TARGET)"

clean:
	rm -f *.o *.mod endftables "$(TARGET)"

check: all
	cd "$(ENDFTABLES_DIR)/samples" && ./verify
