#!/usr/bin/env bash

set -euo pipefail

# Determine the ENDFTABLES installation directory independently of where
# the script is called from.

endftables_dir=$(cd "$(dirname "$0")" && pwd)
source_dir="$endftables_dir/source"

# Verify that the expected source directory and Makefile exist.

if [[ ! -d "$source_dir" ]]; then
  echo "ENDFTABLES installation error: source directory not found:" >&2
  echo "  $source_dir" >&2
  exit 1
fi

if [[ ! -f "$source_dir/Makefile" ]]; then
  echo "ENDFTABLES installation error: Makefile not found:" >&2
  echo "  $source_dir/Makefile" >&2
  exit 1
fi

echo
echo "Installing ENDFTABLES"
echo "Installation directory: $endftables_dir"
echo

# Pass all command-line arguments directly to make. This permits, e.g.:
#
# ./install_endftables.bash FC=ifx FFLAGS="-O3"
# ./install_endftables.bash FC=gfortran FFLAGS="-w -O3 -ffp-contract=off"

make -C "$source_dir" clean
make -C "$source_dir" all "$@"

endftables_exe="$endftables_dir/bin/endftables"

if [[ ! -x "$endftables_exe" ]]; then
  echo "ENDFTABLES installation error: executable not created:" >&2
  echo "  $endftables_exe" >&2
  exit 1
fi

echo
echo "ENDFTABLES executable:"
echo "  $endftables_exe"
echo
echo "If not already done, add the following line to your shell configuration:"
echo
echo "  export PATH=\"$endftables_dir/bin:\$PATH\""
echo
echo "To include your name in output files, you may also set:"
echo
echo "  export ENDFTABLES_USER=\"Your Name\""
echo
