#!/bin/sh

set -ex

tmp_dir=$(pwd)

cabal build streamly-bench conduit-bench

streamly_exe="$(cabal exec --verbose=0 --offline sh -- -c "command -v streamly-bench")"
conduit_exe="$(cabal exec --verbose=0 --offline sh -- -c "command -v conduit-bench")"

head -c 10G </dev/urandom >"${tmp_dir}"/streamly-bench.dat

time $conduit_exe "${tmp_dir}"/streamly-bench.dat "${tmp_dir}"/streamly-bench.dat.conduit.1
time $conduit_exe "${tmp_dir}"/streamly-bench.dat "${tmp_dir}"/streamly-bench.dat.conduit.2
time $conduit_exe "${tmp_dir}"/streamly-bench.dat "${tmp_dir}"/streamly-bench.dat.conduit.3
time $conduit_exe "${tmp_dir}"/streamly-bench.dat "${tmp_dir}"/streamly-bench.dat.conduit.4
time $conduit_exe "${tmp_dir}"/streamly-bench.dat "${tmp_dir}"/streamly-bench.dat.conduit.5

time $streamly_exe "${tmp_dir}"/streamly-bench.dat "${tmp_dir}"/streamly-bench.dat.streamly.1
time $streamly_exe "${tmp_dir}"/streamly-bench.dat "${tmp_dir}"/streamly-bench.dat.streamly.2
time $streamly_exe "${tmp_dir}"/streamly-bench.dat "${tmp_dir}"/streamly-bench.dat.streamly.3
time $streamly_exe "${tmp_dir}"/streamly-bench.dat "${tmp_dir}"/streamly-bench.dat.streamly.4
time $streamly_exe "${tmp_dir}"/streamly-bench.dat "${tmp_dir}"/streamly-bench.dat.streamly.5

rm "${tmp_dir}"/streamly-bench.dat*

