#!/bin/sh
# Copyright (c) 2025, The MITRE Corporation.


# NOTE: Before running this, export DEMO_ROOT = ...your path here.../LayeredAttestation/src/demo_layered_attestation

: "${DEMO_ROOT:?DEMO_ROOT must be set to run this script}"

export DEMO_SRC=$DEMO_ROOT/email_header_filtering_demo 
export CDS_INSTALLED=$DEMO_ROOT/installed_dir
export CDS_BIN=$CDS_INSTALLED/bin
export DIFF_PROG=`which diff`
mkdir -p $CDS_BIN

(cd $DEMO_SRC/lib; dune build @install; echo 'built lib')
(cd $DEMO_SRC/bin; dune build @install; echo 'built bin')
(cd $DEMO_SRC; dune install --bin $CDS_BIN; echo installed)

$CDS_BIN/orchestrate  
$CDS_BIN/deliver_to_cds


