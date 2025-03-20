#!/bin/sh
# Copyright (c) 2025, The MITRE Corporation.


# NOTE: Before running this, export DEMO_ROOT = ...your path here.../LayeredAttestation/src/demo_layered_attestation


: "${DEMO_ROOT:?DEMO_ROOT must be set to run this script}"


### This file includes a locally correct value for DEMO_ROOT.
### If you would like to use this script on your system,
### please edit this line to the correct value in your
### filesystem

export DEMO_ROOT=/Users/guttman/scm/LayeredAttestation/src/demo_layered_attestation # Your value here!  

export DEMO_SRC=$DEMO_ROOT/email_header_filtering_demo 
export CDS_INSTALLED=$DEMO_ROOT/installed_dir
export CDS_BIN=$CDS_INSTALLED/bin
export DIFF_PROG=`which diff`
mkdir -p $CDS_BIN

(cd $DEMO_SRC/lib; dune build; echo 'built lib')
(cd $DEMO_SRC/bin; dune build; echo 'built bin')
(cd $DEMO_SRC; dune install --bin $CDS_BIN; echo installed)

$CDS_BIN/orchestrate  
$CDS_BIN/deliver_to_cds


