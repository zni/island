#!/bin/bash
ISLAND=dist/build/island/island
ls -1 tests/*.src | while read test; do $ISLAND $test; done
