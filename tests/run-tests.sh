#!/bin/bash
ISLAND=dist/build/island/island
ls -1 tests | while read test; do $ISLAND tests/$test; done
