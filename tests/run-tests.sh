#!/bin/bash

ls -1 tests | while read test; do island tests/$test; done
