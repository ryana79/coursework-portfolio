#!/bin/sh
# test_basic.sh - Basic Commands Test

# print current working directory
pwd
# attempt to change directory to a non-existent directory (should produce error)
cd non_existent_directory
# print working directory again (should remain unchanged)
pwd
# test 'which' built-in to locate 'ls'
which ls
