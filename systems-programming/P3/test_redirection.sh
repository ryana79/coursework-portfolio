#!/bin/sh
# test_redirection.sh - Redirection Test
# ensure that there is an input.txt file in the same directory

# use redirection to supply input from input.txt and redirect output to output.txt via cat
cat < input.txt > output.txt
# display the contents of output.txt to make sure it matches input.txt
cat output.txt
