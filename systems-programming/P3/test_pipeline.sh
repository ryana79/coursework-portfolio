#!/bin/sh
# test_pipeline.sh - Pipeline Test
# ensure that test_pipeline_input.txt exists in the same directory

# create a pipeline that counts the words in test_pipeline_input.txt
cat test_pipeline_input.txt | wc -w
