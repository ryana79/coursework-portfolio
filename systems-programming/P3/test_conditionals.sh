#!/bin/sh
# test_conditionals.sh - Conditional Execution Test
# using 'false' and 'true' to check conditional command execution

# execute a command that fails
false
# this command (prefixed by 'and') should NOT run because the previous command failed
and echo should_not_run

# execute a command that succeeds
true
# this command (prefixed by 'or') should NOT run because the previous command succeeded
or echo should_not_run

# another test: since true succeeds, the command after 'and' should execute
true
and echo should_run

# execute a command that fails, so the following 'or' command runs
false
or echo should_run_after_failure
