#!/usr/bin/env bash

# USAGE: bash compare_generated.sh
# can use from any directory, but GOLITEGROUP11 env var must be set to project root dir (can be produced from pwd cmd)

FILE="$GOLITEGROUP11/most_recent_run.txt"
go_file=""
java_file=""
line_no=0
while ifs='' read -r line || [ -n "${line}" ]; do
    # should be 2 file paths in the file we read from: most_recent_run.txt
    if [ ! -f "${line}" ]; then
        echo "file does not exist, could not cat it"
        echo "line is: ${line}"
    else
        # go path is on first line, java path second
        [[ "$line_no" -eq 0 ]] && go_file="${line}" || java_file="${line}"
    fi

    line_no=$(($line_no + 1))
done < "$FILE"

# if 'bat' is installed, use this to generate colored output!
if hash bat > /dev/null 2>&1; then
    bat --paging=never "$java_file"
    bat --paging=never "$go_file"
else
    cat "$java_file"
    cat "$go_file"
fi
