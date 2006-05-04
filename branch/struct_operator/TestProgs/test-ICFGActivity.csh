#!/bin/csh
foreach f ($argv)
    echo "=========================== Testing ", $f
    ../test-open64 --oa-ICFGActivity $f
end
