#!/usr/bin/env bash

for mode in $(echo $FFI | tr ":" "\n"); do

	echo "Testing ${mode}"

	lazbuild $LAZ_OPT LapeTestFFI.lpr --build-mode=$mode > log
	
	if [ $? == 0 ]; then
		$LAZ_ENV ./LapeTestFFI
		if [ $? != 0 ]; then
			export err=1;
		fi
	else
		cat log	
		export err=1;
	fi
done;

exit $err;
