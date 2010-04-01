#!/bin/sh
set -x
for d in 'funcdocs' 'funcdocs/search'
do
	for ext in 'html' 'md5' 'map' 'png' 'gif'
	do
			for f in $d/*.${ext}
			do
				rm -f $f
			done
	done
done

