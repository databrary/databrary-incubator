#!/bin/bash -e
# Control interface for transcode jobs.
# This is run directly from the application, on the webserver.
# It calls transcode, possibly on transcode.host.

escape() {
	for a in "$@" ; do
		echo \'${a//\'/\'\\\'\'}\'
	done
}

cmd=`dirname $0`/transcode

if [[ ! -f $cmd ]] ; then
	echo "$cmd: not found" >&2
	exit 2
fi

while getopts 'i:h:d:v:m:c:k:s:r:f:t' opt ; do case "$opt" in
	i) id=$OPTARG ;;
	h) host=$OPTARG ;;
	d) dir=$OPTARG ;;
	v) version=$OPTARG ;;
	m) mount=$OPTARG ;;

	c) collect=$OPTARG ;;
	k) kill=$OPTARG ;;
	s) src=$OPTARG ;;
	r) url=$OPTARG ;;
	f) fmt=$OPTARG ;;
	t) test=1 ;;

	?) exit 1 ;;
esac ; done

hcmd=./transcode${version:+-$version}

if [[ -n $test ]] ; then
	if [[ -z $dir ]] ; then
		false
	elif [[ -n $host ]] ; then
		ssh "$host" test -d "$dir"
		if [[ -n $mount ]] ; then
			cp -pf "$cmd" "$mount/$hcmd"
			ssh "$host" rsync -p "$mount/$hcmd" "$hcmd"
		else
			rsync -p "$cmd" "$host:$hcmd"
		fi
	else
		test -d "$dir"
	fi
	exit $?
fi

if [[ -z $id || -z $dir || -z $collect$kill && ( -z $src || -z $url || -z $fmt ) ]] ; then
	echo "$0: usage error: $*" >&2
	exit 1
fi

if [[ -n $collect ]] ; then
	if [[ -n $host ]] ; then
		if [[ -n $mount ]] ; then
			ssh "$host" rsync "$dir/$id.$fmt" "$mount/$dir/$id.$fmt"
			mv "$mount/$dir/$id.$fmt" "$collect"
			rm -f "$mount/$dir/$id"
		else
			rsync "$host:$dir/$id.$fmt" "$collect"
		fi
		ssh "$host" rm -f "$dir/$id" "$dir/$id.$fmt"
	else
		mv "$dir/$id.$fmt" "$collect"
		rm -f "$dir/$id"
	fi
elif [[ -n $host ]] ; then
	if [[ -z $kill ]] ; then
		if [[ -n $mount ]] ; then
			ln -fT "$src" "$mount/$dir/$id"
		else
			rsync "$src" "$host:$dir/$id"
		fi
	fi
	# grab only job id from e.g. "Submitted batch job 234324324"
	ssh "$host" "$hcmd" `escape "$@"` | sed 's/^[^0-9]*\([0-9]\+\)$/\1/'
elif [[ -n $kill ]] ; then
	"$cmd" "$@"
else
	ln -fT "$src" "$dir/$id"
	"$cmd" "$@" &
	echo $!
fi

