#/bin/bash

value_from_status() {
  __ret=`echo "$riak_status" | grep $1 | sed 's/.\{1,\} : \(\<\<"\)\{0,1\}\([^>]*\)\("\>\>\)\{0,1\}/\2/'`
  eval "export ${1}=\"$__ret\""
}
echo "Running Riaknostic..."

basedir=$1

release_dirs="$basedir/libexec/releases $basedir/releases $basedir"
for dir in $release_dirs
do
  if [ -f "$dir/start_erl.data" ]
  then
    riak=$dir
    echo "Riak node found: $dir"
    break
  fi
done

if [ -z "$riak" ]
then
  echo "Couldn't find a Riak installation"
  exit 1
fi

admin_cmd=`which riak-admin riaksearch-admin | tail -n 1 2>/dev/null`

if [ -z "$admin_cmd" ]
then
  echo "Couldn't find Riak admin tool in \$PATH"
  exit 1
fi

riak_version=`awk '{print $2}' $dir/start_erl.data`
riak_status=`$admin_cmd status`
value_from_status 'riak_search_core_version'
value_from_status 'sys_system_version'
value_from_status 'ring_num_partitions'

arch=`uname -p`
os=`uname -s`
os_version=`uname -r`

if [ -z "$riak_search_core_version" ]
then
  echo "Riak version: $riak_version"
else
  echo "Riak Search version: $riak_version"
fi

echo "Riak running on: $os $os_version (arch: $arch)"
echo "Erlang runtime: ${sys_system_version}"
echo "Number of partitions: ${ring_num_partitions}"
