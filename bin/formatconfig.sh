#!/bin/sh

PATH=.:$PATH

pathconv()
{
  FROM=$(grep "{$1" ejabberd.yml | head -n 1 | cut -d'"' -f$2)
  FROM=${FROM%/*}/
  TO=$(echo $FROM | sed -e 's/\//\\/g' -e 's/\\/\\\\\\\\/g')
  FROM=$(echo $FROM | sed -e 's/\\/\\\\/g' -e 's/\//\\\//g')
  sed -e "s/$FROM/$TO/" ejabberd.yml > conftmp
  mv -f conftmp ejabberd.yml
}

mv ../conf/ejabberd.yml .
pathconv certfile 2
pathconv domain_certfile 4
pathconv extauth 2
pathconv docroot 2
mv -f ejabberd.yml ../conf/ejabberd.yml
