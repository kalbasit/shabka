#!/usr/bin/env bash

mkdir -p /Library/Application\ Support/TextMate/

sudo chown -R $(whoami) /Library/Application\ Support/TextMate

cd /Library/Application\ Support/TextMate/

if [[ -d Bundles/.svn ]] ; then
  cd Bundles && svn up
else
  if [[ -d Bundles ]] ; then
    mv Bundles Bundles.old
  fi
  svn co http://svn.textmate.org/trunk/Bundles
fi

exit 0