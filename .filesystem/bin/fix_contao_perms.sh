#!/usr/bin/env bash

set -ex

cd $(git rev-parse --show-toplevel)
#rm -f public/system/config/localconfig.php
#ln -s ~/Code/Agence_Durable/localconfig.php public/system/config/localconfig.php
cp public/.htaccess.default public/.htaccess
mkdir -p public/system/logs
chmod 777 public/system/{html,logs,scripts,tmp,cache} 2>/dev/null
touch public/sitemap.xml
chmod 666 public/sitemap.xml
find public/system/modules/efg -type f -exec chmod 666 {} ';'
find public/system/modules/efg -type d -exec chmod 777 {} ';'
