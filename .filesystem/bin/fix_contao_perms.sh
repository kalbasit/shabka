#!/usr/bin/env bash

cd $(git rev-parse --show-toplevel)
mkdir -p public/system/logs
chmod 777 public/system/{html,logs,scripts,tmp}
touch public/sitemap.xml
chmod 666 public/sitemap.xml
