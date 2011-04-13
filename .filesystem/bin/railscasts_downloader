#!/usr/bin/env ruby
# -*- ruby -*-
#--
#  vim:ft=ruby:fenc=UTF-8:ts=2:sts=2:sw=2:expandtab
#
#  Copyright 2010 Wael Nasreddine (TechnoGate) <wael.nasreddine@technogate.fr>
#  Copyright 2009 Maximiliano Guzman (https://github.com/maximilianoguzman)
#
#  This file is part of Cardslib
#
#  Cardslib is free software: you can redistribute it and/or modify it under the
#  terms of the GNU General Public License as published by the Free Software
#  Foundation, either version 3 of the License, or (at your option) any later
#  version.
#
#  Cardslib is distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
#  PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License along with
#  Cardslib. If not, see http://www.gnu.org/licenses/
#++
####

require 'rss'
require 'tempfile'

def execute_command(command)
  progress = nil
  IO.popen(command) do |pipe|
    pipe.each("r") do |line|
      print line
    end
  end
end

p 'Downloading rss index'

# Download the RSS Feed and parse video urls
rss_string = open('http://feeds.feedburner.com/railscasts').read
rss = RSS::Parser.parse(rss_string, false)
videos_urls = rss.items.map { |it| it.enclosure.url }.reverse

# We only need to download what's missing, in progress files are considered missing as well
videos_filenames = videos_urls.map {|url| url.split('/').last }
inprogress_filenames = Dir.glob('*.mov.aria2').map { |f| f.gsub(/\.aria2$/, '') }
existing_filenames = Dir.glob('*.mov') - inprogress_filenames
missing_filenames = videos_filenames - existing_filenames

# Generate a hash of videos urls from filenames (missing filesnames that is)
missing_videos_urls = videos_urls.select { |video_url| missing_filenames.any? { |filename| video_url.match filename } }

if missing_videos_urls.size > 0
  p "Downloading #{missing_filenames.size} missing videos"

  # Add all urls to a temporary file
  missing_videos_file = Tempfile.new('missing_railscasts_files')
  missing_videos_urls.each do |video_url|
    missing_videos_file << video_url + "\n"
  end
  missing_videos_file.flush

  # Run aria2c
  execute_command("/usr/bin/aria2c --ftp-pasv --continue --max-tries=3 --split=5 --input-file=#{missing_videos_file.path}")
end

# Cleanup and exit
p 'Finished synchronization'