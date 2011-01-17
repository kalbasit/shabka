# Mpd_notify sublet file
# Created with sur-0.1.113

require 'librmpd'

configure :mpd_notify do |s|
  s.file = "/var/lib/mpd/mpdstate"
  s.watch s.file

  s.mpd = MPD.new 'localhost', 6600
  s.mpd.connect
end

on :run do |s|
  status = mpd.status
  begin
    if status['state'] == "play" or status['state'] == "pause"
      song = mpd.song_with_id status['song']
      s.data = song.artist + " - " + song.title
    else
      s.data = ""
    end
  rescue RuntimeError => err
    self.data = "Error: #{err}"
  end
end
