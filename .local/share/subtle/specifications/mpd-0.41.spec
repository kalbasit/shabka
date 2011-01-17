# Mpd specification file
# Created with sur-0.1
Sur::Specification.new do |s|
  s.name        = "Mpd"
  s.authors     = [ "Christoph Kappel" ]
  s.date        = "Sat Feb 27 03:05 CET 2010"
  s.contact     = "unexist@dorfelite.net"
  s.description = "Show the current track and controls of mpd"
  s.version     = "0.41"
  s.tags        = [ "Watch" ]
  s.files       = [ "mpd.rb" ]
  s.icons       = [
    "icons/play.xbm",
    "icons/pause.xbm",
    "icons/stop.xbm",
    "icons/prev.xbm",
    "icons/next.xbm",
    "icons/note.xbm"
  ]
end
