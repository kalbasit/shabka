# Clock specification file
# Created with sur-0.1
Sur::Specification.new do |s|
  s.name        = "Clock"
  s.authors     = [ "Christoph Kappel" ]
  s.date        = "Sat Sep 13 19:00 CET 2008"
  s.contact     = "unexist@dorfelite.net"
  s.description = "Show the clock and date"
  s.notes       = <<NOTES
This sublet is just a simple clock.
NOTES
  s.config      = [
    { :name => "format_string", :type => "string",  :description => "Format of the clock (man date)" },
  ]
  s.version     = "0.33"
  s.tags        = [ "Icon", "Config" ]
  s.files       = [ "clock.rb" ]
  s.icons       = [ "icons/clock.xbm" ]
end
