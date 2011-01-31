# Battery specification file
# Created with sur-0.1
Sur::Specification.new do |s|
  s.name        = "Battery"
  s.authors     = [ "Christoph Kappel" ]
  s.date        = "Wed Mar 24 21:42 CET 2020"
  s.contact     = "unexist@dorfelite.net"
  s.description = "Show the battery state"
  s.notes       = <<NOTES
This sublet displays the remaining battery power (percent) and the
state of the power adapter. (icon)
NOTES
  s.config      = [
    { :name => "path", :type => "string", :description => "Path of the battery" }
  ]
  s.version     = "0.6"
  s.tags        = [ "Sys", "Icon", "Config" ]
  s.files       = [ "battery.rb" ]
  s.icons       = [
    "icons/ac.xbm",
    "icons/bat_full_02.xbm",
    "icons/bat_low_02.xbm",
    "icons/bat_empty_02.xbm"
  ]
end
