# Volume specification file
# Created with sur-0.2.143
Sur::Specification.new do |s|
  s.name        = "Volume"
  s.authors     = [ "unexist" ]
  s.date        = "Fri Apr 2 22:40 CEST 2010"
  s.contact     = "unexist@dorfelite.net"
  s.description = "Control the volume with mouse wheel"
  s.notes       = <<NOTES
This sublet shows the volume of the default mixer device, this works
independently of the chosen sound system.

Left click toggles mute, mouse wheel up and down changes the volume.
NOTES
  s.version     = "0.2"
  s.tags        = [ "Icon", "Ioctl", "Linux", "Config" ]
  s.files       = [ "volume.rb" ]
  s.icons       = [
    "icons/spkr_01.xbm",
    "icons/spkr_02.xbm"
  ]

  # Need specific version
  s.required_version = "0.9.1856"
end
