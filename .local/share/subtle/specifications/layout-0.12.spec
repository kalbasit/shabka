# -*- encoding: utf-8 -*-
# Layout specification file
# Created with sur-0.2.155
Sur::Specification.new do |s|
  s.name        = "Layout"
  s.authors     = [ "unexist" ]
  s.date        = "Sun Aug 22 15:49 CEST 2010"
  s.contact     = "unexist@dorfelite.net"
  s.description = "Sublet for view based tiling layouts"
  s.notes       = <<NOTES
This sublet adds common automatic tiling capabilites to subtle for people who
cannot live without. All tiling layouts work view wise and can be changed
independently. It basically consists of an icon that shows a graphic of the
current active layout and also works as button to toggle through all of them.

Following layouts are available in given order:
  gravity    = No automatic tiling (default)
  vertical   = Vertical tiling
  horizontal = Horizontal tiling
  left       = Tile left, master right
  right      = Tile right, master left
  top        = Tile top, master bottom
  bottom     = Tile bottom, master top
NOTES
  s.version     = "0.12"
  s.tags        = [ "Layout", "Tiling", "Automatic", "Icon" ]
  s.files       = [ "layout.rb" ]
  s.icons       = [
    "icons/tb.xbm",
    "icons/tf.xbm",
    "icons/tg.xbm",
    "icons/th.xbm",
    "icons/tl.xbm",
    "icons/tr.xbm",
    "icons/tt.xbm",
    "icons/tv.xbm"
  ]

  # Need specific version
  s.required_version = "0.9.2565"
end
