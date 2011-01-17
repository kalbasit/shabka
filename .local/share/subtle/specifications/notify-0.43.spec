# Notify specification file
# Created with sur-0.2.168
Sur::Specification.new do |s|
  s.name             = "Notify"
  s.authors          = [ "unexist" ]
  s.date             = "Fri Sep 17 14:12 CET 2010"
  s.contact          = "unexist@dorfelite.net"
  s.description      = "Display libnotify messages"
  s.notes            = <<NOTES
This sublet replaces the current libnotify handler, it collects messages and
basically consists of an icon and a message window. Once a message is received
the icon changes it's color to the focus color. By pointing the mouse on the
sublet, the message window will be visible beneath the sublet and the messages
can be read. When the mouse leaves the sublet the message window disappears
again and all messages are discarded.

(Max. length of a message is 50 characters)
NOTES
  s.version          = "0.43"
  s.tags             = [ "FFI", "DBus", "Libnotify", "Icon", "Window" ]
  s.files            = [ "notify.rb" ]
  s.icons            = [ "icons/info.xbm" ]
  s.required_version = "0.9.2127"

  # Gem requirements
  s.add_dependency("ffi", ">=0.5.4")
end
