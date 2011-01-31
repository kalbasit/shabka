# Wifi sublet file
# Created with sur-0.1
require "socket"

# Copied from wireless.h
SIOCGIWESSID      = 0x8B1B  
IW_ESSID_MAX_SIZE = 32

configure :wifi do |s|
  s.interval = 240
  s.icon     = Subtlext::Icon.new("wifi_01.xbm")
  s.device   = "wlan0"
end

on :run do |s|
  begin
    # Get data
    wireless = IO.readlines("/proc/net/wireless", "r")

    device, link, level, noise = wireless.join.scan(/(\w*):\s*\d*\s*([0-9-]+).\s+([0-9-]+).\s+([0-9-]+)/).flatten

    # Get essid
    sock = Socket.new(Socket::AF_INET, Socket::SOCK_DGRAM, 0)

    templ   = "a16pI2"
    iwessid = [ device, " " * IW_ESSID_MAX_SIZE, IW_ESSID_MAX_SIZE, 1 ].pack(templ)

    sock.ioctl(SIOCGIWESSID, iwessid)

    interface, essid, len, flags = iwessid.unpack(templ)

    max = 100

    s.data = "%s%s (%d/%d)" % [ s.icon, essid.strip, link, max ]
  rescue => err # Sanitize to prevent unloading
    s.data = "subtle"
    p err
  end
end
