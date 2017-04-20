// FindProxyForURL returns DIRECT which affectedly disables proxy
function FindProxyForURL(url, host) {
  return "DIRECT";
}
