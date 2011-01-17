# Clock sublet file
# Created with sur-0.1
configure :clock do |s| # {{{
  s.interval = s.config[:interval]      || 60
  s.format   = s.config[:format_string] || "%y/%m/%d %H:%M"
  s.icon     = Subtlext::Icon.new("clock.xbm")
end # }}}

on :run do |s| # {{{
  s.data = s.icon + Time.now().strftime(s.format)
end # }}}
