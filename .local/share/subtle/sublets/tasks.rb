# Tasks sublet file
# Created with sur-0.1
configure :tasks do |s| # {{{
  s.interval = 999
  s.clients  = []

  # Get colors
  colors = Subtlext::Subtle.colors
  s.colors = {
    :active    => s.config[:color_active]    || colors[:focus_fg],
    :inactive  => s.config[:color_inactive]  || colors[:views_fg],
    :separator => s.config[:color_separator] || colors[:sublets_fg]
  }

  # Create separator
  s.separator = "%s%s" % [
    s.colors[:separator], s.config[:separator] || " | "
  ]
end # }}}

helper do |s| # {{{
  def makelist(list) # {{{
    buttons = list.map do |c|
      "%s%s" % [
        self.colors[c.has_focus? ? :active : :inactive], c.instance
      ]
    end

    self.data    = buttons.join(self.separator)
    self.clients = list
  rescue => error
    self.data = "error"
  end # }}}
end # }}}

on :tile do |s| # {{{
  s.makelist(Subtlext::Client.visible)
end # }}}

on :client_focus do |s| # {{{
  s.makelist(s.clients)
end # }}}

on :mouse_down do |s, x, y, b| # {{{
  if(1 == b and 0 < s.clients.size)
     # Stupid position check
     buttonsize = s.geometry.width / s.clients.size
     button     = (x / buttonsize).round
     s.clients[button].focus
  end
end # }}}
