# Layout sublet file
# Created with sur-0.2.155
configure :layout do |s| # {{{
  s.interval = 9999
  s.current  = Subtlext::View.current.name.to_sym
  s.mode     = Hash[*Subtlext::View.all.collect { |v| [ v.name.to_sym, :grav ] }.flatten]
  s.modes    = {
    :grav   => Subtlext::Icon.new("tg.xbm"),
    :vert   => Subtlext::Icon.new("tv.xbm"),
    :horz   => Subtlext::Icon.new("th.xbm"),
    :left   => Subtlext::Icon.new("tl.xbm"),
    :right  => Subtlext::Icon.new("tr.xbm"),
    :top    => Subtlext::Icon.new("tt.xbm"),
    :bottom => Subtlext::Icon.new("tb.xbm")
  }
end # }}}

helper do |s| # {{{
  def tile
    clients = Subtlext::View[self.current].clients

    unless(clients.empty?)
      geometry = Subtlext::Screen.current.geometry
      border   = 2 #< Hardcoded

      # Tiling now
      case self.mode[self.current]
        when :vert, :horz # {{{
          g    = Subtlext::Geometry.new(geometry)
          last = clients.pop

          # Calculate width and height
          case self.mode[self.current]
            when :vert then g.height = geometry.height / (clients.size + 1)
            when :horz then g.width  = geometry.width / (clients.size + 1)
          end

          # Update clients
          clients.each do |c|
            c.resize   = false
            c.geometry = [
              g.x, g.y, g.width - 2 * border, g.height - 2 * border
            ]

            # Steps
            case self.mode[self.current]
              when :vert then g.y += g.height
              when :horz then g.x += g.width
            end
          end

          # Fix rounding
          case self.mode[self.current]
            when :vert
              g.height += geometry.height - (clients.size + 1) * g.height
            when :horz
              g.width  += geometry.width - (clients.size + 1) * g.width
          end

          last.resize   = false
          last.geometry = [
            g.x, g.y, g.width - 2 * border, g.height - 2 * border
          ]
          # }}}
        when :left, :right, :top, :bottom # {{{
          g     = Subtlext::Geometry.new(geometry)
          first = clients.shift
          size  = clients.size.zero? ? 1 : clients.size

          case self.mode[self.current]
            when :left
              g.width = geometry.width / 2
              g.x     = geometry.x + g.width
              fix     = geometry.width - 2 * g.width #< Fix rounding
            when :right
              g.x     = geometry.x
              g.width = geometry.width / 2
              fix     = geometry.width - 2 * g.width #< Fix rounding
            when :top
              g.height = geometry.height / 2
              g.y      = geometry.y + g.height
              fix      = geometry.height - 2 * g.height #< Fix rounding
            when :bottom
              g.y      = geometry.y
              g.height = geometry.height / 2
              fix      = geometry.height - 2 * g.height #< Fix rounding
          end

          # Set first client
          first.resize   = false
          first.geometry = [
            g.x, g.y, g.width - 2 * border, g.height - 2 * border
          ]

          case self.mode[self.current]
            when :left
              g.x      = geometry.x
              g.height = geometry.height / size
              fix      = geometry.height - size * g.height #< Fix rounding
            when :right
              g.x      = geometry.x + g.width
              g.height = geometry.height / size
              fix      = geometry.height - size * g.height #< Fix rounding
            when :top
              g.y     = geometry.y
              g.width = geometry.width / size
              fix     = geometry.width - size * g.width #< Fix rounding
            when :bottom
              g.y     = geometry.y + g.height
              g.width = geometry.width / size
              fix     = geometry.width - size * g.width #< Fix rounding
          end

          # Update clients
          clients.each do |c|
            c.resize   = false
            c.geometry = [
              g.x, g.y, g.width - 2 * border, g.height - 2 * border
            ]

            if(:left == self.mode[self.current] or
                :right == self.mode[self.current])
              g.y += g.height
            else
              g.x += g.width
            end
          end # }}}
      end
    end

    self.data = self.modes[self.mode[self.current]].to_s
  end
end # }}}

on :run do |s| # {{{
  s.data = s.modes[:grav].to_s
end # }}}

on :mouse_down do |s, x, y, b| # {{{
  # Switch mode
  s.mode[s.current] = case s.mode[s.current]
    when :grav   then :vert
    when :vert   then :horz
    when :horz   then :left
    when :left   then :right
    when :right  then :top
    when :top    then :bottom
    when :bottom then :grav
  end

  tile
end # }}}

on :tile do |s| # {{{
  tile
end # }}}

on :view_jump do |s, v| # {{{
  s.current = v.name.to_sym
  s.data    = s.modes[s.mode[s.current]].to_s
end # }}}

on :view_create do |s, v| # {{{
  s.mode[v.name.to_sym] = :grav
end # }}}

on :view_kill do |s, v| # {{{
  s.mode.delete(v.name.to_sym)
end # }}}
