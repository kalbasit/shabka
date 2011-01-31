#
# This program can be distributed under the terms of the GNU GPL.
# See the file COPYING.
#
# $Id: .config/subtle/subtle.rb,v 298 2010/12/24 14:47:39 unexist $
#

require "socket"

begin
  require "#{ENV["HOME"]}/Projects/TechnoGate/subtle-contrib/ruby/launcher.rb"
rescue LoadError => e
  puts e
end

# Options {{{
set :border,     2
set :step,       5
set :snap,       10
set :gravity,    :center
set :urgent,     false
set :resize,     false
set :strut,      [0, 0, 0, 0]
set :padding,    [4, 4, 2, 2]
#set :font,       "xft:Envy Code R:pixelsize=13"
set :font,       "xft:DejaVu Sans Mono:pixelsize=12:antialias=true"
#set :font,       "xft:Ubuntu R:pixelsize=13"
set :separator,  "_"
set :outline,    0
set :gap,        0
# }}}

# Screens {{{
screen 1 do
  stipple false
  top     [:tray, :seperator, :views, :spacer, :battery, :clock]
  bottom  [:sublets]
  view    0
end

screen 2 do
  stipple false
  top     [:views, :spacer, :title, :center, :mpd, :separator, :volume, :center]
  bottom  []
  view    0
end
# }}}

# Colors {{{
color :title_fg,          "#f0dfaf"
color :title_bg,          "#1e2320"
color :title_border,      "#3f3f3f"

color :focus_bg,          "#1e2320"
color :focus_border,      "#3f3f3f"
color :focus_fg,          "#f0dfaf"

color :urgent_fg,         "#cc9393"
color :urgent_bg,         "#3f3f3f"
color :urgent_border,     "#3f3f3f"

color :occupied_fg,       "#cc9393"
color :occupied_border,   "#3f3f3f"
color :occupied_bg,       "#3f3f3f"

color :views_border,      "#3f3f3f"
color :views_bg,          "#3f3f3f"
color :views_fg,          "#dcdccc"

color :sublets_bg,        "#3f3f3f"
color :sublets_border,    "#3f3f3f"
color :sublets_fg,        "#dcdccc"

color :client_inactive,   "#3f3f3f"
color :client_active,     "#6f6f6f"

color :panel,             "#3f3f3f"

color :background,        "#3d3d3d"

color :stipple,           "#dcdccc"

color :separator,         "#dcdccc"
# }}}

# Gravities {{{
gravity :top_left,       [0, 0, 50, 50]
gravity :top_left33,     [0, 0, 50, 33]
gravity :top_left66,     [0, 0, 50, 66]
gravity :top_left75,     [0, 0, 50, 75]
gravity :top,            [0, 0, 100, 50]
gravity :top66,          [0, 0, 100, 67]
gravity :top33,          [0, 0, 100, 33]
gravity :top75,          [0, 0, 100, 75]
gravity :top_right,      [100, 0, 50, 50]
gravity :top_right33,    [100, 0, 50, 33]
gravity :top_right66,    [100, 0, 50, 66]
gravity :top_right75,    [100, 0, 50, 75]
gravity :left,           [0, 0, 50, 100]
gravity :left33,         [0, 50, 25, 33]
gravity :left66,         [0, 50, 50, 33]
gravity :center,         [0, 0, 100, 100]
gravity :center33,       [50, 50, 50, 33]
gravity :center66,       [0, 50, 100, 33]
gravity :right,          [100, 0, 50, 100]
gravity :right33,        [100, 50, 25, 100]
gravity :right66,        [100, 50, 50, 33]
gravity :bottom_left,    [0, 100, 50, 50]
gravity :bottom_left25,  [0, 100, 50, 25]
gravity :bottom_left33,  [0, 100, 50, 33]
gravity :bottom_left66,  [0, 100, 50, 66]
gravity :bottom,         [0, 100, 100, 50]
gravity :bottom66,       [0, 100, 100, 66]
gravity :bottom33,       [0, 100, 100, 33]
gravity :bottom_right,   [100, 100, 50, 50]
gravity :bottom_right25, [100, 100, 50, 25]
gravity :bottom_right33, [100, 100, 50, 33]
gravity :bottom_right66, [100, 100, 50, 66]
gravity :gimp_image,     [50, 50, 80, 100]
gravity :gimp_toolbox,   [0, 0, 10, 100]
gravity :gimp_dock,      [100, 0, 10, 100]
gravity :dia_toolbox,    [0, 0, 100, 15]
gravity :dia_window,     [0, 18, 100, 84]
# }}}

# Grabs {{{
# Host specific
host     = Socket.gethostname
modkey   = "W"
gravkeys = [ "KP_7", "KP_8", "KP_9", "KP_4", "KP_5", "KP_6", "KP_1", "KP_2", "KP_3" ]

# Views and screens
(1..6).each do |i|
  grab modkey + "-#{i}",   "ViewSwitch#{i}".to_sym
  grab modkey + "-S-#{i}", "ViewJump#{i}".to_sym
  #grab modkey + "-F#{i}",  "ScreenJump#{i}".to_sym
end

# Windows
grab modkey + "-B1",      :WindowMove
grab modkey + "-B2",      :WindowResize
grab modkey + "-S-f",     :WindowFloat
grab modkey + "-S-space", :WindowFull
grab modkey + "-S-s",     :WindowStick
grab modkey + "-r",       :WindowRaise
grab modkey + "-l",       :WindowLower
grab modkey + "-Left",    :WindowLeft
grab modkey + "-Down",    :WindowDown
grab modkey + "-Up",      :WindowUp
grab modkey + "-Right",   :WindowRight
grab modkey + "-k",       :WindowKill
grab modkey + "-B3",      :WindowResize
grab modkey + "-h", lambda { |c| c.retag }

# Reload/restart
grab modkey + "-C-q",     :SubtleQuit
grab modkey + "-C-r",     :SubtleReload
grab modkey + "-C-A-r",   :SubtleRestart

# Gravity keys and focus
gravities = [
  [:top_left, :top_left33, :top_left66, :top_left75],
  [:top, :top33, :top66, :top75],
  [:top_right, :top_right33, :top_right66, :top_right75],
  [:left, :left33, :left66],
  [:center, :center33, :center66],
  [:right, :right33, :right66],
  [:bottom_left, :bottom_left25, :bottom_left33, :bottom_left66],
  [:bottom, :bottom33, :bottom66],
  [:bottom_right, :bottom_right25, :bottom_right33, :bottom_right66]
]

gravities.each_index do |i|
  grab "%s-%s" % [ modkey, gravkeys[i] ], gravities[i]

  grab "%s-C-%s" % [ modkey, gravkeys[i] ], lambda {
    c = Subtlext::Client.visible.select { |c|
      gravities[i].include?(c.gravity.name.to_sym)
    }

    c.first.focus unless(c.empty?)
  }
end

# Multimedia keys
grab "XF86AudioMute", "amixer -c 0 set Master toggle"
grab "XF86AudioRaiseVolume", "amixer -c 0 set Master 2dB+"
grab "XF86AudioLowerVolume", "amixer -c 0 set Master 2dB-"
grab "XF86AudioPlay", "mpc toggle"
grab "XF86AudioStop", "mpc stop"
grab "XF86AudioPrev", "mpc prev"
grab "XF86AudioNext", "mpc next"
grab modkey + "-m", "mpc current | tr -d '\n' | xclip"

# Programs
grab modkey + "-Return", "urxvt"
grab modkey + "-g", "gvim"
grab modkey + "-c", "google-chrome"
grab modkey + "-f", "firefox"
grab modkey + "-v", "vlc"

# Clients
grab "A-Tab" do
  clients = Subtlext::View.current.clients
  selected = clients.select { |c| c.has_focus? }.first

  index = clients.index(selected)

  index += 1
  index = 0 if index > clients.size - 1

  clients[index].focus
end

grab "A-F4" do
  clients = Subtlext::View.current.clients
  selected = clients.select { |c| c.has_focus? }.first

  index = clients.index(selected)

  clients[index].kill
end

# System
grab "XF86Sleep" do
  system "xlock &"
  system "sudo pm-suspend &"
end

grab "XF86ScreenSaver" do
  system "xlock &"
end

# Launcher
grab modkey + "-x" do
  Subtle::Contrib::Launcher.run
end
# }}}

# Tags {{{
tag "terms" do
  match    "xterm|urxvt"
  gravity  :center
end

tag "browser" do
  match "(google\-)?chrom[e|ium]"
  gravity :center
end

tag "browser_dev" do
  match "navigator"
  gravity :center
end

tag "pdf" do
  match    "apvlv|evince"
  stick    false
  float    false
end

tag "editor" do
  match   "[g]?vim|scribes"
  resize  false
  float   false
  gravity :center
end

tag "xephyr" do
  match    "xephyr"
  urgent   false
  geometry [857, 96, 800, 800]
end

tag "android" do
  match    :name => "5554:AVD"
  geometry [ 873, 47, 791, 534 ]
end

tag "mplayer" do
  match   "mplayer"
  float   true
  stick   true
  urgent  true
  #full    true
end

tag "vlc" do
  match   "vlc"
  float   true
  stick   true
  urgent  false
end

tag "stick" do
  match  "dialog|subtly|python|gtk.rb|display|pychrom|xev"
  stick  true
  float  true
end

tag "urgent" do
  match  "sun-awt-X11-XDialogPeer"
  type   :dialog
  stick  true
  urgent true
end

tag "im" do
  match  "pidgin|skype"
  float  true
  urgent true
end

tag "void" do
  match   "jd-Main|Virtualbox"
end

tag "dialogs" do
  match :type => :dialog
  match :type => :splash
  stick true
end

tag "flash" do
  match "exe|<unknown>"
  stick true
end

tag "one" do
  match    "urxvt2"
  gravity  :bottom_left
end

tag "one25" do
  match    "urxvt2"
  gravity  :bottom_left25
end

tag "two" do
  match    "urxvt2"
  gravity  :bottom
end

tag "three25" do
  match    "urxvt1"
  gravity  :bottom_right25
end

tag "seven" do
  match    "urxvt1"
  gravity  :top_left
end

tag "eight" do
  match    "urxvt1"
  gravity  :top
end

tag "gimp_image" do
  match    :role => "gimp-image-window"
  gravity  :gimp_image
end

tag "gimp_toolbox" do
  match    :role => "gimp-toolbox$"
  gravity  :gimp_toolbox
end

tag "gimp_dock" do
  match    :role => "gimp-dock"
  gravity  :gimp_dock
end

tag "gimp_scum" do
  match :role => "gimp-.*|screenshot"
end

tag "dia_window" do
  match   :role => "diagram_window"
  gravity :dia_window
end

tag "dia_toolbox" do
  match   :role => "toolbox_window"
  gravity :dia_toolbox
end

tag "inkscape" do
  match "inkscape"
end

tag "xfontsel" do
  match    "xfontsel"
  geometry [464, 433, 676, 113]
  stick    true
end

tag "xev" do
  match    :name => "Event[ ]Tester"
  geometry [1213, 98, 377, 321]
  float    true
  stick    true
end
# }}}

# Views {{{
www_re    = "^browser$"
test_re   = "browser_dev|android|xephyr|seven$|one$"
im_re     = "im"
editor_re = "editor"
icons     = false

iconpath = "#{ENV["HOME"]}/.local/share/icons"

view "terms" do
  match     "terms|eight|two"
  icon      "#{iconpath}/terminal.xbm"
  icon_only icons
end

view "www" do
  match     www_re
  icon      "#{iconpath}/world.xbm"
  icon_only icons
end

view "void" do
  match     "default|void"
  icon      "#{iconpath}/quote.xbm"
  icon_only icons
end

view "im" do
  match     im_re
  icon      "#{iconpath}/chat.xbm"
  icon_only icons
end

view "sketch" do
  match     "inkscape|dia_*|gimp_.*"
  icon      "#{iconpath}/paint.xbm"
  icon_only icons
end

view "test" do
  match     test_re
  icon      "#{iconpath}/bug.xbm"
  icon_only icons
end

view "editor" do
  match     editor_re
  icon      "#{iconpath}/pencil.xbm"
  icon_only icons
end
# }}}

# Hooks {{{
on :start do
  system "nm-applet --sm-disable &"
  system "urxvt -e /bin/zsh -c 'Screen Default' &"
  system "dropbox start"
  system "killall gnome-screensaver"
end
# }}}