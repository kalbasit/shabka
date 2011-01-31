# Tasks specification file
# Created with sur-0.1
Sur::Specification.new do |s|
  s.name        = "Tasks"
  s.authors     = [ "Christoph Kappel" ]
  s.date        = "Tue Dec 1 23:00 CET 2009"
  s.contact     = "unexist@dorfelite.net"
  s.description = "Show visible clients"
  s.notes       = <<NOTES
This sublet displays all visible clients in a tasklist like fashion. Current
client is highlighted and a click on a client name will focus it.
NOTES
  s.config      = [
    { :name => "color_active",    :type => "string",  :description => "Color of active window (#rrggbb)"    },
    { :name => "color_inactive",  :type => "string",  :description => "Color of inactive window (#rrggbb)"  },
    { :name => "color_separator", :type => "string",  :description => "Color of separator (#rrggbb)"        },
    { :name => "separator",       :type => "string",  :description => "Separator between windows"           },
    { :name => "modes",           :type => "bool",    :description => "Display client modes like float"     }
  ]
  s.version     = "0.33"
  s.tags        = [ "Mouse", "Config" ]
  s.files       = [ "tasks.rb" ]
  s.icons       = [ ]

  # Version requirements
  s.required_version = "0.9.2500"
end
