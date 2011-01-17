# Temp sublet file
# Created with sur-0.1
configure :temp do |s|
  s.interval = 60
  s.temp     = ""
  s.icon     = Subtlext::Icon.new("temp.xbm")

  begin
    s.path = Dir["/proc/acpi/thermal_zone/*"][0] #< Get temp slot
  rescue => err
    err
  end
end

on :run do |s|
  begin
    file = ""

    # Read temp state file
    File.open(s.path + "/temperature", "r") do |f|
      file = f.read
    end

    s.temp = file.match(/temperature:\s+(\d+)/).captures.first

    s.data = s.icon + s.temp.to_s + "C"
  rescue => err # Sanitize to prevent unloading
    s.data = "subtle"
    p err
  end
end
