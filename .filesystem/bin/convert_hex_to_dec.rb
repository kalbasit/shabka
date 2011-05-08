#!/usr/bin/ruby

file = File.new(ARGV[0], "r")
while (line = file.gets)
  i = line.index "0x"
  while (i)
    s = i
    hex = ''
    c = line[s,1]
    while "0x123456789abcdef".index(c)
      hex += c
      s = s + 1
      c = line[s,1]
    end
    dec = Integer(hex)
    newline = line[0,i] + dec.to_s + line[s,line.length]
    line = newline
    i = line.index "0x"
  end
  puts line
end
file.close
