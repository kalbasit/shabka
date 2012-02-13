begin
  require "rubygems"
  require "awesome_print"

  Pry.print = proc { |output, value| output.puts value.ai }
rescue LoadError
  puts "Install awesome_print to use it with pry"
end
