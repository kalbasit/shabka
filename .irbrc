def load_factories
  require 'faker'
  Factory.factories.clear
  Dir.glob("#{Rails.root}/spec/support/factories/*.rb") {|f| load f}
end

def my_commands
  puts Readline::HISTORY.entries.split("exit").last[0..-2].join("\n")
end

begin
  require "rubygems"
  require "awesome_print"

  unless IRB.version.include?('DietRB')
    IRB::Irb.class_eval do
      def output_value
        ap @context.last_value
      end
    end
  else # MacRuby
    IRB.formatter = Class.new(IRB::Formatter) do
      def inspect_object(object)
        object.ai
      end
    end.new
  end
rescue LoadError
  puts "Install awesome_print to use it with pry"
end
