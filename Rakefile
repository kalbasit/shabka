$: << File.expand_path('../support/lib', __FILE__)
require 'dot_file'

# Monkey path so we can test it first
class DotFile
  def link_dotfile path
    puts "Linking #{source_path}/#{path} to #{destination_path}/#{path}"
  end
end

desc "Link dotfile to the home folder"
task :link_dotfiles do
  DotFile.new(File.expand_path('../', __FILE__), ENV['HOME']).link_dotfiles
end

task :default => :link_dotfiles
