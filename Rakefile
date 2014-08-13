require 'rake'
require 'erb'

DOTFILES_PATH = File.expand_path("../", __FILE__)
IGNORED_FILES = [
  "Rakefile",
  ".git",
]

desc "install the dot files into user's home directory"
task :install do
  update_submodules
  switch_to_zsh
  link_folder(Dir.getwd)
end

def replace_file(file)
  system %Q{rm -rf "$HOME/#{file.sub(/\.erb$/, '')}"}
  link_file(file)
end

def link_folder(folder)
  replace_all = false

  files = Dir["#{folder}/.??*"] + Dir["#{folder}/*"]
  files.reject! do |file|
    relative_path = file.gsub("#{DOTFILES_PATH}/", "")
    IGNORED_FILES.include?(relative_path)
  end

  files.each do |file|
    relative_path = file.gsub("#{DOTFILES_PATH}/", "")
    home_file = File.join(ENV['HOME'], "#{relative_path.sub(/\.erb$/, '')}")

    if File.exist?(home_file)
      if File.identical? file, home_file
        puts "identical ~/#{relative_path.sub(/\.erb$/, '')}"
      elsif File.directory?(home_file)
        link_folder(file)
      elsif replace_all
        replace_file(file)
      else
        print "overwrite ~/#{file.sub(/\.erb$/, '')}? [ynaq] "
        case $stdin.gets.chomp
        when 'a'
          replace_all = true
          replace_file(file)
        when 'y'
          replace_file(file)
        when 'q'
          exit
        else
          puts "skipping ~/#{file.sub(/\.erb$/, '')}"
        end
      end
    else
      link_file(file)
    end
  end
end

def link_file(file)
  relative_path = file.gsub("#{DOTFILES_PATH}/", "")
  home_file = File.join(ENV['HOME'], "#{relative_path.sub(/\.erb$/, '')}")

  if file =~ /.erb$/
    puts "generating #{home_file}"
    File.open(home_file, 'w') do |new_file|
      new_file.write ERB.new(File.read(file)).result(binding)
    end
  else
    puts "linking #{file}"
    system %Q{ln -s "#{file}" "#{home_file}"}
  end
end

def switch_to_zsh
  if ENV["SHELL"] =~ /zsh/
    puts "using zsh"
  else
    print "switch to zsh? (recommended) [ynq] "
    case $stdin.gets.chomp
    when 'y'
      puts "switching to zsh"
      system %Q{chsh -s `which zsh`}
    when 'q'
      exit
    else
      puts "skipping zsh"
    end
  end
end

def update_submodules
  puts "Updating the submodules"
  `git submodule update --init > /dev/null`
end
