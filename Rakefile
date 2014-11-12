require 'rake'
require 'erb'

DOTFILES_PATH = File.expand_path("../", __FILE__)
IGNORED_FILES = [
  "Rakefile",
  ".git",
]

desc "install the dot files into user's home directory"
task :install => [:update_submodules, :switch_to_zsh] do
  link_folder(Dir.getwd)
end

desc "Switch your shell to ZSH from #{ENV["SHELL"]}"
task :switch_to_zsh do
  if ENV["SHELL"] =~ /zsh/
    puts "using zsh"
  else
    print "switch to zsh? (recommended) [yn] "
    case $stdin.gets.chomp
    when 'y'
      puts "switching to zsh"
      system %Q{chsh -s `which zsh`}
    else
      puts "skipping zsh"
    end
  end
end

desc "Initialize and update submodules to the latest version"
task :update_submodules do
  puts "Updating the submodules"
  `git submodule update --init > /dev/null`
end

desc "Install YouCompleteme"
task :ycm_install do
  ycm_path = File.join(DOTFILES_PATH, ".vim/bundle/YouCompleteMe")
  if !File.exist?(ycm_path)
    puts "YCM is not installed in the bundle, please run 'vim +VundleInstall +qall"
    exit
  end

  puts "Installing required packages, please enter the sudo password"
  sh "sudo apt-get install build-essential cmake python-dev"
  sh "cd #{ycm_path} && ./install.sh --clang-completer"
end

def replace_file(file)
  relative_path = file.gsub("#{DOTFILES_PATH}/", "")
  home_file = File.join(ENV['HOME'], "#{relative_path.sub(/\.erb$/, '')}")
  system %Q{rm -rf '#{home_file}'}
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
        print "overwrite ~/#{relative_path.sub(/\.erb$/, '')}? [ynaq] "
        case $stdin.gets.chomp
        when 'a'
          replace_all = true
          replace_file(file)
        when 'y'
          replace_file(file)
        when 'q'
          exit
        else
          puts "skipping ~/#{relative_path.sub(/\.erb$/, '')}"
        end
      end
    else
      if File.symlink?(file)
        link_symlink(file)
      else
        link_file(file)
      end
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

def link_symlink(link)
  begin
    File.realpath(link)
    link_file(link)
  rescue Errno::ENOENT
    puts "Skipping #{link} symlink because it points to a non-existing file."
  end
end
