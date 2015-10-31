# TODO: WOW! Refactor!!

require 'rake'
require 'erb'
require 'open-uri'

DOTFILES_PATH = File.expand_path("../", __FILE__)
PRIVATE_PATH = File.expand_path(File.join(DOTFILES_PATH, ".private"))
IGNORED_FILES = [
  "Brewfile",
  "Rakefile",
  "README.md",
  ".DS_Store",
  ".etc",
  ".git",
  ".gitignore",
  ".private",
  ".osx",
]

GO_BINARIES = [
  "github.com/monochromegane/the_platinum_searcher/cmd/pt",
  "github.com/smartystreets/goconvey",
  "github.com/jteeuwen/go-bindata/go-bindata",
  "gopkg.in/totp.v0/cmd/totp",
  "github.com/tools/godep",
  "golang.org/x/tools/cmd/cover",
  "github.com/golang/lint/golint",
  "github.com/codegangsta/gin",
  "github.com/peco/peco/cmd/peco",
  "golang.org/x/tools/cmd/stringer",
  "github.com/golang/protobuf/protoc-gen-go",
  "github.com/Masterminds/glide",
  "github.com/kardianos/govendor",
]

desc "Initialize"
task :init, [:osx, :default, :vim_plug, :install_go_binaries]

desc "Initialize the Mac"
task :osx do
  sh %Q{#{DOTFILES_PATH}/.osx} if (/darwin/ =~ RUBY_PLATFORM) != nil
end

desc "Run brew bundle"
task :brew_bundle do
  sh %Q{brew bundle}
end

desc "Install all vim plugins"
task :vim_plug do
  sh %Q{vim +PlugInstall +qall}
end

desc "Install Go Binaries"
task :install_go_binaries do
  GO_BINARIES.each do |binary|
    sh %Q{go get -u #{binary}}
  end
end

desc "link files from dotfiles"
task :link_dotfiles do
  link_folder(DOTFILES_PATH, ENV["HOME"])
end

desc "link files from the private repository"
task :link_private do
  link_folder(PRIVATE_PATH, ENV["HOME"]) if PRIVATE_PATH.exists?
end

task :default => [:update_submodules, :switch_to_zsh, :link_dotfiles, :link_private]

desc "Switch your shell to ZSH from #{ENV["SHELL"]}"
task :switch_to_zsh do
  if ENV["SHELL"] =~ /zsh/
    puts "using zsh"
  else
    print "switch to zsh? (recommended) [yn] "
    case $stdin.gets.chomp
    when 'y'
      puts "switching to zsh"
      sh %Q{chsh -s `which zsh`}
    else
      puts "skipping zsh"
    end
  end
end

desc "Initialize and update submodules to the latest version"
task :update_submodules do
  puts "Updating the submodules"
  sh %Q{git submodule update --init > /dev/null}
end

desc "Update the CA bundler cert"
task :update_ca_bundle_cert do
  url = "https://raw.githubusercontent.com/bagder/ca-bundle/master/ca-bundle.crt"
  path = File.expand_path(File.join(File.dirname(__FILE__), ".ca-bundle.crt"))
  download_and_save_file url, path
end

def relative_path(file)
  return file.gsub("#{PRIVATE_PATH}/", "").gsub("#{DOTFILES_PATH}/", "")
end

def replace_file(file, dest)
  target_file = File.join(dest, dest_filename(relative_path(file)))
  sh %Q{rm -rf '#{target_file}'}
  link_file(file, dest)
end

def files(folder)
  files = Dir["#{folder}/.??*"] + Dir["#{folder}/*"]
  files.reject! do |file|
    IGNORED_FILES.include?(relative_path(file))
  end

  files.reject! {|f| f =~ /\.unsecure$/}

  return files
end

def find_encryption_status
  return false if !File.exists?(File.join(PRIVATE_PATH, ".encrypted"))

  st = `cd #{DOTFILES_PATH}; grep -q OK .private/.encrypted && echo OK || echo NO`.chomp
  if st == "OK"
    return true
  else
    return false
  end
end

def link_folder(folder, dest)
  replace_all = false

  files(folder).each do |file|
    # Take care of an encrypted file if the encryption was not setup.
    if !find_encryption_status() && File.exists?("#{file}.unsecure")
      # Check if we have an unsecure version
      # We do, then symlink the unsecure file instead.
      file = "#{file}.unsecure"
    end

    target_file = File.join(dest, dest_filename(relative_path(file)))

    if File.exist?(target_file)
      if File.identical? file, target_file
        puts "identical ~/#{dest_filename(relative_path(file))}"
      elsif file =~ /\.erb$/
        replace_file(file, dest)
      elsif File.directory?(target_file)
        link_folder(file, dest)
      elsif replace_all
        replace_file(file, dest)
      else
        print "overwrite ~/#{dest_filename(relative_path(file))}? [ynaq] "
        case $stdin.gets.chomp
        when 'a'
          replace_all = true
          replace_file(file, dest)
        when 'y'
          replace_file(file, dest)
        when 'q'
          exit
        else
          puts "skipping ~/#{dest_filename(relative_path(file))}"
        end
      end
    else
      if File.symlink?(file)
        link_symlink(file, dest)
      else
        link_file(file, dest)
      end
    end
  end
end

def dest_filename(path)
  return path.sub(/\.erb$/, '').sub(/\.unsecure$/, '')
end

def link_file(file, dest)
  target_file = File.join(dest, dest_filename(relative_path(file)))

  if file =~ /.erb$/
    puts "generating #{target_file}"
    File.open(target_file, 'w') do |new_file|
      new_file.write ERB.new(File.read(file)).result(binding)
    end
    if is_encrypted?(file)
      File.chmod(0400, target_file)
    end
  else
    puts "linking #{file}"
    sh %Q{ln -s "#{file}" "#{target_file}"}
  end
end

def is_encrypted?(file)
  file =~ /#{PRIVATE_PATH}/
end

def link_symlink(link, dest)
  begin
    File.realpath(link)
    link_file(link, dest)
  rescue Errno::ENOENT
    puts "Skipping #{link} symlink because it points to a non-existing file."
  end
end

# Download and save file
#
# @param [String] url
# @param [String] path
def download_and_save_file(url, path)
  options = {}

  proxy = ENV['http_proxy'] || ENV['HTTP_PROXY']
  if proxy
    uri = URI.parse(proxy)
    proxy_host = uri.scheme + "://" + uri.host + ":" + uri.port.to_s
    proxy_user, proxy_pass = uri.userinfo.split(/:/) if uri.userinfo
    options[:proxy_http_basic_authentication] = [proxy_host,proxy_user,proxy_pass]
  end

  open_and_save_file(path, open(url, options).read)
end

# Open and save file
#
# @param [String] path
# @param [Value] What to write in the file
# @param [&block]
def open_and_save_file(path, value = nil, &block)
  # Make sure the directory up to the folder exists
  mkdir_p File.dirname(path)
  # Open the file and use either the block or the value to write the
  # file
  File.open path, 'w' do |f|
    if block_given?
      f.write(yield)
    else
      f.write(value)
    end
  end
end
