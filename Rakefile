# TODO: WOW! Refactor!!

require 'rake'
require 'erb'
require 'open-uri'

DOTFILES_PATH = File.expand_path("../", __FILE__)
IGNORED_FILES = [
  "Rakefile",
  "README.md",
  ".DS_Store",
  ".encrypted",
  ".encrypted_files_list",
  ".etc",
  ".git",
  ".gitattributes",
  ".git-crypt",
]
ENCRYPTED_FILES_LIST_PATH = File.join(DOTFILES_PATH, ".encrypted_files_list")
ENCRYPTED_FILES = File.read(ENCRYPTED_FILES_LIST_PATH).split("\n")
IGNORED_WHEN_UNSECURE = [
  # Folders
  /\.ssh/,
  /\.gnupg/,
  /\.cron/,
  /\.notmuch/,

  # Files
  /\.bin\/ssh-agents/,
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
]

desc "Generate the list of encrypted files"
task :generate_encryted_files_list do
  sh %Q{git-crypt status | grep -v 'not encrypted' | awk '{print $2}' | sort > #{ENCRYPTED_FILES_LIST_PATH}}
end

desc "Install Go Binaries"
task :install_go_binaries do
  GO_BINARIES.each do |binary|
    sh %Q{go get -u #{binary}}
  end
end

desc "install the dot files into user's home directory"
task :install => [:checkout_vundle_master, :update_submodules, :switch_to_zsh] do
  link_folder(Dir.getwd)
end

task :default => :install

task :checkout_vundle_master do
  sh %Q{cd .vim/bundle/Vundle.vim && git checkout master}
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

desc "Install YouCompleteme"
task :ycm_install do
  ycm_path = File.join(DOTFILES_PATH, ".vim/bundle/YouCompleteMe")
  if !File.exist?(ycm_path)
    puts "YCM is not installed in the bundle, please run 'vim +VundleInstall +qall"
    exit
  end

  puts "Installing required packages, please enter the sudo password"
  sh "sudo apt-get install build-essential cmake python-dev" if RUBY_PLATFORM.downcase =~ /linux/
  sh "cd #{ycm_path} && ./install.sh --clang-completer"
end

desc "Update the CA bundler cert"
task :update_ca_bundle_cert do
  url = "https://raw.githubusercontent.com/bagder/ca-bundle/master/ca-bundle.crt"
  path = File.expand_path(File.join(File.dirname(__FILE__), ".ca-bundle.crt"))
  download_and_save_file url, path
end

def relative_path(file)
 return file.gsub("#{DOTFILES_PATH}/", "")
end

def replace_file(file)
  home_file = File.join(ENV['HOME'], home_file_name(relative_path(file)))
  sh %Q{rm -rf '#{home_file}'}
  link_file(file)
end

def files(folder)
  files = Dir["#{folder}/.??*"] + Dir["#{folder}/*"]
  files.reject! do |file|
    relative_path = file.gsub("#{DOTFILES_PATH}/", "")
    IGNORED_FILES.include?(relative_path)
  end

  files.reject! {|f| f =~ /\.unsecure$/}

  return files
end

def find_encryption_status
  return `cd #{DOTFILES_PATH}; grep -q OK .encrypted && echo OK || echo NO`.chomp
end

def is_encrypted?(file)
  ENCRYPTED_FILES.any? do |encrypted_file|
    relative_path(file) == encrypted_file
  end
end

def link_folder(folder)
  replace_all = false
  encryption_status = find_encryption_status()

  files(folder).each do |file|
    # Take care of an encrypted file if the encryption was not setup.
    if encryption_status == "NO"
      must_skip = false
      IGNORED_WHEN_UNSECURE.each do |p|
        if relative_path(file) =~ p
          puts "In unsecure mode, skipping ~/#{home_file_name(relative_path(file))}"
          must_skip = true
          break
        end
      end
      next if must_skip

      if is_encrypted?(file)
        # Check if we have an unsecure version
        if File.exists?("#{file}.unsecure")
          # We do, then symlink the unsecure file instead.
          secure_file = "#{file}"
          file = "#{file}.unsecure"
          unsecure_file = "#{file}"
        else
          # We don't so let's skip this file entirely.
          puts "In unsecure mode, skipping ~/#{home_file_name(relative_path(file))}"
          next
        end
      end
    end

    relative_path = file.gsub("#{DOTFILES_PATH}/", "")
    home_file = File.join(ENV['HOME'], home_file_name(relative_path))

    if File.exist?(home_file)
      if File.identical? file, home_file
        puts "identical ~/#{home_file_name(relative_path)}"
      elsif file =~ /\.erb$/
        replace_file(file)
      elsif File.directory?(home_file)
        link_folder(file)
      elsif replace_all
        replace_file(file)
      else
        print "overwrite ~/#{home_file_name(relative_path)}? [ynaq] "
        case $stdin.gets.chomp
        when 'a'
          replace_all = true
          replace_file(file)
        when 'y'
          replace_file(file)
        when 'q'
          exit
        else
          puts "skipping ~/#{home_file_name(relative_path)}"
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

def home_file_name(path)
  return path.sub(/\.erb$/, '').sub(/\.unsecure$/, '')
end

def link_file(file)
  relative_path = file.gsub("#{DOTFILES_PATH}/", "")
  home_file = File.join(ENV['HOME'], home_file_name(relative_path))

  if file =~ /.erb$/
    puts "generating #{home_file}"
    File.open(home_file, 'w') do |new_file|
      new_file.write ERB.new(File.read(file)).result(binding)
    end
    if is_encrypted?(file)
      File.chmod(0400, home_file)
    end
  else
    puts "linking #{file}"
    sh %Q{ln -s "#{file}" "#{home_file}"}
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
