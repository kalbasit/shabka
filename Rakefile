require 'rake'
require 'erb'
require 'open-uri'

# Cross-platform way of finding an executable in the $PATH.
#
#   which('ruby') #=> /usr/bin/ruby
# Credit: mislav at http://stackoverflow.com/a/5471032
def which(cmd)
	exts = ENV['PATHEXT'] ? ENV['PATHEXT'].split(';') : ['']
	ENV['PATH'].split(File::PATH_SEPARATOR).each do |path|
		exts.each { |ext|
			exe = File.join(path, "#{cmd}#{ext}")
			return exe if File.executable?(exe) && !File.directory?(exe)
		}
	end
	return nil
end

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

GO_BINARIES = {
	# Editing helpers (linters and error checkers)
	"github.com/alecthomas/gometalinter" => {postinstall: "gometalinter --install"},

	"github.com/3rf/codecoroner"                              => {},
	"github.com/axw/gocov/gocov"                              => {},
	"github.com/d4l3k/go-pry"                                 => {},
	"github.com/derekparker/delve/cmd/dlv"                    => {},
	"github.com/dim13/gone"                                   => {},
	"github.com/erroneousboat/slack-term"                     => {},
	"github.com/golang/dep/cmd/dep"                           => {},
	"github.com/golang/protobuf/protoc-gen-go"                => {},
	"github.com/golang/tools/cmd/gomvpkg"                     => {},
	"github.com/jteeuwen/go-bindata/go-bindata"               => {},
	"github.com/kalbasit/swm/cmd/swm"                         => {},
	"github.com/monochromegane/the_platinum_searcher/cmd/pt"  => {},
	"github.com/ngdinhtoan/glide-cleanup"                     => {},
	"github.com/opsgenie/opsgenie-lamp"                       => {},
	"github.com/peco/peco/cmd/peco"                           => {},
	"github.com/pocke/lemonade"                               => {},
	"golang.org/x/tools/cmd/cover"                            => {},
	"golang.org/x/tools/cmd/stringer"                         => {},
}

LOCAL_BINARIES = [
	"https://github.com/junegunn/dotfiles/raw/master/bin/tmuxwords.rb",
	# Awesome script by Paul Giden Dann, that let's me undo a Pacman action. Just
	# run `pacman-undo`, delete what you don't want to change and save/exit.
	"https://github.com/giddie/bits-n-pieces/raw/master/pacman-undo/pacman-undo",
	# script for accessing lastpass from rofi
	"https://github.com/Mange/rofi-lpass/raw/master/rofi-lpass",
]

### Initialize

desc "initialize the home directory"
task :init => [:default, :vim_plug, :install_rbenv, :install_go_binaries]

desc "Install all vim plugins"
task :vim_plug do
	sh %Q{vim +PlugUpgrade +PlugInstall +PlugUpdate +qall}
end

desc "install rbenv"
task :install_rbenv do
	sh <<-EOF
		curl https://raw.githubusercontent.com/fesplugas/rbenv-installer/master/bin/rbenv-installer | bash
		export PATH="${HOME}/.rbenv/bin:$PATH"
		eval "$(rbenv init --no-rehash -)"
		ruby_version="`curl -s https://raw.githubusercontent.com/postmodern/ruby-versions/master/ruby/versions.txt | tail -1`"
		rbenv install "${ruby_version}"
		rbenv global "${ruby_version}"
		gem install bundler git-smart
	EOF
end

desc "Install Go Binaries"
task :install_go_binaries do
	# record the current GOPATH and switch to the global one
	oldGoPath = ENV["GOPATH"]
	if ENV["SYSTEM_GOPATH"].nil?
		ENV["GOPATH"] = File.join(ENV["HOME"], ".filesystem")
	else
		ENV["GOPATH"] = ENV["SYSTEM_GOPATH"]
	end
	begin
		# Install the binaries
		GO_BINARIES.each do |bin, options|
			puts "installing #{bin}"
			sh <<~EOF
				go get -u -d #{bin}
				cd #{File.join(ENV["GOPATH"], "src", bin)}
				if [[ -f glide.lock ]]; then
					glide install
				elif [[ -f Gopkg.lock ]]; then
					dep ensure -v
				fi
				if [[ -f Makefile ]]; then
					make install || go install -v
				else
					go install -v
				fi
			EOF
			unless options[:postinstall].nil?
				puts "running post install for #{bin}"
				sh options[:postinstall]
			end
		end
	ensure
		ENV["GOPATH"] = oldGoPath
	end
end

### Default

desc "run the default tasks"
task :default => [:update_submodules, :update_completions, :link, :lesskey, :generate]

desc "Link both private and public config files"
task :link => [:link_dotfiles, :link_private]

desc "Generate lesskey"
task :lesskey do
	sh %Q{lesskey}
end

desc "Update ZSH completions"
task :update_completions do
	if which("kubectl")
		mkdir_p File.join(DOTFILES_PATH, ".zsh", "completions")
		sh %Q{kubectl completion zsh > #{File.join(DOTFILES_PATH, ".zsh", "completions", "_kubectl")}}
	end
end

desc "update README's TOC"
task :update_readme_toc do
	sh %Q{doctoc --title "**Table of Contents**" README.md}
	sh %Q{doctoc --title "**Table of Contents**" .config/nvim/README.md}
end

desc "link files from dotfiles"
task :link_dotfiles do
	link_folder(DOTFILES_PATH, ENV["HOME"])
end

desc "link files from the private repository"
task :link_private do
	link_folder(PRIVATE_PATH, ENV["HOME"]) if File.exists?(PRIVATE_PATH)
end

desc "Initialize and update submodules to the latest version"
task :update_submodules do
	puts "Updating the submodules"
	sh %Q{git submodule update --init > /dev/null}
end

### Generate

task :generate => [:gen_ca_bundle_cert, :gen_local_bin, :download_iterm_shell_integration]

desc "Generate the CA bundler cert"
task :gen_ca_bundle_cert do
	urls = [
		"https://raw.githubusercontent.com/bagder/ca-bundle/master/ca-bundle.crt",
		"https://kalbas.it/ca.crt",
	]
	path = File.expand_path(File.join(ENV["HOME"], ".ca-bundle.crt"))
	options = proxy_options

	open_and_save_file path do |f|
		sep=""
		urls.each do |u|
			f.write("#{sep}########\n######## #{u}\n########\n")
			f.write(open(u, options).read)
			sep="\n"
		end
	end
end

desc "Genereate the local binaries"
task :gen_local_bin do
	LOCAL_BINARIES.each do |u|
		p = File.expand_path(File.join(ENV["MYFS"], "bin", File.basename(u)))
		download_and_save_file(u, p)
		File.chmod(0755, p)
	end
end

desc "Download the iterm shell integration"
task :download_iterm_shell_integration do
	if (/darwin/ =~ RUBY_PLATFORM) != nil
		download_and_save_file("https://iterm2.com/misc/zsh_startup.in", File.expand_path(File.join(ENV["HOME"], ".iterm2_shell_integration.zsh")))
	end
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
	return @encrypted unless @encrypted.nil?
	return false unless File.exists?(File.join(PRIVATE_PATH, ".encrypted"))

	st = `cd #{DOTFILES_PATH}; grep -q OK .private/.encrypted && echo OK || echo NO`.chomp
	if st == "OK"
		@encrypted = true
	else
		@encrypted = false
	end

	return @encrypted
end

def link_folder(folder, dest)
	replace_all = false

	files(folder).each do |file|
		# Take care of an encrypted file if the encryption was not setup.
		# Check if we have an unsecure version
		if !find_encryption_status() && File.exists?("#{file}.unsecure")
			# We do, then symlink the unsecure file instead.
			file = "#{file}.unsecure"
		end

		dest_fn = dest_filename(relative_path(file))
		target_file = File.join(dest, dest_fn)

		if File.symlink?(target_file) && File.identical?(file, target_file)
			puts "identical ~/#{dest_fn}"
		elsif File.exist?(target_file)
			if file =~ /\.erb$/
				replace_file(file, dest)
			elsif File.directory?(target_file)
				link_folder(file, dest)
			elsif replace_all
				replace_file(file, dest)
			else
				print "overwrite ~/#{dest_fn}? [ynaq] "
				case $stdin.gets.chomp
				when 'a'
					replace_all = true
					replace_file(file, dest)
				when 'y'
					replace_file(file, dest)
				when 'q'
					exit
				else
					puts "skipping ~/#{dest_fn}"
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
		link_file(link, dest)
	rescue Errno::ENOENT
		puts "Skipping #{link} symlink because it points to a non-existing file."
	end
end

def proxy_options
	options = {}
	proxy = ENV['http_proxy'] || ENV['HTTP_PROXY']
	if proxy
		uri = URI.parse(proxy)
		proxy_host = uri.scheme + "://" + uri.host + ":" + uri.port.to_s
		proxy_user, proxy_pass = uri.userinfo.split(/:/) if uri.userinfo
		options[:proxy_http_basic_authentication] = [proxy_host,proxy_user,proxy_pass]
	end

	options
end

# Download and save file
#
# @param [String] url
# @param [String] path
def download_and_save_file(url, path)
	options = proxy_options
	open_and_save_file(path, open(url, options).read)
end

# Open and save file
#
# @param [String] path
# @param [Value] What to write in the file
# @param [&block]
def open_and_save_file(path, value = nil, &block)
	# Make sure the directory up to the folder exists
	mkdir_p File.dirname(path) unless File.exist?(File.dirname(path))
	# Open the file and use either the block or the value to write the
	# file
	File.open path, 'w' do |f|
		if block_given?
			yield f
		else
			f.write(value)
		end
	end
end
