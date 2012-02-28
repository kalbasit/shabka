class DotFile
  attr_reader :source_path, :destination_path

  def initialize source_path, destination_path
    @source_path = File.expand_path source_path
    @destination_path = File.expand_path destination_path
  end

  def link_dotfile path
    FileUtils.ln_s("#{source_path}/#{path}", "#{destination_path}/#{path}")
  end

  def link_dotfiles
    find_files(@source_path, recursive: false).each do |path|
      next if File.folder?(path) && find_files(path, recursive: false).select {|p| p =~ /\/\.dont_link$/}.any?
      link_dotfile remove_source_path_from_path(path)
    end
  end

  protected

  def remove_source_path_from_path path
    path.gsub(/#{Regexp.quote source_path}\//, '')
  end

  def find_files path, options = {}
    options = {recursive: true}.merge options
    if options[:recursive]
      @recursive_scans ||= {}
      @recursive_scans[path] ||= Dir["#{path}/**/*"] + Dir["#{path}/**/.*"] - ["#{path}/.", "#{path}/.."]
    else
      @not_recursive_scans ||= {}
      @not_recursive_scans[path] ||= Dir["#{path}/*"] + Dir["#{path}/.*"] - ["#{path}/.", "#{path}/.."]
    end
  end
end
