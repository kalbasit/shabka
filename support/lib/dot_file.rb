class DotFile
  attr_reader :source_path, :destination_path

  DONT_LINK_FILENAME = '.dont_link'
  LINK_ONLY_CHILDS_FILENAME = '.link_childs'

  def initialize source_path, destination_path
    @source_path = File.expand_path source_path
    @destination_path = File.expand_path destination_path
  end

  def link_dotfile path
    FileUtils.ln_s("#{source_path}/#{path}", "#{destination_path}/#{path}")
  end

  def link_dotfiles
    find_files(@source_path, recursive: false).each do |path|
      next if file_exists_in_path(path, DONT_LINK_FILENAME)
      next if file_exists_in_path(path, LINK_ONLY_CHILDS_FILENAME)
      link_dotfile remove_source_path_from_path(path)
    end
  end

  protected

  def file_exists_in_path(path, file_name)
    File.folder?(path) && find_files(path).select {|p| p =~ /\/#{Regexp.quote file_name}$/}.any?
  end

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
