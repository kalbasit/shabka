require 'active_support/core_ext/string'

class DotFile
  attr_reader :source_path, :destination_path

  DONT_LINK_FILENAME = '.dont_link'
  LINK_ONLY_CHILDS_FILENAME = '.link_childs'

  def initialize source_path, destination_path
    @source_path = File.expand_path source_path
    @destination_path = File.expand_path destination_path
  end

  def link_dotfile path
    unless File.exists?(absolute_destination_path(path))
      FileUtils.ln_s(absolute_source_path(path), absolute_destination_path(path))
    end
  end

  def link_dotfiles
    find_files(@source_path, recursive: false).each do |path|
      next if file_exists_in_path?(path, DONT_LINK_FILENAME)
      if file_exists_in_path?(path, LINK_ONLY_CHILDS_FILENAME)
        link_dotfiles_in_child_dotfile(path)
        next
      end
      link_dotfile remove_source_path_from_path(path)
    end
  end

  protected

  def absolute_source_path path
    "#{source_path}/#{path}"
  end

  def absolute_destination_path path
    "#{destination_path}/#{path}"
  end

  def link_dotfiles_in_child_dotfile path
    child_path = File.read find_files(path, matches: /\/#{Regexp.quote LINK_ONLY_CHILDS_FILENAME}$/).first
    if child_path.present?
      dest_path = absolute_destination_path(child_path)
    else
      dest_path = absolute_destination_path(remove_source_path_from_path(path))
    end

    DotFile.new path, dest_path
  end

  def file_exists_in_path?(path, file_name)
    File.directory?(path) && find_files(path).select {|p| p =~ /\/#{Regexp.quote file_name}$/}.any?
  end

  def remove_source_path_from_path path
    path.gsub(/#{Regexp.quote source_path}\//, '')
  end

  def find_files path, options = {}
    options = {recursive: true}.merge options
    if options[:recursive]
      @recursive_scans ||= {}
      result = @recursive_scans[path] ||= Dir["#{path}/**/*"] + Dir["#{path}/**/.*"] - ["#{path}/.", "#{path}/.."]
    else
      @not_recursive_scans ||= {}
      result = @not_recursive_scans[path] ||= Dir["#{path}/*"] + Dir["#{path}/.*"] - ["#{path}/.", "#{path}/.."]
    end

    if options[:matches]
      result.select {|p| p =~ options[:matches]}
    else
      result
    end
  end
end
