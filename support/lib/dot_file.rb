class DotFile
  attr_reader :source_path, :destination_path

  def initialize source_path, destination_path
    @source_path = File.expand_path source_path
    @destination_path = File.expand_path destination_path
  end

  def link_dotfile path
    FileUtils.ln_s("#{source_path}/#{path}", "#{destination_path}/#{path}")
  end

  protected

  def find_files path
    @files ||= {}
    @files[path] ||= Dir["#{path}/**/*"] + Dir["#{path}/**/.*"] - ["#{path}/.", "#{path}/.."]
  end
end
