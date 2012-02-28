class DotFile
  attr_reader :source_path, :destination_path

  def initialize source_path, destination_path
    @source_path = File.expand_path source_path
    @destination_path = File.expand_path destination_path
  end

  def link_dotfile path
    FileUtils.ln_s(path, dotfile_path(path))
  end

  protected

  def dotfile_path path
    "#{@source_path}/#{path}"
  end

  def find_files

  end
end
