#!/usr/bin/ruby

File.class_eval do

  def relative_path(from)
    path = File.expand_path(self.path).split(File::SEPARATOR)
    rel = from.split(File::SEPARATOR)
    while (path.length > 0) && (path.first == rel.first)
      path.shift
      rel.shift
    end
    ('..' + File::SEPARATOR) * rel.length + path.join(File::SEPARATOR)
  end
end

class CopyToIphone

  def initialize(music_folder)
    @music_folder = music_folder
  end

  def symlink(file)
    relative_path = File.new(file).relative_path(@music_folder)
    dest_file = @music_folder + File::SEPARATOR + File.basename(relative_path)
    while File.exists? dest_file
      #raise "File already exists" if File.identical? file dest_file
      if File.identical? file, dest_file
        p "File " + file + " already exists"
        return false
      end
      dest_file += '_'
    end
    File.symlink(relative_path, dest_file)
    true
  end

end

iphone_music_folder = "/home/wael/Files/Downloads/iPhone/Music"

if ARGV.size == 0
  p "USAGE: " + __FILE__ + " file [file file ...]"
  exit 0
end

Files = []

while ARGV.size > 0
  file_path += " " + ARGV[0] if file_path
  file_path = ARGV[0] unless file_path
  if File.exists? file_path
    Files << file_path
    file_path = nil
  end
  ARGV.shift
end

if Files.size == 0
  p "None of the files given exists"
  exit 1
end

cti = CopyToIphone.new(iphone_music_folder)
count = 0

Files.each do |f|
  count += 1 if cti.symlink(f)
end

print count.to_s + " files has been copied"

exit