require 'spec_helper'
require 'dot_file'

describe DotFile do
  let(:source_path) { stub "source_path" }
  let(:expanded_source_path) { stub "expanded_source_path" }

  let(:destination_path) { stub "destination_path" }
  let(:expanded_destination_path) { stub "expanded_destination_path" }

  let(:file) { stub "file" }
  let(:source_file) { stub "source file" }
  let(:destination_file) { stub "destination file" }

  let(:folder) { stub "folder" }
  let(:source_folder) { stub "source folder" }
  let(:destination_folder) { stub "destination folder" }

  let(:dont_link_file) { stub "dont_link_file" }
  let(:source_dont_link_file) { stub "source dont_link_file" }
  let(:destination_dont_link_file) { stub "destination dont_link_file" }

  let(:link_childs_file) { stub "link_childs" }
  let(:source_link_childs_file) { stub "source link_childs" }
  let(:destination_link_childs_file) { stub "destination link_childs" }

  before(:each) do
    File.stub(:expand_path).with(source_path).and_return(expanded_source_path)
    File.stub(:expand_path).with(destination_path).and_return(expanded_destination_path)
    FileUtils.stub(:ln_s)
    File.stub(:read)

    File.stub(:directory?).and_return false

    subject.stub(:absolute_source_path).with(file).and_return(source_file)
    subject.stub(:absolute_destination_path).with(file).and_return(destination_file)
    subject.stub(:remove_source_path_from_path).with(source_file).and_return(file)

    subject.stub(:absolute_source_path).with(folder).and_return(source_folder)
    subject.stub(:absolute_destination_path).with(folder).and_return(destination_folder)
    subject.stub(:remove_source_path_from_path).with(source_folder).and_return(folder)

    subject.stub(:absolute_source_path).with(dont_link_file).and_return(source_dont_link_file)
    subject.stub(:absolute_destination_path).with(dont_link_file).and_return(destination_dont_link_file)
    subject.stub(:remove_source_path_from_path).with(source_dont_link_file).and_return(dont_link_file)

    subject.stub(:absolute_source_path).with(link_childs_file).and_return(source_link_childs_file)
    subject.stub(:absolute_destination_path).with(link_childs_file).and_return(destination_link_childs_file)
    subject.stub(:remove_source_path_from_path).with(source_link_childs_file).and_return(link_childs_file)
  end

  subject { DotFile.new source_path, destination_path }

  context "Initialize" do
    it "should require source and destination paths" do
      expect do
        DotFile.new
      end.to raise_error ArgumentError
    end

    it "should expand_path the arguments while stored" do
      File.should_receive(:expand_path).with(source_path).and_return(expanded_source_path)
      File.should_receive(:expand_path).with(destination_path).and_return(expanded_destination_path)

      it = DotFile.new source_path, destination_path
      it.source_path.should == expanded_source_path
      it.destination_path.should == expanded_destination_path
    end
  end

  context '#link_dotfile' do
    it {should respond_to :link_dotfile }

    it "should, given a path, links it to the destination folder" do
      FileUtils.should_receive(:ln_s).with(source_file, destination_file)
      subject.link_dotfile file
    end

    it "should not link if the file exists" do
      File.stub(:exists?).with(destination_file).and_return(true)
      FileUtils.should_not_receive(:ln_s).with(source_file, destination_file)
      subject.link_dotfile file
    end
  end

  context '#link_dotfiles' do
    before :each do
      subject.stub(:link_dotfiles_in_child_dotfile)
      subject.stub(:find_files).with(expanded_source_path, recursive: false).and_return [source_folder]
      subject.stub(:file_exists_in_path?).with(any_args).and_return false
    end

    it {should respond_to :link_dotfiles }

    it "should find_files in the source_path" do
      subject.should_receive(:find_files).with(expanded_source_path, recursive: false).and_return []
      subject.link_dotfiles
    end

    it "should call link_dotfile for files and folders that does not have a .link_childs not a .dont_link" do
      subject.should_receive(:find_files).with(expanded_source_path, recursive: false).and_return [source_file]
      subject.should_receive(:link_dotfile).with(file)

      subject.link_dotfiles
    end

    it "should not call link_dotfile if the folder in question has a .dont_link file" do
      subject.stub(:file_exists_in_path?).with(source_folder, DotFile::DONT_LINK_FILENAME).and_return true
      subject.should_not_receive(:link_dotfile).with(folder)

      subject.link_dotfiles
    end

    it "should not link any folder that has .link_childs file but calls link_dotfiles_in_child_dotfile on it instead" do
      subject.stub(:file_exists_in_path?).with(source_folder, DotFile::LINK_ONLY_CHILDS_FILENAME).and_return true
      subject.should_receive(:link_dotfiles_in_child_dotfile).with(source_folder)
      subject.should_not_receive(:link_dotfile).with(folder)

      subject.link_dotfiles
    end
  end

  context '#link_dotfiles_in_child_dotfile' do
    it {should respond_to :link_dotfiles_in_child_dotfile}

    it "should create a new DotFile starting from the existing source/destination paths" do
      subject.class.should_receive(:new).with(source_folder, destination_folder).once

      subject.send(:link_dotfiles_in_child_dotfile, source_folder)
    end

    it "should honor the contents of .link_childs" do
      some_path = stub "some path"
      absolute_some_path = stub "absolute some path"
      File.stub(:read).and_return(some_path)
      subject.stub(:absolute_destination_path).with(some_path).and_return(absolute_some_path)
      subject.class.should_receive(:new).with(source_folder, absolute_some_path)

      subject.send(:link_dotfiles_in_child_dotfile, source_folder)
    end
  end

  context '#find_files' do
    before :each do
      Dir.stub(:[]).with("#{source_path}/**/*").and_return []
      Dir.stub(:[]).with("#{source_path}/**/.*").and_return ["#{source_path}/.", "#{source_path}/..", "#{source_path}/.file"]
    end

    it {should respond_to :find_files}

    it "should be able to find all files within a folder" do
      Dir.should_receive(:[]).with("#{source_path}/**/*").and_return []
      Dir.should_receive(:[]).with("#{source_path}/**/.*").and_return ["#{source_path}/.", "#{source_path}/..", "#{source_path}/.file"]
      subject.send(:find_files, source_path).should include("#{source_path}/.file")
    end

    it "should not return . and .." do
      subject.send(:find_files, source_path).should_not include("#{source_path}/.")
      subject.send(:find_files, source_path).should_not include("#{source_path}/..")
    end

    it "should cache the result" do
      Dir.should_receive(:[]).with("#{source_path}/**/.*").once.and_return(["#{source_path}/file"])
      subject.send(:find_files, source_path).should == ["#{source_path}/file"]
      subject.send(:find_files, source_path).should == subject.send(:find_files, source_path)
    end

    it "should not return the cache of the wrond options hash" do
      Dir.should_receive(:[]).with("#{source_path}/.*").once
      Dir.should_receive(:[]).with("#{source_path}/*").once
      Dir.should_receive(:[]).with("#{source_path}/**/*").once
      Dir.should_receive(:[]).with("#{source_path}/**/.*").once

      subject.send(:find_files, source_path, recursive: true)
      subject.send(:find_files, source_path, recursive: false)
    end

    it "should allow me to find files under just one level without the . and .." do
      Dir.should_receive(:[]).with("#{source_path}/*").and_return []
      Dir.should_receive(:[]).with("#{source_path}/.*").and_return ["#{source_path}/.", "#{source_path}/..", "#{source_path}/.file"]
      subject.send(:find_files, source_path, :recursive => false).should == ["#{source_path}/.file"]
    end

    it "support matches" do
      Dir.stub(:[]).with("#{source_path}/**/.*").
        and_return ["#{source_path}/.", "#{source_path}/..", "#{source_path}/.file", "#{source_path}/.file2"]
      subject.send(:find_files, source_path, matches: /\.file$/).should == ["#{source_path}/.file"]
    end
  end
end
