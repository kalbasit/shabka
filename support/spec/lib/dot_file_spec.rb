require 'spec_helper'
require 'dot_file'

describe DotFile do
  let(:source_path) { stub "source_path" }
  let(:expanded_source_path) { stub "expanded_source_path" }

  let(:destination_path) { stub "destination_path" }
  let(:expanded_destination_path) { stub "expanded_destination_path" }

  before(:each) do
    File.stub(:expand_path).with(source_path).and_return(expanded_source_path)
    File.stub(:expand_path).with(destination_path).and_return(expanded_destination_path)
    FileUtils.stub(:ln_s)
    File.stub(:directory?).and_return(false)
  end

  subject { DotFile.new source_path, destination_path }

  context "Initialize" do
    it "should require source and destination paths" do
      expect do
        DotFile.new
      end.to raise_error ArgumentError
    end

    it "should set the @source to the given source path, but expanded" do
      File.should_receive(:expand_path).with(source_path).and_return(expanded_source_path)
      File.should_receive(:expand_path).with(destination_path).and_return(expanded_destination_path)
      subject.source_path.should == expanded_source_path
      subject.destination_path.should == expanded_destination_path
    end
  end

  context '#link_dotfile' do
    it {should respond_to :link_dotfile }

    it "should, given a path, links it to the home folder" do
      file = stub
      FileUtils.should_receive(:ln_s).with("#{expanded_source_path}/#{file}", "#{expanded_destination_path}/#{file}")
      subject.link_dotfile file
    end

    it "should not link if the file exists" do
      file = stub
      File.stub(:exists?).with("#{expanded_destination_path}/#{file}").and_return(true)
      FileUtils.should_not_receive(:ln_s).with("#{expanded_source_path}/#{file}", "#{expanded_destination_path}/#{file}")
      subject.link_dotfile file
    end
  end

  context '#link_dotfiles' do
    before :each do
      File.stub(:directory?).with("#{expanded_source_path}/folder").and_return true
      subject.stub(:link_dotfiles_in_child_dotfile)
      subject.stub(:find_files).with(expanded_source_path, recursive: false).
        and_return ["#{expanded_source_path}/folder"]
    end

    it {should respond_to :link_dotfiles }

    it "should find_files in the source_path" do
      subject.should_receive(:find_files).with(expanded_source_path, recursive: false).and_return []
      subject.link_dotfiles
    end

    it "should call link_dotfile for files and folders that does not have a .link_childs not a .dont_link" do
      subject.should_receive(:find_files).with(expanded_source_path, recursive: false).and_return ["#{expanded_source_path}/.file"]
      subject.should_receive(:link_dotfile).with('.file')
      subject.link_dotfiles
    end

    it "should not call link_dotfile if the folder in question has a .dont_link file" do
      subject.should_not_receive(:link_dotfile).with('folder')
      subject.should_receive(:find_files).with("#{expanded_source_path}/folder").at_least(:once).
        and_return ["#{expanded_source_path}/folder/.file", "#{expanded_source_path}/folder/.dont_link"]

      subject.link_dotfiles
    end

    it "should not link any folder that has .link_childs file" do
      subject.should_not_receive(:link_dotfile).with('folder')
      subject.should_receive(:find_files).with("#{expanded_source_path}/folder").at_least(:once).
        and_return ["#{expanded_source_path}/folder/.file", "#{expanded_source_path}/folder/.link_childs"]

      subject.link_dotfiles
    end

    it "should call link_dotfiles_in_child_dotfile if .link_childs is present" do
      subject.should_receive(:link_dotfiles_in_child_dotfile).with("#{expanded_source_path}/folder")
      subject.should_receive(:find_files).with("#{expanded_source_path}/folder").at_least(:once).
        and_return ["#{expanded_source_path}/folder/.file", "#{expanded_source_path}/folder/.link_childs"]

      subject.link_dotfiles
    end
  end

  context '#link_dotfiles_in_child_dotfile' do
    before :each do
      File.stub(:directory?).with("#{expanded_source_path}/folder").and_return true
      File.stub(:read).with("#{expanded_source_path}/folder/.link_childs").and_return("")
      subject.stub(:find_files).with(expanded_source_path, recursive: false).
        and_return ["#{expanded_source_path}/folder"]
      subject.stub(:find_files).with("#{expanded_source_path}/folder", recursive: false).
        and_return ["#{expanded_source_path}/folder/.file", "#{expanded_source_path}/folder/.link_childs"]
      subject.stub(:find_files).with("#{expanded_source_path}/folder", matches: /\/\.link_childs$/).
        and_return ["#{expanded_source_path}/folder/.link_childs"]
    end

    it {should respond_to :link_dotfiles_in_child_dotfile}

    it "should create a new DotFile with the adjust source and destination paths" do
      subject.class.should_receive(:new).with("#{expanded_source_path}/folder", "#{expanded_destination_path}/folder").once

      subject.send(:link_dotfiles_in_child_dotfile, "#{expanded_source_path}/folder")
    end

    it "an empty .link_childs should not affect the path" do
      File.should_receive(:read).with("#{expanded_source_path}/folder/.link_childs").and_return("")
      subject.class.should_receive(:new).with("#{expanded_source_path}/folder", "#{expanded_destination_path}/folder").once

      subject.send(:link_dotfiles_in_child_dotfile, "#{expanded_source_path}/folder")
    end

    it "should honor the contents of .link_childs" do
      File.should_receive(:read).with("#{expanded_source_path}/folder/.link_childs").and_return("some/path")
      subject.class.should_receive(:new).with("#{expanded_source_path}/folder", "#{expanded_destination_path}/some/path").once

      subject.send(:link_dotfiles_in_child_dotfile, "#{expanded_source_path}/folder")
    end
  end

  context '#remove_source_path_from_path' do
    it {should respond_to :remove_source_path_from_path}

    it "should remove source_path from it" do
      subject.send(:remove_source_path_from_path, "#{expanded_source_path}/file").should == "file"
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
