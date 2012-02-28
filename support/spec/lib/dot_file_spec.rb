require 'spec_helper'
require 'dot_file'

describe DotFile do
  let(:source_path) { stub }
  let(:expanded_source_path) { stub }

  let(:destination_path) { stub }
  let(:expanded_destination_path) { stub }

  before(:each) do
    File.stub(:expand_path).with(source_path).and_return(expanded_source_path)
    File.stub(:expand_path).with(destination_path).and_return(expanded_destination_path)
    FileUtils.stub(:ln_s)
    File.stub(:folder?).and_return(false)
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
      FileUtils.should_receive(:ln_s).with(file, "#{expanded_source_path}/#{file}")
      subject.link_dotfile file
    end

    xit "should be able to link only child files/folders" do
      folder = stub
      File.should_receive(:folder?).with(folder).and_return(true)
    end
  end

  context '#find_files' do
    it {should respond_to :find_files}

    it "should be able to find all files within a folder" do
      Dir.stub([]).with(source_path)
    end
  end
end
