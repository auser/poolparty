require 'config/requirements'

begin
  require 'hanna/rdoctask'
rescue LoadError => e
  require "rake/rdoctask"
end

require 'config/jeweler' # setup gem configuration

Dir['tasks/**/*.rake'].each { |rake| load rake }

desc "Clean tmp directory"
task :clean_tmp do |t|
  FileUtils.rm_rf("#{File.dirname(__FILE__)}/Manifest.txt") if ::File.exists?("#{File.dirname(__FILE__)}/Manifest.txt") 
  FileUtils.touch("#{File.dirname(__FILE__)}/Manifest.txt")
  %w(logs tmp).each do |dir|
    FileUtils.rm_rf("#{File.dirname(__FILE__)}/#{dir}") if ::File.exists?("#{File.dirname(__FILE__)}/#{dir}")
  end
end

desc "Remove the pkg directory"
task :clean_pkg do |t|
  %w(pkg).each do |dir|
    FileUtils.rm_rf("#{File.dirname(__FILE__)}/#{dir}") if ::File.exists?("#{File.dirname(__FILE__)}/#{dir}")
  end
end


namespace :gem do
  task(:build).prerequisites.unshift :gemspec # Prepend the gemspec generation
  
  desc "Build the gem only if the tests pass"
  task :test_then_build => [:test, :build]
  
  desc "Build and install the gem only if the tests pass"
  task :test_then_install => [:test, :install]
end

# Generate documentation
Rake::RDocTask.new do |rd|
  rd.main = "Readme.txt"
  rd.rdoc_files.include("Readme.txt", "lib/**/*.rb")
  rd.rdoc_dir = "rdoc"
  # rd.template = "hanaa"
end