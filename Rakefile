$:.unshift(File.join(File.dirname(__FILE__), "."))
require 'rake'
require 'rake/testtask'
require 'rake/rdoctask'

require 'config/requirements'

begin
  require 'hanna/rdoctask'
rescue LoadError => e
  require "rake/rdoctask"
end

require 'config/jeweler' # setup gem configuration

task :default  => [:test, :cleanup_test]
desc "Update vendor directory and run tests"

namespace :poolparty do
    namespace :vendor do
        desc "Fetch all the submodules"
        task :submodules do
            `git submodule update`
        end
    end
end

task :vendor => ["poolparty:vendor:submodules"]
 
task :cleanup_test do
  ::FileUtils.rm_rf "/tmp/poolparty"
end
 
# task :test do
#   sh "ruby -Ilib:test #{Dir["#{File.dirname(__FILE__)}/../test/poolparty/*/*.rb"].join(" ")}"
# end
 
# Rake::TestTask.new(:test) do |t|
#   t.test_files = FileList['test/lib/**/*_test.rb']
#   t.warning = false
#   t.verbose = false
# end

Rake::TestTask.new(:test) do |test|
  test.libs << 'lib' << 'test'
  test.pattern = 'test/**/*_test.rb'
  test.verbose = true
end
 
begin
  require 'rcov/rcovtask'
 
  Rcov::RcovTask.new(:rcov) do |t|
    t.libs << FileList['lib/**/*.rb']
    t.rcov_opts = [
      '-xRakefile', '-xrakefile',
      '-xlib/erlang',
      '--text-report',
      '--sort coverage'
    ] + FileList['tasks/*.rake'].pathmap("-x%p")
    t.test_files = FileList['test/lib/**/*_test.rb']
    t.output_dir = 'coverage'
    t.verbose = true
  end
rescue LoadError
  puts "RCov is not available"
end


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
  rd.main = "README.rdoc"
  rd.rdoc_files.include("README.rdoc", "lib/**/*.rb")
  rd.rdoc_dir = "rdoc"
  # rd.template = "hanaa"
end
