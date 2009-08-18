require 'rake'
require 'rake/testtask'
require 'rake/rdoctask'

task :default  => [:test, :cleanup_test]
desc "Update vendor directory and run tests"
task :ci => ["poolparty:vendor:setup", "poolparty:vendor:update", :spec, :test]

task :cleanup_test do
  ::FileUtils.rm_rf "/tmp/poolparty"
end

# task :test do
#   sh "ruby -Ilib:test #{Dir["#{File.dirname(__FILE__)}/../test/poolparty/*/*.rb"].join(" ")}"
# end

Rake::TestTask.new(:test) do |t|
  t.test_files = FileList['test/lib/**/*_test.rb']
  t.warning = false
  t.verbose = false
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