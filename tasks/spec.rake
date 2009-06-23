require 'rake'
require 'spec/rake/spectask'
require 'rake/testtask'
require 'rake/rdoctask'

task :default  => [:spec, :test, :cleanup_test]
desc "Update vendor directory and run tests"
task :ci => ["pp:vendor:setup", "pp:vendor:update", :spec, :test]

task :cleanup_test do
  ::FileUtils.rm_rf "/tmp/poolparty"
end

Spec::Rake::SpecTask.new(:spec) do |t|
  t.warning = t.rcov = false
  t.spec_files = Dir["spec/**/*_spec.rb"]
end

Spec::Rake::SpecTask.new(:spec_v) do |t|
  t.rcov = true
  t.rcov_opts = ['--exclude', 'gems*,spec*,website*,test*']
  t.ruby_opts = []
  t.spec_files = Dir["spec/**/*_spec.rb"]
end

# task :test do
#   sh "ruby -Ilib:test #{Dir["#{File.dirname(__FILE__)}/../test/poolparty/*/*.rb"].join(" ")}"
# end

Rake::TestTask.new(:test) do |t|
  t.test_files = FileList['test/poolparty/**/*_test.rb']
  t.warning = false
  t.verbose = false
end

begin
  require 'rcov/rcovtask'
 
  Rcov::RcovTask.new(:rcov) do |t|
    t.libs << FileList['lib/poolparty/**/*.rb']
    dot_rakes = 
    t.rcov_opts = [
      '-xRakefile', '-xrakefile',
      '-xlib/erlang',
      '--text-report',
      '--sort coverage'
    ] + FileList['tasks/*.rake'].pathmap("-x%p")
    t.test_files = FileList['test/poolparty/**/*_test.rb']
    t.output_dir = 'coverage'
    t.verbose = true
  end
rescue LoadError
  puts "RCov is not available"
end