require 'rake'
require 'spec/rake/spectask'

task :default  => :spec

Spec::Rake::SpecTask.new(:spec) do |t|
  t.warning = 
    t.rcov = false
  t.spec_files = Dir["spec/**/*_spec.rb"]
end