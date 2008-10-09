require 'config/requirements'
require 'config/hoe' # setup Hoe + all gem configuration

Dir['tasks/**/*.rake'].each { |rake| load rake }

desc "Generate gemspec"
task :gemspec  => [:spec] do |t|
  res = %x[rake debug_gem]
  res = res.split("\n")[1..-1].join("\n")
  ::File.open("#{GEM_NAME.downcase}.gemspec", "w+") do |f|
    f << res
  end
end