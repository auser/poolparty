require 'config/requirements'
require 'config/hoe' # setup Hoe + all gem configuration

Dir['tasks/**/*.rake'].each { |rake| load rake }

desc "Clean tmp directory"
task :clean_tmp do |t|
  %w(logs tmp ~).each do |dir|
    FileUtils.rm_rf("#{File.dirname(__FILE__)}/#{dir}") if ::File.exists?("#{File.dirname(__FILE__)}/#{dir}")
  end
end
desc "Remove the pkg directory"
task :clean_pkg do |t|
  %w(pkg).each do |dir|
    FileUtils.rm_rf("#{File.dirname(__FILE__)}/#{dir}") if ::File.exists?("#{File.dirname(__FILE__)}/#{dir}")
  end
end
desc "Generate gemspec"
task :gemspec  => [:spec, :clean_tmp, :"manifest:refresh", :local_deploy, :clean_pkg] do |t|
  res = %x[rake debug_gem]
  res = res.split("\n")[1..-1].join("\n")
  ::File.open("#{GEM_NAME.downcase}.gemspec", "w+") do |f|
    f << res
  end
end