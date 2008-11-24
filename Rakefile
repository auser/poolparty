require 'config/requirements'
require 'config/hoe' # setup Hoe + all gem configuration

Dir['tasks/**/*.rake'].each { |rake| load rake }

desc "Clean tmp directory"
task :clean_tmp do |t|
  %x[rm #{File.dirname(__FILE__)}/Manifest.txt; touch #{File.dirname(__FILE__)}/Manifest.txt]
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

desc "Generate a new manifest and a new gem"
task :build_local_gem => [:clean_tmp, :spec, :clean_pkg, :"manifest:refresh", :package]

desc "Packge with timestamp"
task :update_timestamp do
  data = open("PostInstall.txt").read
  str = "Updated at #{Time.now.strftime("%H:%M %D")}"
  
  if data.scan(/Updated at/).empty?
    data = data ^ {:updated_at => str}    
  else
    data = data.gsub(/just installed PoolParty\!(.*)$/, "just installed PoolParty! (#{str})")
  end
  ::File.open("PostInstall.txt", "w+") {|f| f << data }
end

desc "Release to github"
task :github_release => [:clean_tmp, :spec, :clean_pkg, :"manifest:refresh", :update_timestamp, :package] do
  res = %x[rake debug_gem]
  res = res.split("\n")[1..-1].join("\n")
  ::File.open("#{GEM_NAME.downcase}.gemspec", "w+") do |f|
    f << res
  end
  `mv #{::File.expand_path(::File.dirname(__FILE__))}/pkg/*.gem #{::File.expand_path(::File.dirname(__FILE__))}/pkg/poolparty.gem`
end

desc "Generate gemspec"
task :gemspec  => [:spec, :clean_tmp, :"manifest:refresh", :build_local_gem] do |t|
  res = %x[rake debug_gem]
  res = res.split("\n")[1..-1].join("\n")
  ::File.open("#{GEM_NAME.downcase}.gemspec", "w+") do |f|
    f << res
  end
end

desc "Generate gemspec for github"
task :gh => [:github_release] do
  filepath = ::File.join(::File.dirname(__FILE__), "poolparty.gemspec")
  data = open(filepath).read
  spec = eval("$SAFE = 3\n#{data}")
  yml = YAML.dump spec
  File.open(filepath, "w+") do |f|
    f << yml
  end
end

desc "Generate github gemspec and latest gem"
task :ghgem => [:gh] do
  %x[sudo gem install pkg/poolparty.gem]
end