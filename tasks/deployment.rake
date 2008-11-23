desc 'Release the website and new gem version'
task :deploy => [:check_version, :website, :release] do
  puts "Remember to create SVN tag:"
  puts "svn copy svn+ssh://#{rubyforge_username}@rubyforge.org/var/svn/#{PATH}/trunk " +
    "svn+ssh://#{rubyforge_username}@rubyforge.org/var/svn/#{PATH}/tags/REL-#{VERS} "
  puts "Suggested comment:"
  puts "Tagging release #{CHANGES}"
end

# desc 'Runs tasks website_generate and install_gem as a local deployment of the gem'
# task :local_deploy => []
desc "Deploy the gem locally"
task :local_deploy => [:website_generate, :install_gem, :build_local_gem] do
  sh "#{'sudo ' unless Hoe::WINDOZE }gem install pkg/*.gem --no-rdoc --no-ri"
end

task :check_version do
  unless ENV['VERSION']
    puts 'Must pass a VERSION=x.y.z release version'
    exit
  end
  unless ENV['VERSION'] == VERS
    puts "Please update your version.rb to match the release version, currently #{VERS}"
    exit
  end
end

desc 'Install the package as a gem, without generating documentation(ri/rdoc)'
task :install_gem_no_doc => [:clean, :package] do
  sh "#{'sudo ' unless Hoe::WINDOZE }gem install pkg/*.gem --no-rdoc --no-ri"
end

desc "Ensure .hoerc exists in the homedirectory. Create it if it doesn't"
task :hoerc do
str =<<-EOE
---
publish_on_announce: true
exclude: !ruby/regexp
  /tmp$|\.git|log$|local/.*\.rb$|Makefile|\.beam$/
EOE
  hoerc_path = ::File.join( ENV["HOME"], ".hoerc" )
  ::File.open(hoerc_path, "w+") {|f| f << str } unless ::File.file?(hoerc_path)
end

namespace :manifest do
  desc 'Recreate Manifest.txt to include ALL files'
  task :refresh => [:hoerc] do
    `rake check_manifest | patch -p0 > Manifest.txt`
  end
end