# You also need to setup your name and email for git if you haven't already done so.
# Info at http://github.com/guides/tell-git-your-user-name-and-email-address

begin
  require 'jeweler'
  Jeweler::Tasks.new do |s|
    s.name = "poolparty"
    s.summary = <<-EOM      
      Self-healing, auto-scaling system administration, provisioning
      and maintaining tool that makes cloud computing fun and easy
    EOM
    s.description = s.summary
    
    s.homepage = "http://poolpartyrb.com"
    s.email = "ari.lerner@citrusbyte.com"
    s.authors = ["Ari Lerner"]
    
    s.test_files = Dir["test/**/test_*.rb"]
    s.files = %w(Rakefile History.txt README.txt PostInstall.txt License.txt VERSION.yml) + 
              Dir["{config,examples,lib,spec,test,tasks,script,generators,bin,vendor}/**/*"]
    
    s.has_rdoc = true
    s.extra_rdoc_files = ["README.txt", "License.txt", 'History.txt']
    s.rdoc_options = ['--quiet', '--title', 'PoolParty documentation',
        "--opname", "index.html",
        "--line-numbers",
        "--main", "README",
        "--inline-source"]
    
    s.add_dependency 'activesupport'
    s.add_dependency 'logging'
    s.add_dependency 'ruby2ruby'
    s.add_dependency 'grempe-amazon-ec2'
    s.add_dependency 'rubigen', ">=1.5.2"
    # Certainly there should be more here
    
  end
rescue LoadError
  puts "Jeweler not available. Install it with: sudo gem install technicalpickles-jeweler -s http://gems.github.com"
end
