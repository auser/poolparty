# You also need to setup your name and email for git if you haven't already done so.
# Info at http://github.com/guides/tell-git-your-user-name-and-email-address

begin
class Array
  def one_of_regex
    option_list = join "|"
    Regexp.new "(#{option_list})"
  end
end

  require 'jeweler'
  Jeweler::Tasks.new do |s|
    s.name = "poolparty"
    s.description = "PoolParty: The easy, open-source, cross-cloud management solution"
    s.summary = <<-EOM      
      Self-healing, auto-scaling system administration, provisioning
      and maintaining tool that makes cloud computing fun and easy
    EOM
    
    s.homepage = "http://poolpartyrb.com"
    s.email = "ari.lerner@citrusbyte.com"
    s.authors = ["Ari Lerner"]
    
    s.test_files = Dir["test/**/test_*.rb"]
    excludes_regexp = ["lib/erlang"].one_of_regex

    s.files = (%w(Rakefile History.txt README.txt PostInstall.txt License.txt VERSION.yml) + 
              Dir["{config,examples,lib,spec,test,tasks,script,generators,bin,vendor}/**/*"]).delete_if{|f| f =~ excludes_regexp}
    
    s.has_rdoc = true
    s.extra_rdoc_files = ["README.txt", "License.txt", 'History.txt']
    s.rdoc_options = ['--quiet', '--title', 'PoolParty documentation',
        # "index.html",
        "--line-numbers",
        "--main", "README"
        ]
    
    s.add_dependency 'activesupport'
    s.add_dependency 'logging'
    s.add_dependency 'right_aws'
    s.add_dependency 'rubigen', ">=1.5.2"
    
  end
rescue LoadError
  puts "Jeweler not available. Install it with: sudo gem install technicalpickles-jeweler -s http://gems.github.com"
end
