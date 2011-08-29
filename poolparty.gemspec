# -*- encoding: utf-8 -*-
$:.push File.expand_path("../lib", __FILE__)
require "poolparty/version"

Gem::Specification.new do |s|
  s.name        = "poolparty"
  s.version     = PoolParty::VERSION 

  s.authors     = ["Ari Lerner", "Michael Fairchild", "Nate Murray"]
  s.email       = %q{arilerner@mac.com}
  s.homepage    = %q{http://poolpartyrb.com}
  s.summary     = %q{Simple DSL to describe and realize cloud deployment architectures.}
  s.description = %q{PoolParty: The easy, open-source, cross-cloud management solution}

  s.extra_rdoc_files = [ "README.rdoc" ]
  s.rdoc_options = ["--quiet", "--title", "PoolParty documentation", "--line-numbers", "--main", "README.rdoc"]

  s.files         = `git ls-files`.split("\n")
  s.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  s.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  s.require_paths = ["lib"]

  s.add_runtime_dependency("amazon-ec2", "~> 0.9.17")
  s.add_runtime_dependency("xml-simple", ">= 0")
  s.add_runtime_dependency("json", ">= 0")
end

