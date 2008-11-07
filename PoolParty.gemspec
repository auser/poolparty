--- !ruby/object:Gem::Specification 
name: poolparty
version: !ruby/object:Gem::Version 
  version: 0.2.45
platform: ruby
authors: 
- Ari Lerner
autorequire: 
bindir: bin
cert_chain: []

date: 2008-11-06 00:00:00 -08:00
default_executable: 
dependencies: 
- !ruby/object:Gem::Dependency 
  name: activesupport
  type: :runtime
  version_requirement: 
  version_requirements: !ruby/object:Gem::Requirement 
    requirements: 
    - - ">="
      - !ruby/object:Gem::Version 
        version: "0"
    version: 
- !ruby/object:Gem::Dependency 
  name: logging
  type: :runtime
  version_requirement: 
  version_requirements: !ruby/object:Gem::Requirement 
    requirements: 
    - - ">="
      - !ruby/object:Gem::Version 
        version: "0"
    version: 
- !ruby/object:Gem::Dependency 
  name: ruby2ruby
  type: :runtime
  version_requirement: 
  version_requirements: !ruby/object:Gem::Requirement 
    requirements: 
    - - ">="
      - !ruby/object:Gem::Version 
        version: "0"
    version: 
- !ruby/object:Gem::Dependency 
  name: hoe
  type: :runtime
  version_requirement: 
  version_requirements: !ruby/object:Gem::Requirement 
    requirements: 
    - - ">="
      - !ruby/object:Gem::Version 
        version: 1.8.2
    version: 
description: Self-healing, auto-scaling cloud computing tool
email: 
- ari.lerner@citrusbyte.com
executables: []

extensions: []

extra_rdoc_files: []

files: 
- test/test_generator_helper.rb
- test/test_helper.rb
- test/test_pool_spec_generator.rb
- test/test_poolparty.rb
has_rdoc: true
homepage: http://poolparty.rubyforge.org
post_install_message: |-
  Get ready to jump in the pool, you just installed PoolParty!
  
  To get started, run the generator:
  
    pool spec <name>
  
  Please check out the documentation for any questions or check out the google groups at
    http://groups.google.com/group/poolpartyrb
  
  More tutorials can be found at 
    http://poolpartyrb.com
  
  Don't forget to check out the plugin tutorial @ http://poolpartyrb.com to extend PoolParty for your needs!
  
  For more information, check http://PoolPartyrb.com or visit us on IRC at:
    irc.freenode.net
    #poolpartyrb
    
  *** Ari Lerner @ <arilerner@mac.com> ***
rdoc_options: 
- --main
- README.txt
require_paths: 
- lib
required_ruby_version: !ruby/object:Gem::Requirement 
  requirements: 
  - - ">="
    - !ruby/object:Gem::Version 
      version: "0"
  version: 
required_rubygems_version: !ruby/object:Gem::Requirement 
  requirements: 
  - - ">="
    - !ruby/object:Gem::Version 
      version: "0"
  version: 
requirements: []

rubyforge_project: poolparty
rubygems_version: 1.2.0
signing_key: 
specification_version: 2
summary: Self-healing, auto-scaling cloud computing tool
test_files: 
- test/test_generator_helper.rb
- test/test_helper.rb
- test/test_pool_spec_generator.rb
- test/test_poolparty.rb
