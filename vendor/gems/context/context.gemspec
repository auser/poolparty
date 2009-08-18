Gem::Specification.new do |s|
  s.name     = "context"
  s.version  = "0.5.5"
  s.date     = "2008-10-03"
  s.summary  = "Contexts and DSL sugar for your tests"
  s.email    = "jeremy@entp.com"
  s.homepage = "http://github.com/jeremymcanally/context"
  s.description = "If you've ever wanted contexts in your Test::Unit tests, then context is for you.  Your tests will be easier to read and write without all the magic and extra code smell!"
  s.has_rdoc = true
  s.authors  = ["Jeremy McAnally"]
  s.files    = [
  	"README.rdoc", 
  	"Rakefile", 
  	"context.gemspec", 
    "History.txt",
    "License.txt",
    "Manifest.txt",
    "PostInstall.txt",
    "config/hoe.rb",
    "config/requirements.rb",
    "lib/context.rb",
    "lib/context/version.rb",
    "lib/context/lifecycle.rb",
    "lib/context/suite.rb",
    "lib/context/context.rb",
    "lib/context/shared_behavior.rb",
    "lib/context/test.rb",
    "lib/context/version.rb",
    "lib/context/core_ext/string.rb",
    "lib/context/core_ext/rails_hacks.rb",
    "setup.rb"
  ]
  
  s.test_files = [
    "test/test_context.rb",
    "test/test_core_ext.rb",
    "test/test_lifecycle.rb",
    "test/test_test.rb",
    "test/test_helper.rb"
  ]

  s.rdoc_options = ["--main", "README.rdoc"]
  s.extra_rdoc_files = ["History.txt", "Manifest.txt", "README.rdoc"]
end