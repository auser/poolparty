# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{git-style-binaries}
  s.version = "0.1.10"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Nate Murray"]
  s.date = %q{2009-06-12}
  s.description = %q{Ridiculously easy git-style binaries}
  s.email = %q{nate@natemurray.com}
  s.extra_rdoc_files = [
    "README.markdown"
  ]
  s.files = [
    "README.markdown",
    "Rakefile",
    "VERSION.yml",
    "lib/ext/colorize.rb",
    "lib/ext/core.rb",
    "lib/git-style-binary.rb",
    "lib/git-style-binary/autorunner.rb",
    "lib/git-style-binary/command.rb",
    "lib/git-style-binary/commands/help.rb",
    "lib/git-style-binary/helpers/name_resolver.rb",
    "lib/git-style-binary/helpers/pager.rb",
    "lib/git-style-binary/parser.rb",
    "test/fixtures/flickr",
    "test/fixtures/flickr-download",
    "test/fixtures/wordpress",
    "test/fixtures/wordpress-categories",
    "test/fixtures/wordpress-list",
    "test/fixtures/wordpress-post",
    "test/git-style-binary/command_test.rb",
    "test/git_style_binary_test.rb",
    "test/running_binaries_test.rb",
    "test/shoulda_macros/matching_stdio.rb",
    "test/test_helper.rb",
    "vendor/gems/trollop/FAQ.txt",
    "vendor/gems/trollop/History.txt",
    "vendor/gems/trollop/Manifest.txt",
    "vendor/gems/trollop/README.txt",
    "vendor/gems/trollop/Rakefile",
    "vendor/gems/trollop/lib/trollop.rb",
    "vendor/gems/trollop/release-script.txt",
    "vendor/gems/trollop/test/test_trollop.rb",
    "vendor/gems/trollop/www/index.html"
  ]
  s.has_rdoc = true
  s.homepage = %q{http://github.com/jashmenn/git-style-binaries}
  s.rdoc_options = ["--charset=UTF-8"]
  s.require_paths = ["lib"]
  s.rubygems_version = %q{1.3.2}
  s.summary = %q{Add git-style binaries to your project easily.}
  s.test_files = [
    "test/git-style-binary/command_test.rb",
    "test/git_style_binary_test.rb",
    "test/running_binaries_test.rb",
    "test/shoulda_macros/matching_stdio.rb",
    "test/test_helper.rb"
  ]

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 3

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
      s.add_runtime_dependency(%q<trollop>, [">= 0"])
      s.add_runtime_dependency(%q<thoughtbot-shoulda>, [">= 0"])
    else
      s.add_dependency(%q<trollop>, [">= 0"])
      s.add_dependency(%q<thoughtbot-shoulda>, [">= 0"])
    end
  else
    s.add_dependency(%q<trollop>, [">= 0"])
    s.add_dependency(%q<thoughtbot-shoulda>, [">= 0"])
  end
end
