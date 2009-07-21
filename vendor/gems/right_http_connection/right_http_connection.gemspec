# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{right_http_connection}
  s.version = "1.2.5"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["RightScale"]
  s.date = %q{2009-03-03}
  s.description = %q{RightScale's robust HTTP/S connection module}
  s.email = %q{rubygems@rightscale.com}
  s.extra_rdoc_files = ["History.txt", "Manifest.txt", "README.txt"]
  s.files = ["History.txt", "Manifest.txt", "README.txt", "Rakefile", "lib/net_fix.rb", "lib/right_http_connection.rb", "setup.rb"]
  s.has_rdoc = true
  s.homepage = %q{http://rightscale.rubyforge.org}
  s.rdoc_options = ["--main", "README.txt"]
  s.require_paths = ["lib"]
  s.rubyforge_project = %q{rightscale}
  s.rubygems_version = %q{1.3.1}
  s.summary = %q{RightScale's robust HTTP/S connection module}

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 2

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
    else
    end
  else
  end
end
