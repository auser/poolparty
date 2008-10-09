require 'PoolParty/version'

AUTHOR = 'Ari Lerner'  # can also be an array of Authors
EMAIL = "ari.lerner@citrusbyte.com"
DESCRIPTION =<<-EOM      
  Self-healing, auto-scaling cloud computing tool
EOM
GEM_NAME = 'poolparty' # what ppl will type to install your gem
RUBYFORGE_PROJECT = 'poolparty' # The unix name for your project
HOMEPAGE = "http://poolpartyrb.com"
HOMEPATH = "http://#{GEM_NAME}.rubyforge.org"
DOWNLOAD_PATH = "http://rubyforge.org/projects/#{RUBYFORGE_PROJECT}"
EXTRA_DEPENDENCIES = [
 ['activesupport'],
 ['open4'],
 ['logging']
]    # An array of rubygem dependencies [name, version]

@config_file = "~/.rubyforge/user-config.yml"
@config = nil
RUBYFORGE_USERNAME = "unknown"
def rubyforge_username
  unless @config
    begin
      @config = YAML.load(File.read(File.expand_path(@config_file)))
    rescue
      puts <<-EOS
ERROR: No rubyforge config file found: #{@config_file}
Run 'rubyforge setup' to prepare your env for access to Rubyforge
 - See http://newgem.rubyforge.org/rubyforge.html for more details
      EOS
      exit
    end
  end
  RUBYFORGE_USERNAME.replace @config["username"]
end


REV = nil
# UNCOMMENT IF REQUIRED:
# REV = YAML.load(`svn info`)['Revision']
VERS = PoolParty::VERSION::STRING + (REV ? ".#{REV}" : "")
RDOC_OPTS = ['--quiet', '--title', 'PoolParty documentation',
    "--opname", "index.html",
    "--line-numbers",
    "--main", "README",
    "--inline-source"]

class Hoe
  def extra_deps
    @extra_deps.reject! { |x| Array(x).first == 'hoe' }
    @extra_deps
  end
end

# Generate all the Rake tasks
# Run 'rake -T' to see list of generated tasks (from gem root directory)
$hoe = Hoe.new(GEM_NAME, VERS) do |p|
  p.developer(AUTHOR, EMAIL)
  p.description = DESCRIPTION
  p.summary = DESCRIPTION
  p.url = HOMEPATH
  p.rubyforge_name = RUBYFORGE_PROJECT if RUBYFORGE_PROJECT
  p.test_globs = ["test/**/test_*.rb"]
  p.clean_globs |= ['**/.*.sw?', '*.gem', '.config', '**/.DS_Store']  #An array of file patterns to delete on clean.

  # == Optional
  p.changes = p.paragraphs_of("History.txt", 0..1).join("\n\n")
  p.extra_deps = EXTRA_DEPENDENCIES

    #p.spec_extras = {}    # A hash of extra values to set in the gemspec.
  end

CHANGES = $hoe.paragraphs_of('History.txt', 0..1).join("\\n\\n")
PATH    = (RUBYFORGE_PROJECT == GEM_NAME) ? RUBYFORGE_PROJECT : "#{RUBYFORGE_PROJECT}/#{GEM_NAME}"
$hoe.remote_rdoc_dir = File.join(PATH.gsub(/^#{RUBYFORGE_PROJECT}\/?/,''), 'rdoc')
$hoe.rsync_args = '-av --delete --ignore-errors'
$hoe.spec.post_install_message = File.open(File.dirname(__FILE__) + "/../PostInstall.txt").read rescue ""

# # Gemspec creator
# spec = Gem::Specification.new do |s|
#   s.name = GEM_NAME
#   s.version = VERS
#   s.platform = Gem::Platform::RUBY
#   s.has_rdoc = true
#   s.extra_rdoc_files = ["README.txt", "License.txt", 'History.txt']
#   s.summary = DESCRIPTION
#   s.description = s.summary
#   s.author = AUTHOR
#   s.email = EMAIL
#   s.homepage = HOMEPATH
#   
#   # Uncomment this to add a dependency
#   EXTRA_DEPENDENCIES.each do |arr|
#     s.add_runtime_dependency arr
#   end  
#   # s.extra_deps = EXTRA_DEPENDENCIES
#   
#   s.require_path = 'lib'
#   s.autorequire = GEM_NAME
#   s.files = %w(Rakefile History.txt README.txt) + Dir.glob("{examples,lib,specs,tasks,script,generators,bin}/**/*")
# end
#  
# Rake::GemPackageTask.new(spec) do |pkg|
#   pkg.gem_spec = spec
# end
#   
# desc "create a gemspec file"
# task :make_spec do
#   ::File.unlink "#{GEM_NAME.downcase}.gemspec" if ::File.exists?("#{GEM_NAME.downcase}.gemspec")
#   ::File.open("#{GEM_NAME.downcase}.gemspec", "w+") do |file|
#     file.puts spec.to_ruby
#   end
# end