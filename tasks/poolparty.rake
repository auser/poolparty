desc "Run the specs"
task :slow_spec do
  stats = {:example=>0, :failures=>0, :pending=>0}
  Dir["#{::File.dirname(__FILE__)}/../spec/poolparty/**/*_spec.rb"].each do |sp|
    puts "---------------- #{::File.basename(sp)} ----------------"
    results = `spec #{sp}`
    results.match(/([1-9]+0?)\sfailures|errors/)
    stats[:failures] += $i.to_i
    puts results
  end
  puts "#{stats[:failures]} total errors"
end

desc "Generate thrift docs"
task :thrift do
  [
    "cd #{File.dirname(__FILE__)}/../lib/proto && thrift --gen rb --gen py --gen erl poolparty.thrift",
    "cp -R #{File.dirname(__FILE__)}/../lib/proto/gen-erl/*.hrl #{File.dirname(__FILE__)}/../examples/thrift/erlang/include",
    "cp -R #{File.dirname(__FILE__)}/../lib/proto/gen-erl/*.erl #{File.dirname(__FILE__)}/../examples/thrift/erlang/src"
  ].each do |cmd|
    `#{cmd}`
  end
end

namespace(:pp) do
  task :build_gem => ["poolparty:vendor:setup", "poolparty:vendor:update", :gemspec, :build]
  
  namespace(:setup) do
    desc "Generate a manifest for quicker loading times"
    task :manifest do
      $GENERATING_MANIFEST = true
      out = capture_stdout do
        $_poolparty_load_directories.each do |dir|
          PoolParty.require_directory ::File.join(::File.dirname(__FILE__), '../lib/poolparty', dir)
        end
      end
      ::File.open(::File.join(::File.dirname(__FILE__), '../config', "manifest.pp"), "w+") {|f| f << out.map {|f| "#{f}"} }
      puts "Manifest created"
    end
  end
  
  namespace :vendor do
    desc "Fetch all the submodules"
    task :submodules do
      `git submodule update --init && cd vendor/gems/git-style-binaries && git submodule update --init`
    end
    desc "Initialize the submodules"
    task :setup do
      `git submodule init`
    end
    desc "Update the submodules"
    task :update do
      Dir["#{::File.dirname(__FILE__)}/../vendor/gems/*"].each do |dir|
        puts "Fetching #{dir}..."
        `cd #{dir} && git fetch && git rebase origin/master`
      end
      # `git submodule update`
    end
  end
  namespace :deps do
    desc "Get all the edge packages required and stash them in pkg/"
    task :grab do
      require 'rubygems/dependency_installer'
      require "#{File.dirname(__FILE__)}/../lib/poolparty"
      di = Gem::DependencyInstaller.new
      to = "#{::File.dirname(__FILE__)}/../pkg"
      existing_gems = Dir["#{to}/*.gem"]
      PoolParty::Provision::BootStrapper.gem_list.each do |g|
        unless existing_gems.find {|f| f =~ /#{g}/}
          puts "Downloading #{g}"
          spec, url = di.find_spec_by_name_and_version(g).first
          f = begin
            Gem::RemoteFetcher.fetcher.download spec, "http://gems.github.com", to
          rescue Exception => e
            Gem::RemoteFetcher.fetcher.download spec, url, to
          end
          ::FileUtils.mv f, "#{::File.dirname(__FILE__)}/../pkg/#{::File.basename(f)}"          
        end
      end
      FileUtils.rm_rf "#{to}/cache"
    end
    task :clean_gem_cache do
      gem_location = "#{::File.dirname(__FILE__)}/../vendor/dependencies"
      cache_dir = "#{gem_location}/cache"
      Dir["#{cache_dir}/*.gem"].each {|file| ::File.unlink file }
    end
    desc "Update dependencies gem"
    task :update => [:clean_gem_cache] do
      gem_location = "#{::File.dirname(__FILE__)}/../vendor/dependencies"
      Suitcase::Zipper.gems open("#{gem_location}/gems_list").read.split("\n"), gem_location
      Suitcase::Zipper.packages ['http://rubyforge.org/frs/download.php/45905/rubygems-1.3.1.tgz'], gem_location
    end
  end
end