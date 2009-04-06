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
namespace(:poolparty) do
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