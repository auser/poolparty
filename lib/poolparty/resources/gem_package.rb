=begin rdoc
== Gempackage

# Allows you to specify a gem to be installed
# You can optionally pass a :download_url if you want a specific gem or version installed

== Usage

  has_gempackage(:name => '...') do
    # More options. 
    # This block is optional
  end

== Options

* <tt>name</tt> The gem name
* <tt>download_url</tt> The location of the gem to download and server across the instances
* <tt>version</tt> The gem version requirement (optional and useless if download_url is given)
* <tt>source</tt> The gem source (optional and useless matter if download_url is given)
* <tt>bin</tt> Specify exact gem binary
* <tt>jruby</tt> Will set 'bin' to 'jruby -S gem'

== Example
  has_gempackage(:name => 'rake', 
    :download_url => 'http://rubyforge.org/frs/download.php/43954/rake-0.8.3.gem')
=end
module PoolParty
  module Resources
    
    class GemPackage < Resource
      
      default_options(
        :action         => :install,
        :version        => nil,
        :response_file  => nil,
        :source         => nil,
        :options        => nil
      )
      
      def print_to_chef
        str = <<-EOE
gem_package "<%= name %>" do
  action <%= print_variable(action ? action : (exists ? :install : :remove)) %>
EOE
        str << "  options <%= print_variable(options) %>\n" if options
        str << "  version <%= print_variable(version) %>\n" if version
        str << "  source <%= print_variable(source) %>\n" if source
        str << "  response_file <%= print_variable(response_file) %>\n" if response_file
        str << "end"
      end
      
    end
    
  end
end
