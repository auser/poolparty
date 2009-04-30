module PoolParty    
=begin rdoc

Resources are items that poolparty can make available in your clouds.
Resources are used in the <tt>clouds.rb</tt>, and in Plugins.
Plugins are collections of resources.

All of these resources can be defined as <tt>has_resource</tt> and <tt>does_not_have_resource</tt>:

* <tt>has_</tt>
  example <tt>has_file(...)</tt>
* <tt>does_not_have_</tt>
  For example: <tt>does_not_have_file(...)</tt>

== Resources


see PoolParty::Resources::File

* +has_file+ PoolParty::Resources::File
* +has_package+ PoolParty::Resources::Package
* +has_gempackage+ PoolParty::Resources::GempackageResource
* +has_exec+ PoolParty::Resources::Exec
* +has_variable+ (for templates) PoolParty::Resources::Variable
* +has_line_in_file+ PoolParty::Resources::LineInFile
* +has_remotefile+ PoolParty::Resources::Remotefile
* +hash_cron+ PoolParty::Resources::Cron
* +has_host+ PoolParty::Resources::Host
* +has_service+ PoolParty::Resources::Service
* +has_symlink+ PoolParty::Resources::Symlink
* +has_directory+ PoolParty::Resources::Directory
* +has_deploy_directory+ PoolParty::Resources::Deploydirectory
* +has_mount+ PoolParty::Resources::Mount

== Helper methods
    * execute_if

=end

  module Resources
  end
end
