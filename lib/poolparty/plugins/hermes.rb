=begin rdoc
=end

module PoolParty
  module Resources
    
    class Hermes < Resource
      
      default_options(
        :name => nil
      )
      
      def after_loaded



        # has_exec(:name => "git-#{name}", :creates => creates_dir ) do
        #   # Cloud, GitRepos, Exec
        #   if requires_user
        #     command("git clone #{requires_user}@#{repository} #{dir}")
        #   else
        #     command("cd #{dir} && git clone #{repository}")
        #   end
        #
        #   cwd "#{dir if dir}"          
        #   requires get_directory("#{dir}")
        # end
      end
      
    end
  end
end
