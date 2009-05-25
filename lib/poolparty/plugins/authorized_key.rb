=begin rdoc
  Authorized key
  
  Usage:
    has_authorized_key  :for_user         => "username",
                        :public_key_file  => "/Users/alerner/.ssh/id_rsa.pub"
=end
module PoolParty
  class Base
    plugin :authorized_key do
      
      default_options(
        :for_user => "root",
        :public_key_file => nil
      )
      
      def loaded(o={}, &block)
        has_directory "~#{for_user}/.ssh", :owner => for_user
        has_file      "~#{for_user}/.ssh/authorized_keys", :content => public_key_content
        has_line_in_file(:line => public_key_content, :file => "~#{for_user}/.ssh/authorized_keys")
      end
      
      def public_key_content
        open(public_key_file).read
      end
      
    end
  end
end