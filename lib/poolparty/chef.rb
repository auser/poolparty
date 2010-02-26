module PoolParty
  class Chef < Base
    def self.types
      return [:solo]
    end
    
    def self.get_chef(type,cloud,&block)
      ("Chef" + type.to_s.capitalize).constantize(PoolParty).send(:new,type,:cloud => cloud,&block)
    end
    # Chef    
    
    def attributes(hsh={}, &block)
      @attributes ||= ChefAttribute.new(hsh, &block)
    end

    def override_attributes(hsh={}, &block)
      @override_attributes ||= ChefAttribute.new(hsh, &block)
    end
    
    # Adds a chef recipe to the cloud
    #
    # The hsh parameter is inserted into the override_attributes.
    # The insertion is performed as follows. If
    # the recipe name = "foo::bar" then effectively the call is
    #
    # override_attributes.merge! { :foo => { :bar => hsh } }
    def recipe(recipe_name, hsh={})
      _recipes << recipe_name unless _recipes.include?(recipe_name)

      head = {}
      tail = head
      recipe_name.split("::").each do |key|
        unless key == "default"
          n = {}
          tail[key] = n
          tail = n
        end
      end
      tail.replace hsh

      override_attributes.merge!(head) unless hsh.empty?
    end
    
    def recipes(*recipes)
      recipes.each do |r|
        recipe(r)
      end
    end

    def node_run!(remote_instance)
      envhash = {
        :GEM_BIN => %q%$(gem env | grep "EXECUTABLE DIRECTORY" | awk "{print \\$4}")%
      }
      remote_instance.ssh([chef_cmd.strip.squeeze(' ')], :env => envhash )
    end

    def node_bootsrapped?(remote_instance)
      remote_instance.ssh(["(gem list; dpkg -l chef) | grep chef"], :do_sudo => false).empty?
    end
   def node_bootstrap!(remote_instance)
      remote_instance.ssh([
        'apt-get update',
        'apt-get autoremove -y',
        'apt-get install -y ruby ruby-dev rubygems git-core libopenssl-ruby',
        'gem sources -a http://gems.opscode.com',
        'gem install chef ohai --no-rdoc --no-ri' ])
      remote_instance.ssh(remote_instance.bootstrap_gems.collect { |gem| "gem install #{gem} --no-rdoc --no-ri" } )
    end
    private
    
    def _recipes
      @_recipes ||= []
    end

    def method_missing(m,*args,&block)
      cloud.send(m,*args,&block) if cloud.respond_to?(m)
    end
    
  end
end
