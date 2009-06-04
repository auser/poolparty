module PoolParty
  module Plugin

    class IncludeChefRecipe < Plugin
      def loaded(opts={}, &block)
        has_chef_recipe ::File.basename(name)
      end
      def before_configure
        ::Suitcase::Zipper.add(name, "chef/cookbooks") if ::File.exist?(name)
      end
    end
    
  end
end