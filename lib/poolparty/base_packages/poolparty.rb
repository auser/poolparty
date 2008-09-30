module PoolParty
  class Base
    plugin :poolparty do
      
      def enable
        has_gem(:name => "poolparty")
        
        (self.respond_to?(:instances_list) ? self : parent).instances_list.each do |ri|
          has_host({:name => ri.name, :ip => ri.ip})
        end
      end
      
    end  
  end
end