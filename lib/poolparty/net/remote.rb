module PoolParty
  module Remote
    
    def using(type)
      if available_bases.include?(type.to_sym)
        unless using_remoter?
          self.instance_eval do |t|
            t.extend "#{type}".preserved_module_constant if type
            "#{type}".preserved_module_constant.extend(RemoterBase)
          end
          @remote_base = type          
        end
      else
        puts "Unknown remote base" 
      end
    end
    
    def available_bases
      remote_bases
    end
    
    def using_remoter?
      @remote_base ||= nil
    end
        
  end
  
  def register_remote_base(*args)
    args.each do |arg|
      (remote_bases << "#{arg}".downcase.to_sym)
    end    
  end
  def remote_bases
    $remote_bases ||= []
  end
  
end

Dir["#{File.dirname(__FILE__)}/remote_bases/*.rb"].each {|base| require base }
