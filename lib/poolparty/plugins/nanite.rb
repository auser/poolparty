module PoolParty
  class Nanite
    
    plugin :nanite do
      
      def loaded(o={}, &block)
        has_package "erlang"

        # TODO: change this with has_gem_package
        has_exec "install nanite rubygem" do
          command "cd ~ && git clone git://github.com/ezmobius/nanite.git && cd nanite/ && rake gem && gem install pkg/nanite*.gem"
          not_if "gem list -l | grep nanite"
        end

        has_gem_package "eventmachine"
        has_gem_package "amqp"

        has_package "python"

        has_exec "install easy_install" do
          command "cd ~ && wget http://pypi.python.org/packages/2.5/s/setuptools/setuptools-0.6c9-py2.5.egg && sh setuptools-0.6c9-py2.5.egg"
          not_if "which easy_install"
        end

        has_exec "install simplejson" do
          command "cd ~ && easy_install simplejson"
        end

        has_exec "install rabbitmq" do
          command "cd ~ && wget http://www.rabbitmq.com/releases/rabbitmq-server/v1.5.3/rabbitmq-server-1.5.3.tar.gz && cd /usr/local/lib/erlang/lib && tar -zxf ~/rabbitmq-server-1.5.3.tar.gz && cd rabbitmq-server-1.5.3 && make"
          not_if "which easy_install"
        end
        
        has_gem_package "ezmobius-nanite"
        
      end
            
    end
    
  end
end