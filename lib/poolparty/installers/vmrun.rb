module PoolParty
  module Installers
    class Vmrun < BaseInstaller
      
      def commands
        [
          :get_vmrun_file, :get_vm_ip, :get_key,
          :add_vmware_fusion_to_path, :start_vmrun_instance,
          :scp_key, :test_login, :fix_eth0
        ]
      end
      
      private
      
      def get_vmrun_file
        vmrun_file_help =<<-EOV
Vmware uses a vmwarevm file to keep information about the vmware instance. To find the vmwarevm file, 
navigate to vmware and find the vm you'd like to use. Find this in finder and paste that here.
        EOV
                
        vmrun_file = <<-EOE
Awesome. What's the path to your vmwarevm file?#{if default_vmrun_file
 "\nIs this it: #{default_vmrun_file}? (Press enter to accept default)" 
end}
        EOE
        ask_with_help :message => vmrun_file, :help => vmrun_file_help do |t|
          @vmrun_file = default_vmrun_file if t.nil? || t.empty?
        end
      end
      
      def get_vm_ip
        ip_help =<<-EOV
Right now, vmrun, the remoter base needs an explicitly set ip. Log into your vm and type ifconfig. Copy and paste that here.
        EOV
        ask_with_help :message => "Now, what's the ip of your vm?",
                      :validate => /\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}|h|H/,
                      :help => ip_help do |i|
          @ip = i
        end
      end
      
      def get_key
        key_help =<<-EOV
Finally, we'll set somethings up here shortly, but first we'll need to know where your public key is. We'll assume it's 
at ~/.ssh/id_rsa.pub. If this is true, then just press enter. Otherwise, enter the path of your public key.        
        EOV
        
        ask_with_help :message => "What keypair would you like to use? (default: ~/.ssh/id_rsa.pub)",
                      :help => key_help do |k|
          @key = k.empty? ? ::File.expand_path("~/.ssh/id_rsa.pub") : k
        end
      end
      
      def scp_key
        colored_say "Sending key to vmware instance..."
        o = %x{scp #{@key} root@#{@ip}:~/.ssh/authorized_keys}
        sleep 2
      end
      
      def test_login
        raise "Could not connect to #{@ip}. Check to make sure you set the ip properly" unless ping_port(@ip, 22, 3)
      end
      
      # TODO: Fix this
      def fix_eth0    
      end
      
      def add_vmware_fusion_to_path
        colored_say "Exporting path with the VMware Fusion. You will want to add this to your path by adding \n\texport PATH=/Library/Application Support/VMware Fusion:$PATH\n to your .profile or .bashrc file"
        o = %x{export PATH=/Library/Application\\\ Support/VMware\\\ Fusion:$PATH}
        sleep 2
      end
      
      def start_vmrun_instance
        vmrun_path = `which vmrun`.chomp!
        command = "#{vmrun_path.path_quote} start #{@vmrun_file.path_quote}"
        %x{#{command}}
      end
      
      def closing_message
        vmx_file = Dir["#{@vmrun_file}/*.vmx"].first
          clds =<<-EOC
pool :my_pool do
  cloud :my_app do
    using :vmrun do
      vmx_hash({
        "#{vmx_file}" => "#{@ip}"
      })
    end

    has_file "/etc/motd" do
      content "Welcome to your first PoolParty instance!"
    end
  end
end             
                EOC

        ::File.open("clouds.rb", "w") {|f| f << clds}
        super
      end
      
      def default_vmrun_file
        @default_vmrun_file ||= find_default_vmrun_file rescue nil
      end
      
      def find_default_vmrun_file
        Dir["#{::File.expand_path("~")}/Documents/Virtual\ Machines.localized/*.vmwarevm"].first
      end
      
    end
  end
end