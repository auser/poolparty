require 'rubygems'
$:.unshift File.dirname(__FILE__)+'/../lib/'
require "poolparty"

pool :multiverse do
  
  cloud :front do
    instances 1
    has_file "/etc/motd", :content => "Welcome to your poolparty instance!"
    using :metavirt do
      # keypair 'multiverse_front'
      server_config({:host=>"abbot", :port=>3000})
      authorized_keys 'ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCTppECfx7Tb0zoviRfqFaePyAek6+ZktKkHiTHu/jkhG1s4q1oHEe89no21xLxuReyJrDlNe8rLxxZzoYCaAWRdhcqMR3BNqb2w2jpF4pH+bFj0557KrwWP6HSNpRRkyYhxLqZbuH/2t3TzkPevZbcfSYa09jIzqnmTruh9l1s+n5E3cNr/RDdDn7tv3Ok7mKN7GEjkK7F83Pt9xviHevg22xqzm99nS+hg6Kl/fQUTO6pOmC5x+9V47RJz1+9WdhGJ7M83zijX9rMnAwrR5LFoL6aZyyU0G71SpoIL5e8XD/jt1WNKFJOfG8YMLb3i03UABm/Q5Q30+R7UoRxSWRX'

      # using :vmrun do
      #   # vmx_files Dir[::File.expand_path("~/Documents/Virtual Machines.localized/metavirts/*/*.vmx")]
      #   vmx_files [
      #     "/Users/alerner/Documents/vm/Ubuntu32bitVM.vmwarevm/Ubuntu32bitVM.vmx"
      #     ]
      # end
      
      using :libvirt do
        image_id 'jaunty19'
      end
      
    end
  end
  
end

