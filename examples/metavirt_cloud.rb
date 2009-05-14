require File.dirname(__FILE__)+"/../lib/poolparty"

pool :poolparty do
  # verbose true
  # debug true
  
  cloud :metavirt do
    keypair "~/.ec2oncourse/r_and_d"
    instances 1
    using :metavirt do
      public_key 'ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC1iNGRmR5r8RE0HKk/DoanV+GQ58OhwMLZ3K9NkHRASTMT75nusMaQmks6QN5pBQVzbJGgQ7cck4ciFkbwRSSkHkjB8LY0FjGB3Rfjq4mNzzWpxfPzI1xiPQVM0Df0n44q4wP6WzVQRGAMOgNqyFKsmYNde0y/yFLsIL1XUImV94eQpxdC2YIo0RJcLLLEUniwYTj/88vc7c1bArRPhQqOkj/rK97DAUARKZTMbgobRUUZSZzC86Z0Dz7w6oJ33+QycJaqPtkegJRRSGi6DR1Vu6t+a2ywY2ikrD98c8eYt/Tgm8NGTHUAjteBRzUDdFNtqAA7FTgiFgLTkwyarbn3'
      server_config = {:host=>'localhost', :port=>3000}
      vmx_files = [
        ::File.expand_path("~/Documents/Virtual\ Machines.localized/Ubuntu-jaunty.vmwarevm/Ubuntu-jaunty.vmx")
      ]
    end
  end
  
end