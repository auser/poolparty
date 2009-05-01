# Basic poolparty template

pool :clouds do
  cloud :app do
    instances 1..3
    # keypair "#{ENV['HOME']}/ec2/ec2-keypair.pem" # or the full path to the keypair on the development machine
    ami 'ami-7cfd1a15'
    using :vmrun do
      vmx_hash({
        "/Users/alerner/Documents/vm/Ubuntu32bitVM.vmwarevm/Ubuntu32bitVM.vmx" => "192.168.248.133"
      })
    end
    
    # directories
    has_directory "/var/www"

    # packages
    has_package(:name => "logwatch")
    has_package(:name => "imagemagick")
    has_package(:name => "libmagick9-dev")
    has_package(:name => "librmagick-ruby")
    has_package(:name => "ffmpeg")
    has_package(:name => "monit")

    # gems
    has_gem_package(:name => "rmagick")
    has_gem_package(:name => "rubyist-aasm")
    has_gem_package(:name => "rvideo")
    has_gem_package(:name => "right_aws")
    has_gem_package(:name => "ap4r")
    has_gem_package(:name => "rmagick")
    
    # services
    has_service(:name => "monit")
  end

end