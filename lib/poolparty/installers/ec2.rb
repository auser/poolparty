module PoolParty
  module Installers
    class Ec2 < BaseInstaller
      
      def commands
        [
          :get_access_key, :get_secret_access_key, :show_cert_message,
          :get_keypair, :write_clouds_rb
        ]
      end
      
      def get_access_key        
        key_help_str = <<-EOE
Now you need to get your keys from Amazon AWS.
If you don't already have your keys setup, navigate to http://aws.amazon.com
and click on Your Account. Click on the Access Identifiers tab and find:
Your Access Key ID. This is your access key.
        EOE
        access_key_str = <<-EOE
Since you'll be using ec2, we'll have to set some things up before
we can get going on PoolParty. Don't worry, this information will stay between us.

        EOE
        say access_key_str
        ask_with_help :message => "First, what's your access key?",
                      :help => key_help_str do |t|
          @access_key = t
        end
      end
      
      def get_secret_access_key
        key_help_str = <<-EOE
If you don't already have your keys setup, navigate to http://aws.amazon.com
and click on Your Account. Click on the Access Identifiers tab and find:
Your Secret Access Key ID. Click on the - show button.
This is your secret access key.
        EOE
        ask_with_help :message => "Awesome. Now what's your secret access key? ",
                      :help => key_help_str do |t|
          @secret_access_key = t
        end
      end
      
      def show_cert_message
        rescued_ask_str = <<-EOS
Super duper! You'll need to make sure you have yout X.509 certificate downloaded too.
Save this in your ~/.ec2 directory.

Press enter when you're ready
EOS

        cert_help_str = <<-EOE
To get your X.509 certificates, navigate to http://aws.amazon.com and login. Click on
Your Account and scroll down. The X.509 Certificate box is at the bottom. Make sure you
click download and save the cert-*.pem file. If you don't know or don't have the pk-*.pem file
you may have to recreate it. Not to worry though, it's super easy. Click on the Create New button. 
From there, you can download the cert-*.pem and the pk-*.pem files.

        EOE

        ask_with_help :message => rescued_ask_str,
                      :help => cert_help_str do |t|
          @cert = true
        end
      end
      
      def get_keypair
        begin
          ec2 = PoolParty::Remote::Ec2.ec2({:access_key => @access_key, :secret_access_key => @secret_access_key})
          keypairs = ec2.describe_keypairs["keySet"]["item"]
          keynames = keypairs.map {|k| k["keyName"]}  
        rescue Exception => e
          colored_say "There was an error: #{e}. Recheck your access_key and secret_access_key to make sure you copied them correctly"
          exit 1
        end

        key_str =<<-EOK

Finally, what's the name of the keypair you'd like to use?

You already have the following keypairs setup:
\t#{keynames.join("\n\t")}

You can use one of these keys, or create a new one.
EOK

        key_str_help =<<-EOH
  To make a new keypair, make sure you have the ec2 tools installed. You can create a new one by typing the command:
    ec2-add-keypair

  Save this into a file in your ~/.ec2 directory. Enter the name of the keypair here.
EOH
        ask_with_help :message => key_str,
                      :help => key_str_help do |t|
            @keypair = t
            say <<-EOE

In your clouds.rb, you can use this keypair in your clouds.

Create one for each cloud you want to use.

          EOE
        end
      end
      
      def write_clouds_rb
        clds =<<-EOC
pool :my_pool do
  cloud :my_app do

    # Copy these lines and source them in your .profile or .bashrc file
    # export AWS_ACCESS_KEY="#{@access_key}"
    # export AWS_SECRET_ACCESS_KEY="#{@secret_access_key}"
    # export EC2_PRIVATE_KEY=$(ls ~/.ec2/pk-*.pem)
    # export EC2_CERT=$(ls ~/.ec2/cert-*.pem)

    access_key "#{@access_key}"
    secret_access_key "#{@secret_access_key}"
    keypair "#{@keypair}"

    has_file "/etc/motd" do
      content "Welcome to your first PoolParty instance!"
    end
  end
end             
        EOC

        ::File.open("clouds.rb", "w") {|f| f << clds}
      end

    end
  end
end