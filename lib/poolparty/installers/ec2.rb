module PoolParty
  module Installers
    class Ec2 < Installer
      
      def steps
        [
          :find_ec2_directory, :ask_for_access_key, :ask_for_private_access_key,
          :show_env_setup
        ]
      end
      
      def self.name
        "Ec2"
      end
      
      def self.description
        "Ec2 installer"
      end
      
      def find_ec2_directory
        msg = "We found the following vmware files in the default vmware directory.\nChoose one of these to use as your vmrun file or select other\n<line>"
        
        directories = {}
        default_ec2_directories.each_with_index do |file,idx|
          directories.merge!(idx+1 => file)
        end
        
        base = choose(msg, directories)
        @ec2_directory = base == :other ? ask_for_ec2_directory : base
      end
      
      def ask_for_access_key
        access_key_help =<<-EOV
EC2 uses an access key to identify you and allows you to start and stop instances.
        EOV

        access_key = <<-EOE
What is your access key?
        EOE
        ask_with_help :message => access_key, :help => access_key_help do |k|
          @access_key = k
        end
      end
      
      def ask_for_private_access_key
        private_access_key_help =<<-EOV
EC2 uses a private access key to identify you and allows you to start and stop instances.
        EOV

        private_access_key = <<-EOE
What is your private access key?
        EOE
        ask_with_help :message => private_access_key, :help => private_access_key_help do |k|
          @secret_access_key = k
        end
      end
      
      
      def ask_for_ec2_directory
        ec2_directory_help =<<-EOV
Ec2 needs to know where you store your certificates and private keys. Amazon expects these to be in the ~/.ec2 directory. We suggest a subdirectory of the ~/.ec2 directory so you can separate ec2 accounts. 
        EOV

        ec2_directory_msg = <<-EOE
What's path to your ec2 directory with your cert and pk files?
        EOE
        ask_with_help :message => ec2_directory_msg, :help => ec2_directory_help
      end

      
      def show_env_setup
        colored_say <<-EOE
<line>
  
  Setup your environment:
  
  export EC2_ACCESS_KEY=#{@access_key}
  export EC2_SECRET_KEY=#{@secret_access_key}

  export EC2_PRIVATE_KEY=$(ls #{@ec2_directory}/pk-*.pem)
  export EC2_CERT=$(ls #{@ec2_directory}/cert-*.pem)
  
<line>
        EOE
        exit 0
      end
      
      private
      
      def default_ec2_directories
        @default_ec2_directories ||= find_default_ec2_directories rescue nil
      end
      
      def find_default_ec2_directories
        Dir["#{::File.expand_path("~")}/.ec2/*"].reject {|f| File.file?(f) }
      end
      
    end
  end
end