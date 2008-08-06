# Tasks to be run on the server
namespace(:server) do                
  task :init  do
    PoolParty::Coordinator.init(false)
  end
  # bundle, upload and register your bundle on the server
  desc "Bundle, upload and register your ami"
  task :all => [:bundle, :upload, :register] do
    puts "== your ami is ready"
  end
  # Cleanup the /mnt directory
  desc "Clean the /mnt directory"
  task :clean_mnt do
    `rm -rf /mnt/image* img*`
  end
  # Before we can bundle, we have to make sure we have the cert and pk files
  desc "Ensure the required bundle files are present in /mnt"
  task :check_bundle_files do
    raise Exception.new("You must have a private key in your /mnt directory") unless File.exists?("/mnt/pk-*.pem")
    raise Exception.new("You must have your access key in your /mnt directory") unless File.exists?("/mnt/cert-*.pem")          
  end
  # Bundle the image
  desc "Bundle this image into the /mnt directory"
  task :bundle => [:clean_mnt, :check_bundle_files] do
    puts `ec2-bundle-vol -k /mnt/pk-*.pem -u '#{Planner.user_id}' -d /mnt -c /mnt/cert-*.pem -r i386`
  end
  # Upload the bundle into the app_name bucket
  desc "Upload the bundle to your bucket with a unique name: deletes old ami"
  task :upload => [:init, :delete_bucket] do
    puts `ec2-upload-bundle -b #{Planner.app_name} -m /mnt/image.manifest.xml -a #{Planner.access_key} -s #{Planner.secret_access_key}`
  end
  # Register the bucket with amazon and get back an ami
  desc "Register the bundle with amazon"
  task :register do
    puts `ec2-register -K /mnt/pk-*.pem -C /mnt/cert-*.pem #{Planner.app_name}/image.manifest.xml`
  end
  # Delete the bucket
  desc "Delete the bucket with the bundle under tha app name"
  task :delete_bucket do
    Planner.app_name.delete_bucket 
  end
end