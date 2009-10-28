
simple.rb
===
The simple.rb configuration will create an ec2 auto scaling group that will launch the minimum_instances (1 in this case,) inside a security group. The default naming of most objects, such as security_groups, auto scaling groups, launch_configurations and load_balancers and keypairs follow the poolname-cloudname convention.  For example, the cloud below will create a security-group named 'pooolparty-simple'.  The names can be overridden, important if you want more than one.   If a keypair does not exist, it will be created.

Since auto scaling is specified in this cloud, poolparty will not start the instance directly.  Instead, poolparty will create an ec2_launch_configuration and auto scaling group. 

Some important notes:  Some of the methods used inside the cloud block are specific to the cloud provider being used, ec2 in this example.  The cloud_provider methods are added when the cloud_provider is specified, therefore you must be sure to specify the cloud_provider before any cloud_provider specific methods.

User_data.  While not a specific feature of poolparty, we often use user_data scripts to configure instance in the cloud.  This allows for separation of concerns:
  - PoolParty takes care of setting up an infrastructure of firewalled autoscaling, load balanced instances across multiple data centers.
  - Userdata scripts configure the instances, optionally pulling in from a repository of additional tasks.    Userdata script can be used in conjunction with [runrul](http://alestic.com/2009/08/runurl) to run scripts form a shared repository, even installing and run chef if desired.
  
A benefit of userdata scripted instance configuration is that it does not require active management and monitoring. You just define your infrastructure with poolparty and let the autoscaling take care of the rest.

chef_cloud.rb
===
The chef_cloud.rb does not define an autoscaling group.  Since there is no autoscaling group, poolparty will launch the instances (ec2_run_instance.) and wait for ssh to be available on all the launched hosts.
Next, if a chef-repo was specified:
  
  1.  The repo will be uploaded the /etc/chef on the instances.
  2.  Ssh to the instances and call chef -c /etc/chef/solo.rb -j /etc/chef/dna.json

The dna.json can be populated either by simply editing the file dna.json in your chef-repo, or you can specify chef_attributes in your cloud block.  for example:

    chef_attributes :apache2 => {:listen_ports => ["80", "8080"]} "8080"]}
    
If you specify chef_attributes they will be compiled into a a role namded for the cloud at /etc/chef/roles/cloudname.json, and a dna.json file that will execute the clouds role on the instance.

Notes, Tips and Comments:
===
The goal of having an completely idempotent declarative configuration system is particularly valuable when you need to manage and update running servers. In the cloud we have found that often, it is easier to simply throw away the server and start a new one from a fresh state.

  1. Update userdata scripts and launch configs
  2. Terminate the instances (1 at a time for a rolling restart, or all at once)
  3. Let the autoscaling (or client side poolparty) start new instances with the new userdata

Since the instances are always starting from a known state (a base ami) the configuration scripts can be much simpler, simple shell scripts, possibly executed as runurls.

If runurls are being called from the userdata script, another way to update the cloud instances configuration is to update the runurl (if you are hosting it, which is recommended) you can terminate instances and the newly launched instances will use the updated runurl/

We try and keep private data out of the runurl scripts, opting to supply this data either as environment variables (possibly set by the userdata script,) as command line arguments to runrul (i.e. $1, $2 etc), or variables contained in a separate file our userdata script can download. 

When the runurl cannot be public, and we do not want to embed all the runurl scripts in the userdata, we have used an authenticate only aws user.  That is create an aws user, but do not provide a credit card or sign up for any services. Even tho the user can not create any resources, it can authenticate.  We put the authenticate only user credentials in the userdata file, and  a small bit of code at the top of our userdata file to fetch and install an s3 utility, such as s3cmd. Then the s3cmd can download private readonly s3 urls.

chef
---
If you want to use chef, one way to do so is to put code like the following in your userdata

    # OPTIONAL: install chef-solo
    export CLOUD=${1:-'poolparty'}
    sudo gem install ohai chef --source http://gems.opscode.com --no-rdoc --no-ri
    mkdir -p /etc/chef/cookpooks
    s3sync -r https://mybucket.s3.amazonaws.com/chef-repo /etc/chef/chef
    s3sync https://cloudteam.s3.amazonaws.com/chef-repo/$CLOUD.json /etc/chef/dna.json
    chef-solo -c /etc/chef/chef/solo.rb -j /etc/chef/chef/dna.json

