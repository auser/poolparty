set[:ganglia][:ganglia_this_nodes_private_ip] = ipaddress

# set[:ganglia][:ganglia_gmetad_data_sources]  = "foo"
# set[:ganglia][:ganglia_pool_name] = "name"
# set[:ganglia][:ganglia_cloud_name]  = "foo"
# set[:ganglia][:ganglia_first_node_in_clusters_ip] = "ip"
# set[:ganglia][:gmond_user] = "root" 
# set[:ganglia][:gmetad_user] = "root" 
# set[:ganglia][:gmond_web_root] = "/var/www/ganglia" 
# set[:ganglia][:ganglia_gmetad_data_sources] = %Q{data_source "a name goes here todo" 127.0.0.1} # space separated ip/hostnames

# # hopefully we can delete the attributes below now that the recipes are split up
# set[:ganglia][:enable_web_interface] = false
# set[:ganglia][:enable_gmetad] = false
