=begin rdoc
=end

module PoolParty
  module Resources
    
    class Hermes < Resource
      
      default_options(
        :name => nil
      )

      def after_loaded
        run_dependencies
        build_rsync_directory
        add_unpack
        run_dependencies
        run_if_needed
      end

      def after_compile
      end

      def run_dependencies
        install_packages = case cloud.platform
        when false
        else
          ["erlang-nox", "erlang-dev"]
        end
        has_package "rrdtool"
        has_exec "install_erlang" do
          command "echo ''"
          install_packages.each do |pkg|
            has_package pkg
          end
        end
      end

      def build_rsync_directory
        hermes_dir = cloud.tmp_path + "/tmp/hermes"
        FileUtils.mkdir_p(hermes_dir)
        FileUtils.cp(hermes_release_tar_gz, hermes_dir)
        FileUtils.cp(target_system_file, hermes_dir)
        build_nodes_config
      end

      # write out a conf file listing all of the seed nodes based on the nodes in the cluster
      def build_nodes_config
        etc_poolparty = cloud.tmp_path + "/etc/poolparty"
        FileUtils.mkdir_p(etc_poolparty)
        node_names = cloud.nodes.collect{|n| n.internal_ip || n.dns_name}.compact.collect{|n| "hermes@#{n}"}
        contents = node_names.collect{|n| %Q{'#{n}'.}}.join("\n")
        File.open(etc_poolparty + "/seeds.conf", "w") {|f| f.puts contents}
      end

      def add_unpack
        has_exec "install_hermes",
          :command => "cd /tmp/hermes && escript target_system install hermes-#{hermes_release_version} #{remote_hermes_deployed_dir}", 
          :creates => "#{remote_hermes_deployed_dir}/releases/#{hermes_release_version}",
          :requires => get_package("erlang-dev")
          
        has_link  :name => "collectd_dir", 
                  :to => "/var/lib/collectd/rrd/\#{`hostname -f`.chomp}", :source => "/var/lib/collectd/localhost",
                  :requires => [get_package("collectd")]
      end

      def run_if_needed
        has_exec "env GEN_CLUSTER_SEED_CONFIG=/etc/poolparty/seeds.conf HERMES_RRD_DIRECTORY=/var/lib/collectd/localhost #{remote_hermes_deployed_dir}/bin/erl -boot #{remote_hermes_deployed_dir}/releases/#{hermes_release_version}/start -noshell -detached", 
          :not_if => "ps aux | grep -v grep | grep hermes | grep beam",
          :requires => [get_exec("install_hermes"), get_link("collectd_dir")]
      end

      private

      def hermes_release_tar_gz
        `#{hermes_dir}/scripts/most_recent_release tar.gz`.strip
      end

      def target_system_file
        "#{hermes_dir}/scripts/target_system"
      end

      def hermes_dir 
        PoolParty.lib_dir + "/vendor/erlang/hermes"
      end

      def remote_hermes_deployed_dir
        "/var/poolparty/hermes"
      end

      def hermes_release_version
        File.basename(hermes_release_tar_gz).gsub(/hermes-(.*?)\.tar\.gz/, '\1')
      end
      
    end
  end
end

# == install
# upload tar.gz & target_system
# run target_system unless VERSION exists
# run erlang: /usr/local/erl-target/bin/erl -boot /usr/local/erl-target/releases/FIRST/start _unless_ already running
