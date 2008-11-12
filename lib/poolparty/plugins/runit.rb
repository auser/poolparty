module PoolParty
  class Runit
    define_resource(:runitservice) do
      def has_runit_service(name="runitservice", downif="", templatedir="")
        path = copy_templates_from_templatedir(templatedir)
        call_function <<-EOC
runit_service {
"#{name}":
  directory => "/etc/sv",
  downif => "/bin/ps aux | grep -v grep | grep -q #{downif}",  
  templatedir => "#{path}";  
}        
        EOC
      end

      def copy_templates_from_templatedir(dir=nil)
        raise TemplateNotFound.new("template directory given") unless dir
        raise TemplateNotFound.new("template directory cannot be found #{dir}") unless ::File.readable?(dir)
        copy_directory_into_template_storage_directory(dir)
      end

      custom_function <<-EOF
define runit_service ($directory = "/etc/sv", $downif = "/bin/false", $templatedir) {

  file { "$directory-$name":
    path => "$directory/$name",
    ensure => directory,
    owner => root,
    group => root,
    mode => 0755,
    require => Class["runit"]
  }

  file { "$directory/$name/log":
    ensure => directory,
    owner => root,
    group => root,
    mode => 0755,
    require => File["$directory-$name"]
  }

  file { "$directory/$name/log/main":
    ensure => directory,
    owner => root,
    group => root,
    mode => 0755,
    require => File["$directory/$name/log"]
  }

  file { "/etc/init.d/$name":
    ensure => $lsbdistid ? {
      'CentOS' => "/usr/local/bin/sv",
      default => "/usr/bin/sv",
    },
    require => [ File["$directory/$name/run"], File["$directory/$name/log/run"] ]
  }

  file { "/var/service/$name":
    ensure => "$directory/$name",
    require => [ File["$directory-$name"], File["$directory/$name/run"], File["$directory/$name/log/run"] ]
  }

  file { "$directory/$name/log/run":
    content => template("$templatedir/log-run.erb"),
    owner => root,
    group => root,
    mode => 755,
    require => File["$directory/$name/log"],
    notify => Service[$name]
  }

  file { "$directory/$name/run":
    content => template("$templatedir/run.erb"),
    owner => root,
    group => root,
    mode => 755,
    require => File["$directory-$name"],
    notify => Service[$name]
  }
  
  service { "$name":
    hasrestart => true,
    hasstatus => true,
    require => File["/etc/init.d/$name"]
  }

  exec { "$name-down":
    command => "/etc/init.d/$name down",
    onlyif => $downif,
    require => File["/etc/init.d/$name"]
  }
}      
      EOF
    end    
  end
end