case node[:platform]
when "debian", "ubuntu"
  # package "policykit"
  # package "emacs22-nox"
  require_recipe "apt"
else 
  # package "emacs-nox"
end

package "vim"
package "tree"
package "nmap"
package "ngrep"
package "irb"
package "curl"
package "man-db"
package "strace"
package "host"
package "lsof"
package "gdb"
package "socat"
package "procmail"
