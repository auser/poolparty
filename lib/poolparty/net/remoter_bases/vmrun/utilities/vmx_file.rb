# class VmxFile
#   include Dslify
#   
#   default_options(
#     ".encoding" => "UTF-8",
#     "config.version" => "8",
#     "virtualHW.version" => "6",
#     "guestOS" => "other26xlinux",
#     "displayName" => "PoolParty vmx",
#     "vumvcpus" => "1",
#     "memsize" => "256",
#     "MemAllowAutoScaleDown" => "TRUE",
#     "MemTrimRate" => "-1",
#     "gui.powerOnAtStartup" => "FALSE",
#     "gui.fullScreenAtPowerOn" => "FALSE",
#     "gui.exitAtPowerOff" => "FALSE",
#     "uuid.action" => "create",
#     # Settings for VMware Tools
#     "tools.remindInstall" => "FALSE",
#     "tools.upgrade.policy" => "upgradeAtPowerCycle",
#     # Startup hints interfers with automatic startup of a virtual machine
#     # This setting has no effect in VMware Player
#     "hints.hideAll" => "TRUE",
# 
#     # Enable time synchronization between computer
#     # and virtual machine
#     "tools.syncTime" => "TRUE",
# 
#     # USB settings
#     # This config activates USB
#     "usb.present" => "TRUE",
#     "usb.generic.autoconnect" => "FALSE",
# 
#     # First serial port, physical COM1 is available
#     "serial0.present" => "FALSE",
#     "serial0.fileName" => "Auto Detect",
#     "serial0.autodetect" => "TRUE",
#     "serial0.hardwareFlowControl" => "TRUE",
# 
#     # Optional second serial port, physical COM2 is not available
#     "serial1.present" => "FALSE",
# 
#     # First parallel port, physical LPT1 is available
#     "parallel0.present" => "FALSE",
#     "parallel0.fileName" => "Auto Detect",
#     "parallel0.autodetect" => "TRUE",
#     "parallel0.bidirectional" => "TRUE",
# 
#     # Sound settings
#     "sound.present" => "TRUE",
#     "sound.fileName" => "-1",
#     "sound.autodetect" => "TRUE",
# 
#     # Logging
#     # This config activates logging, and keeps last log
#     "logging" => "TRUE",
#     "log.fileName" => "PoolParty.log",
#     "log.append" => "TRUE",
#     "log.keepOld" => "3",
# 
#     # These settings decides interaction between your
#     # computer and the virtual machine
#     "isolation.tools.hgfs.disable" => "FALSE",
#     "isolation.tools.dnd.disable" => "FALSE",
#     "isolation.tools.copy.enable" => "TRUE",
#     "isolation.tools.paste.enabled" => "TRUE",
# 
#     # Other default settings
#     "svga.autodetect" => "TRUE",
#     "mks.keyboardFilter" => "allow",
#     "snapshot.action" => "autoCommit",
# 
#     # First network interface card
#     "ethernet0.present" => "TRUE",
#     "ethernet0.virtualDev" => "vlance",
#     "ethernet0.connectionType" => "nat",
#     "ethernet0.addressType" => "generated",
#     "ethernet0.generatedAddressOffset" => "0",
# 
#     # Settings for physical floppy drive
#     "floppy0.present" => "FALSE",
# 
#     # First IDE disk, size 4800Mb
#     "ide0:0.present" => "TRUE",
#     "ide0:0.fileName" => "PoolParty.vmdk",
#     "ide0:0.mode" => "persistent",
#     "ide0:0.startConnected" => "TRUE",
#     "ide0:0.writeThrough" => "TRUE",
#     
#     "checkpoint.vmState" => "",
#     "ethernet0.generatedAddress" => "00:0c:29:73:79:fb",
#     "virtualHW.productCompatibility" => "hosted",
#     "ide0:0.redo" => "",
#     "vmotion.checkpointFBSize" => "65536000",
#     "parallel0.startConnected" => "FALSE",
#     "serial0.startConnected" => "FALSE"
#   )
#   
#   def initialize(o={})
#     dsl_options({:vmx_file => default_dsl_options}.merge(o))
#   end
#   
#   def set(k,v)
#     options[:vmx_file]["#{k}"] = v
#   end
#   
#   def compile
#     ::File.open("#{base_directory}/#{name}/#{name}.vmx", "w") {|f| f << to_vmx }
#   end
#   def to_vmx
#     out = []
#     options[:vmx_file].each do |k,v|
#       out << "#{k}=\"#{v}\""
#     end
#     out.join("\n")
#   end
# end