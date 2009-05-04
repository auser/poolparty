#!/usr/bin/env ruby

output = %x[yes "" | /usr/bin/passenger-install-apache2-module]
keep_array = %w(LoadModule PassengerRoot PassengerRuby)

out_array = []
output.split(/\n/).each do |line|
  out_array << line if keep_array.reject {|l| line.include?(l) }.size != keep_array.size
end
# TODO: Clean this up, maybe use unpack(M*)
outlines = []
out_array.each do |l|
  outlines << l.gsub(/\e\[1m/, '').gsub(/\e\[0m\e\[37m\e\[40m/, '')
end

outlines.each do |line|
  puts line
end

File.open("/etc/apache2/conf.d/passenger.conf", "w+") {|f| f << outlines.join("\n")}