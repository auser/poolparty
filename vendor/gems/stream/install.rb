#!/usr/bin/env ruby

require 'rbconfig'
require 'ftools'
require 'find'

SRC_BASE = 'lib'
INSTDIR = File.join Config::CONFIG['sitedir']

def install
  begin
    pwd = Dir.pwd
    Dir.chdir(SRC_BASE)
	Dir['*.rb'].each do |file|
      dst = File.join( INSTDIR, file )
      File.install(file, dst, 0644, true)
    end
    Dir.chdir(pwd)
  rescue
    puts $!
  end
end

install
