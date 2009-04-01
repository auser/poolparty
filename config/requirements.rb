require 'fileutils'
include FileUtils

require 'rubygems'
require 'rake'

$:.unshift(File.join(File.dirname(__FILE__), %w[.. lib]))
