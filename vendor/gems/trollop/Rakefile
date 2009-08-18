# -*- ruby -*-

require 'rubygems'
require 'hoe'

$:.unshift "lib"
require 'trollop'

class Hoe
  def extra_dev_deps; @extra_dev_deps.reject { |x| x[0] == "hoe" } end
end

Hoe.new('trollop', Trollop::VERSION) do |p|
  p.rubyforge_name = 'trollop'
  p.author = "William Morgan"
  p.summary = "Trollop is a commandline option parser for Ruby that just gets out of your way. One line of code per option is all you need to write. For that, you get a nice automatically-generated help page, robust option parsing, command subcompletion, and sensible defaults for everything you don't specify."
  p.description = p.paragraphs_of('README.txt', 4..5, 9..18).join("\n\n").gsub(/== SYNOPSIS/, "Synopsis")
  p.url = "http://trollop.rubyforge.org"
  p.changes = p.paragraphs_of('History.txt', 0..0).join("\n\n")
  p.email = "wmorgan-trollop@masanjin.net"
end

WWW_FILES = FileList["www/*"] + %w(README.txt FAQ.txt)
task :upload_webpage => WWW_FILES do |t|
  sh "rsync -Paz -essh #{t.prerequisites * ' '} wmorgan@rubyforge.org:/var/www/gforge-projects/trollop/"
end

task :rdoc do |t|
  sh "rdoc lib README.txt History.txt -m README.txt"
end

task :upload_docs => :rdoc do |t|
  sh "rsync -az -essh doc/* wmorgan@rubyforge.org:/var/www/gforge-projects/trollop/trollop/"
end

# vim: syntax=ruby
