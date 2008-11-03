#!/usr/bin/env ruby

require "rubygems"
require "sinatra"

cmd = "rm ebin/*; erl -pa ./ebin -run make all load -run app start"
Kernel.system cmd

get '/' do
  "body"
end