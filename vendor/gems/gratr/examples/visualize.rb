#!/usr/bin/ruby -I../lib

require 'gratr/import'
require 'gratr/dot'

class Integer
end

dg = Digraph[1,2, 2,3, 2,4, 4,5, 6,4, 1,6]

dg.write_to_graphic_file('jpg','visualize')