require 'ruby2ruby' 

class Proc
  def to_ruby(opts={})
    Ruby2Ruby.new.process(self.to_sexp).gsub(/proc \{/, "do \n#{opts.map {|k,v| "#{k} \"#{v}\""}.join("\n")}\n").sub(/\}/, "\nend")
  end
end