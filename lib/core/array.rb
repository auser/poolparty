=begin rdoc
  Array extensions
=end

class Array

  # Example  nodes.select_with_hash(:status=>'running')
  def select_with_hash(conditions={})
    return self if conditions.empty?
    select do |node|
      conditions.any? do |k,v|
        ( node.has_key?(k) && node[k]==v ) or ( node.respond_to?(k) && node.send(k)==v )
      end
    end
  end
    
end