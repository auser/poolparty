class String
  def grab_entry_for(type, name)
    begin      
      matches = self.match(/#{type}(.*)\{(.*)"#{name}":(.*)(#{_allowed})*\}/)[0]
    rescue
      ""
    end
  end
  def _allowed
    /[ \$\._\*\-\[\]\n\t\\\/&,\(\)"',|:=\>\<A-Za-z0-9]/
  end
  def _grab_key_value_for(type, name, key)
    grab_entry_for(type, name).scan(/#{key}[ =>]*(.*)[,?]?$/).flatten.first rescue ""
  end
  def grab_key_value_for(ty, name, key)
    _grab_key_value_for(ty,name,key).gsub(/,/, '')
  end
end