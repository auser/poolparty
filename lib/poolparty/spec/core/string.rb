class String
  def grab_entry_for(type, name)
    begin
      allowed = /[ \$\._\-\[\]\n\t\\\/&,\(\)"',|:=\>\<0-9a-zA-Z]/
      matches = self.match(/#{type}(.*)\{(.*)"#{name}":(.*)(#{allowed})*\}/)[0]
    rescue
      ""
    end
  end
  def grab_key_value_for(type, name, key)
    grab_entry_for(type, name).scan(/#{key}[ =>]*(.*)/).flatten.first rescue ""
  end
end