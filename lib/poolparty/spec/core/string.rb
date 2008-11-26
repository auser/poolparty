class String
  def grab_entry_for(type, name)
    begin
      allowed = /[ \$\._\-\[\]\n\t\\\/&,\(\)"':=>0-9a-zA-Z]/
      matches = self.match(/#{type}(.*)\{(.*)#{name}\":(.*)(#{allowed})*\}/)[0]
    rescue
      ""
    end
  end
end