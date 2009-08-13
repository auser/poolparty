class CommandInterfaceHandler
  def run_command(cld, command, args)
    cr = CloudThrift::CloudResponse.new
    cr.name = cld.name
    cr.command = command
    begin
      the_cloud = clouds[cld.name]
      raise Exception.new("Cloud not found") unless the_cloud
    rescue Exception => e
      cr.response = "Error: #{e.inspect}"
      return cr
    end
    resp = begin
      the_cloud.send(command.to_sym, *args)
    rescue
      "undefined method"
    end
    
    cr.response = case resp
    when Array
      resp
    else
      [resp]
    end.map {|ele| ele.to_s }

    return cr
  end
end