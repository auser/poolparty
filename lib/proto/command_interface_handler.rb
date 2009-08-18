class CommandInterfaceHandler
  def run_command(cld, command, args)
        
    cr = CloudThrift::CloudResponse.new
    cr.name = cld.name
    cr.command = command
    resp = begin
      the_cloud = clouds[cld.name]
      the_cloud ? the_cloud.send(command.to_sym, *args) : "Cloud not found: #{cld.name}"
    rescue Exception => e
      cr.response = "Error: #{e.inspect}"
    end
    
    cr.response = format_response(resp)

    return cr
  end
  
  private
  
  def format_response(resp)
    case resp
    when Array
      resp.join(",")
    when Hash
      resp.map {|k,v| "#{k}:#{format_response(v.empty? ? "null" : v)}" }
    else
      [resp]
    end.map {|ele| ele.to_s }
  end
end