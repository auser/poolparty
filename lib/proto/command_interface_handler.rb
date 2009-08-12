class CommandInterfaceHandler
  def run_command(cld, command, argv)
    args = *(argv.split(" "))
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
    cr.response = the_cloud.send(command.to_sym, *args).to_s
    return cr
  end
end