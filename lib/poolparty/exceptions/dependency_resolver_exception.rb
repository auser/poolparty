class DependencyResolverException < Exception  
  def initialize(msg="Compile Error")
    @message = "Dependency resolver error: #{msg}"
  end
end