class FakeResource < PoolParty::Resource
  
  default_options(
    :name => nil
  )
  def self.has_method_name
    "tester"
  end
          
  def print_to_chef        
    <<-EOE
fake "<%= name %>" do
  content "<%= content %>"
end
    EOE
  end
  
end