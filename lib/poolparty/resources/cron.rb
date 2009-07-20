module PoolParty
  module Resources
    
    class Cron < Resource
            
      default_options(
        :minute => "*",
        :hour => "*",
        :day => "*",
        :month => "*",
        :weekday => "*",
        :user => "root",
        :command => nil
      )
            
      def print_to_chef        
        <<-EOE
cron "<%= name %>" do
  command "<%= command || name %>"
  action :<%= exists? ? :create : :delete %>
  minute <%= print_variable(minute) %>
  hour <%= print_variable(hour) %>
  day <%= print_variable(day) %>
  month <%= print_variable(month) %>
  weekday <%= print_variable(weekday) %>
  user <%= print_variable(user) %>
end
        EOE
      end
      
    end
    
  end
end