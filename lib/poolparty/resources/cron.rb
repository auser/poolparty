module PoolParty    
  module Resources
=begin rdoc
== Cron

The cron resource will set cron jobs at the intervals you set

== Usage

  has_cron(:name => '...') do
    # More options.
    # This block is optional
  end

== Options

* <tt>name</tt> The name of the cronjob
* <tt>user</tt> The users who owns the cronjob  
* <tt>command</tt> The cronjob command to run
* <tt>minute</tt> Set the minute of the cronjob
* <tt>hour</tt> Set the hour of the cronjob
* <tt>month</tt> Set the month of the cronjob
* <tt>monthday</tt> Set the day of the month of the cronjob
* <tt>weekday</tt> Set the weekday of the cronjob, in 0-6 format, where 0 is Sunday

== Example

  has_cron(:name => "report mailer", :minute => "5", :hour => "0", :weekday => 1) do
    command "/bin/sh /home/user/email_reports.sh"
  end

=end
    class Cron < Resource
      
      default_options(
        :command => nil,
        :user => "root",
        :minute => "*",
        :hour => "*",
        :month => "*",
        :weekday => "*"
      )
      
      def present
        :create
      end
      
      def absent
        :delete
      end

    end
    
  end
end