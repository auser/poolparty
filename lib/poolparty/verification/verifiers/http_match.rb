module PoolParty
  module Verifiers
    
=begin

== HttpMatch Verifier

Verify the body of an HTTP call matches a regular expression

== Usage 

  http_match url, response_code

== Example

  verify do
    http_match  "http://host/index.html", /Welcome to your/
    http_match  "http://host/index.html", /new poolparty instance/
  end

=end

    class HttpMatch < VerifierBase
      require 'open-uri'
      
      attr_reader :uri, :regexp
      def initialize(uri, regexp)
        @uri    = URI.parse(uri)
        @regexp = regexp
      end

      def passing?
        @regexp.match(@uri.read) ? true : false
      end

      def to_s
        "<#{self.class.to_s} uri:#{uri} regexp:#{regexp}>"
      end

    end
    
  end
end
