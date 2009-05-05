module PoolParty
  module Verifiers
    
=begin

== HttpStatus Verifier

Verify the HTTP response code from a particular url

== Usage 

  http_status url, response_code

== Example

  verify do
    http_status "http://host/index.html", 200
    http_status "http://host/index.html", :success
    http_status "http://host/asdfasdfads.html", 404
  end

=end

    class HttpStatus < VerifierBase
      require 'net/http'
      require 'uri'
      
      attr_reader :uri, :status
      def initialize(uri, status)
        @uri    = URI.parse(uri)
        @status = status
      end

      def passing?
        http = Net::HTTP.new(@uri.host)
        response = http.request_get(@uri.path && !@uri.path.empty? ? @uri.path : "/")

        return case true
               when status.kind_of?(Numeric)
                 response.code == @status.to_s 
               when status.kind_of?(Symbol)
                 case status
                 when :success then response.kind_of?(Net::HTTPSuccess)
                 else 
                   raise "unknown symbol #{status}"
                 end
               else
                 raise "unknown status type #{status}"
               end
      end

      def to_s
        "<#{self.class.to_s} uri:#{uri} status:#{status}>"
      end

    end
    
  end
end
