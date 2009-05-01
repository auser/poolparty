require "#{::File.dirname(__FILE__)}/../../test_helper"

module PoolParty
  module Verifiers
    class Ding < VerifierBase
      attr_accessor :port, :checked
      def initialize(port=80)
        @port = port
      end
      def passing?
        @checked
      end
    end
    
  end
end
class TestVerificationClass
  include PoolParty::Verification
  
  attr_reader :port
  def initialize(port=70, &block)
    @port = port
    instance_eval &block
  end    
end

class TestVerification < Test::Unit::TestCase
  context "verification included" do
    setup do
      @vc = TestVerificationClass.new do
        verify do
          ding 830
        end
      end
    end
    should "have a the ping verifier as a verifiers in the class" do
      @vc.verifiers.first.class.should == PoolParty::Verifiers::Ding
    end
    should "not explode when calling verify on the verifier" do
      @vc.verifiers.first.checked = true
      @vc.passing?.should == true
    end
    should "explode if the verification fails" do
      @vc.verifiers.first.checked = false
      lambda {@vc.passing?}.should raise_error
    end
  end
  
end