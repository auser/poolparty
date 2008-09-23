require File.dirname(__FILE__) + '/../spec_helper'

include PoolParty::Resources

describe "Custom Resource" do
  before do
    reset_resources!
  end
  it "should provide custom_resource as a method" do
    self.respond_to?(:define_resource).should == true
  end
  describe "defining" do
    before(:each) do
      reset_resources!
    end
    it "should not raise ResourceException if custom_function and custom_usage are defined" do
      lambda {
        define_resource(:rockstar) do          
          custom_usage do
            def has_line_in_file(line="line_in_file", file="file")              
            end
          end
          custom_function <<-EOF
          define line($file, $line, $ensure = 'present') {
          }
          EOF
        end
      }.should_not raise_error
    end
    describe "printing" do
      before do
        reset_resources!
        define_resource(:rockstar) do
          custom_usage do
            def has_line_in_file(line="line_in_file", file="file")
              store "line(#{file}, #{line})"
            end
          end
          custom_function <<-EOF
          define line($file, $line, $ensure = 'present') {
            case $ensure {
              default: { err ( "unknown ensure value ${ensure}" ) }
              present: {
                exec {
                  "/bin/echo '${line}' >> '${file}'":
                   unless => "/usr/bin/grep -qFx '${line}' '${file}'"
                }
              }
              absent: {
                exec {
                  "/usr/bin/sed -i '' -e '/^${line}\$/d' '${file}'":
                    onlyif => "/usr/bin/grep -qFx '${line}' '${file}'"
                }
              }
            }
          }
          EOF
        end
      end
      it "should be able to be printed" do
        custom_resource(:rockstar).function_strings.should =~ /define line/
      end
      it "should allow for the has_line_in_file to be called from within a plugin" do
        has_line_in_file("hi", "filename").should == ["line(filename, hi)"]
      end
      it "should have several lines in the files when called several times" do
        has_line_in_file("hi", "filename")
        has_line_in_file("hi", "filename2")
        custom_resource(:rockstar).to_string("\t").should =~ /line\(filename, hi\)/
        custom_resource(:rockstar).to_string("\t").should =~ /line\(filename2, hi\)/
      end
    end
  end
end