module PoolParty    
  class LineInFile
    
    virtual_resource(:line_in_file) do
      def loaded(opts={}, &block)
        has_exec "line_in_#{file}" do
          command "grep -q \'#{line.safe_quote}\' #{file} || echo \'#{line.safe_quote}\' >> #{file}"
        end
      end
    end
    
  end
end