module PoolParty
  module Output

    def output(*args)
      returning (@output ||= []) do |output|
        args.each do |line|
          (output ||= []) << line
        end
      end
    end

  end
end