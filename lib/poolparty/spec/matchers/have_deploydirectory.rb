module PoolParty
  class SpecCore
    dynamic_matcher_for(:deploydirectory) do
      it "should have the directory" do
        @manifest.should have_directory(@to)
      end
      it "should have the rsyncmirror" do
        @manifest.should have_rsyncmirror("deploydirectory-#{@to}")
      end
      it "should have the exec to unpack the archive" do
        @manifest.should have_exec("deploy-directory-#{@to}")
      end
    end
  end
end