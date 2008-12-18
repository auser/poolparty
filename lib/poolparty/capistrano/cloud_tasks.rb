Capistrano::Configuration.instance(:must_exist).load do
  namespace(:cloud_tasks) do    
    desc "List running clouds"
    task :list do
      cmd = "cloud-list -n #{cloud.name}"
      puts cmd
      # %x[cmd]
    end
  end
end