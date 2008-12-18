set :keep_releases,       5

task :development do
  role :littleye_web, cloud(:littleye_web).ip
end