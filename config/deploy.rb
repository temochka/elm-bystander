lock '3.7.1'

set :deploy_to, '/srv/www/elm-bystander.temochka.com'
set :keep_releases, 5
set :repo_url, 'public'
set :tarball_exclude, []

namespace :deploy do
  task :build_clean do
    run_locally do
      execute :rm, '-rf public'
    end
  end

  task build: :build_clean do
    run_locally do
      execute :elm, 'make', 'src/Main.elm', '--output', 'public/index.html', '--optimize'
    end
  end

  before :starting, :build
end
