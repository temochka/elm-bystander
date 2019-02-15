require 'capistrano/setup'
require 'capistrano/deploy'

require 'capistrano/tarball_scm'
install_plugin Capistrano::TarballScm::Plugin

Dir.glob('lib/capistrano/tasks/*.rake').each { |r| import r }
