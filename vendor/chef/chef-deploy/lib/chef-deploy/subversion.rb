require 'capistrano/recipes/deploy/scm/base'
require 'yaml'

class Subversion

  def initialize(opts={})
    @config = opts
  end
  
  def config
    @config
  end
  
  # Subversion understands 'HEAD' to refer to the latest revision in the
  # repository.
  def head
    "HEAD"
  end

  def repository
    config[:repository]
  end

  # Returns the command that will check out the given revision to the
  # given destination.
  def checkout(revision, destination)
    scm :checkout, config[:arguments], verbose, authentication, "-r#{revision}", repository, destination
  end

  # Returns the command that will do an "svn update" to the given
  # revision, for the working copy at the given destination.
  def sync(revision, destination)
    scm :update, arguments, verbose, authentication, "-r#{revision}", destination
  end

  # Returns the command that will do an "svn export" of the given revision
  # to the given destination.
  def export(revision, destination)
    scm :export, arguments, verbose, authentication, "-r#{revision}", repository, destination
  end

  # Returns the command that will do an "svn diff" for the two revisions.
  def diff(from, to=nil)
    scm :diff, repository, authentication, "-r#{from}:#{to || head}"
  end

  # Returns an "svn log" command for the two revisions.
  def log(from, to=nil)
    scm :log, repository, authentication, "-r#{from}:#{to || head}"
  end

  # Attempts to translate the given revision identifier to a "real"
  # revision. If the identifier is an integer, it will simply be returned.
  # Otherwise, this will yield a string of the commands it needs to be
  # executed (svn info), and will extract the revision from the response.
  def query_revision(revision)
    return revision if revision =~ /^\d+$/
    command = scm(:info, repository, authentication, "-r#{revision}")
    result = yield(command)
    yaml = YAML.load(result)
    raise "tried to run `#{command}' and got unexpected result #{result.inspect}" unless Hash === yaml
    yaml['Last Changed Rev'] || yaml['Revision']
  end

  # Increments the given revision number and returns it.
  def next_revision(revision)
    revision.to_i + 1
  end


  private

    # If a username is configured for the SCM, return the command-line
    # switches for that. Note that we don't need to return the password
    # switch, since Capistrano will check for that prompt in the output
    # and will respond appropriately.
    def authentication
      username = config(:svn_username)
      return "" unless username
      result = "--username #{config(:svn_username)} "
      result << "--password #{config(:svn_password)} "
      result
    end

    # If verbose output is requested, return nil, otherwise return the
    # command-line switch for "quiet" ("-q").
    def verbose
      "-q"
    end
    
    def scm(*args)
      ['svn', *args].compact.join(" ")
    end

    def svn_password_prompt
      config[:password]
    end
end
