# stolen wholesale from capistrano, thanks Jamis!

class Git
  # Performs a clone on the remote machine, then checkout on the branch
  # you want to deploy.
  
  def initialize(opts={})
    @configuration = opts
  end
  
  def configuration
    @configuration
  end
  
  def git
    "git"
  end
  
  def respository
    configuration[:repository]
  end
  
  def checkout(revision, destination)
    remote = origin

    args = []
    args << "-o #{remote}" unless remote == 'origin'
    if depth = configuration[:git_shallow_clone]
      args << "--depth #{depth}"
    end

    execute = []
    if args.empty?
      execute << "#{git} clone #{verbose} #{configuration[:repository]} #{destination}"
    else
      execute << "#{git} clone #{verbose} #{args.join(' ')} #{configuration[:repository]} #{destination}"
    end

    # checkout into a local branch rather than a detached HEAD
    execute << "cd #{destination} && #{git} checkout #{verbose} -b deploy #{revision}"
    
    if configuration[:git_enable_submodules]
      execute << "#{git} submodule #{verbose} init"
      execute << "#{git} submodule #{verbose} update"
    end

    execute.join(" && ")
  end
  
  # An expensive export. Performs a checkout as above, then
  # removes the repo.
  def export(revision, destination)
    checkout(revision, destination) << " && rm -Rf #{destination}/.git"
  end

  # Merges the changes to 'head' since the last fetch, for remote_cache
  # deployment strategy
  def sync(revision, destination)
    remote  = origin

    execute = []
    execute << "cd #{destination}"

    # Use git-config to setup a remote tracking branches. Could use
    # git-remote but it complains when a remote of the same name already
    # exists, git-config will just silenty overwrite the setting every
    # time. This could cause wierd-ness in the remote cache if the url
    # changes between calls, but as long as the repositories are all
    # based from each other it should still work fine.
    if remote != 'origin'
      execute << "#{git} config remote.#{remote}.url #{configuration[:repository]}"
      execute << "#{git} config remote.#{remote}.fetch +refs/heads/*:refs/remotes/#{remote}/*"
    end

    # since we're in a local branch already, just reset to specified revision rather than merge
    execute << "#{git} fetch #{verbose} #{remote} && #{git} reset #{verbose} --hard #{revision}"

    if configuration[:git_enable_submodules]
      execute << "#{git} submodule #{verbose} init"
      execute << "#{git} submodule #{verbose} update"
    end

    execute.join(" && ")
  end

  # Returns a string of diffs between two revisions
  def diff(from, to=nil)
    from << "..#{to}" if to
    scm :diff, from
  end

  # Returns a log of changes between the two revisions (inclusive).
  def log(from, to=nil)
    scm :log, "#{from}..#{to}"
  end

  # Getting the actual commit id, in case we were passed a tag
  # or partial sha or something - it will return the sha if you pass a sha, too
  def query_revision(revision)
    raise ArgumentError, "Deploying remote branches is no longer supported.  Specify the remote branch as a local branch for the git repository you're deploying from (ie: '#{revision.gsub('origin/', '')}' rather than '#{revision}')." if revision =~ /^origin\//
    return revision if revision =~ /^[0-9a-f]{40}$/
    command = scm('ls-remote', configuration[:repository], revision)
    result = yield(command)
    rev, ref = result.split(/[\t\n]/)
    newrev = nil
    if ref.sub(/refs\/.*?\//, '').strip == revision
      newrev = rev
    end
    raise "Unable to resolve revision for '#{revision}' on repository '#{configuration[:repository]}'." unless newrev =~ /^[0-9a-f]{40}$/
    return newrev
  end
  
  def scm(*args)
    [git, *args].compact.join(" ")
  end
  
  def head
    configuration[:branch] || 'HEAD'
  end

  def origin
    configuration[:remote] || 'origin'
  end

  private

    # If verbose output is requested, return nil, otherwise return the
    # command-line switch for "quiet" ("-q").
    def verbose
      nil#configuration[:scm_verbose] ? nil : "-q"
    end
end
