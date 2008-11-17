= PoolParty

http://poolpartyrb.com

== DESCRIPTION:

PoolParty makes it easy and simple to configure any cloud of computers. In clear language, describe your cloud
with language such as:

pool :cloud do
 cloud :app do
  apache do
   has_virtualhost(:name => "/var/www/sites/poolpartyrb.com")
  end
 end
end

== FEATURES/PROBLEMS:

* Written in Ruby and Erlang
* Written from the ground up to be extensible with plugins
* Easy git-style commands to communicate with your clouds
* Much much more

== SYNOPSIS:

PoolParty is written with the intention of being as application-agnostic as possible. It installs only the basic 
required software to glue the cloud together on the instances as listed below.

PoolParty is easily configuration. In fact, it makes little assumptions about your development environment and allows 
several options on how to begin configuring the cloud.

== REQUIREMENTS:

COMING SOON

== INSTALL:

sudo gem install auser-poolparty

== TODO:
* Replace services with Runit
* Refactor provisioning to use erlang
* Move the building of the resource tree on read to on resolve
* Add queuing of tasks on the messenger

== LICENSE:

(The MIT License)

Copyright (c) 2008 Ari Lerner

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
'Software'), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.