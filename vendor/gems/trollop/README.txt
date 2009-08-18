== trollop

by William Morgan (wmorgan-trollop at the masanjin dot nets or http://cs.stanford.edu/~ruby)

Main page: http://trollop.rubyforge.org

Release announcements and comments: http://all-thing.net/search/label/trollop

Documentation quickstart: See Trollop::options (for some reason rdoc isn't
linking that; it's in the top right of the screen if you're browsing online)
and then Trollop::Parser#opt. Also see the examples at http://trollop.rubyforge.org/.

== DESCRIPTION

Trollop is a commandline option parser for Ruby that just gets out of your
way. One line of code per option is all you need to write. For that, you get
a nice automatically-generated help page, robust option parsing, command
subcompletion, and sensible defaults for everything you don't specify.

== FEATURES/PROBLEMS

- Dirt-simple usage.
- Sensible defaults. No tweaking necessary, much tweaking possible.
- Support for long options, short options, short option bundling,
  and automatic type validation and conversion.
- Support for subcommands.
- Automatic help message generation, wrapped to current screen width.
- Lots of unit tests.

== REQUIREMENTS

* A burning desire to write less code.

== INSTALL

* gem install trollop

== LICENSE

Copyright (c) 2008 William Morgan. Trollop is distributed under the same terms as Ruby.
