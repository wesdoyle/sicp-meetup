# Structure and Interpretation of Computer Programs Meetup

This repository contains notes, examples, and local environment provisioning for following along with Abelson and Sussman's __Structure and Interpretation of Computer Programs__ text, available in the public domain at ~[this website](https://mitpress.mit.edu/sites/default/files/sicp/index.html), hosted by MIT.~  

The MIT-hosted SICP now appears to 404 at the above link.  The new site hosted by MIT uses JavaScript examples.  The best resource we're aware of reflecting the original text in Scheme is here, created by Li Xuanji: [https://xuanji.appspot.com/isicp/index.html](https://xuanji.appspot.com/isicp/index.html).

Also, the [sarabander version](https://sarabander.github.io/sicp/)

The repo reflects working through the text at a twice-monthly Meetup hosted in Madison, WI in 2022.

## Setup

### Scheme
SICP uses the Scheme programming language.

#### Implementations
Scheme goes by many names. The language specification is small, so available features vary between implementations. Any implementation that's R5RS+ compatible should be able to run most of the examples in SICP, though adaptations may be necessary. We will attempt to address these if/when we encounter imcompatibilities throughout the course of the book. Consider using one of the following:

- [Guile](https://www.gnu.org/software/guile/) 🍎: Should be compatible with examples provided by SICP. This is the primary implementation used by Wes and Jacob. Reference [this documentation](https://www.gnu.org/software/guile/manual/html_node/Loading-Readline-Support.html) to enable a nicer REPL experience with completion, history, command line editing, etc.
- [Racket](https://racket-lang.org/), with [#lang sicp](https://docs.racket-lang.org/sicp-manual/index.html) 🍎: Comes with its own IDE: ([DrRacket](https://docs.racket-lang.org/drracket/index.html)). An implementation of the picture language used in SICP is available [here](https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html)
- [Chicken](https://call-cc.org/) 🍎: An implementation of R5RS and R7RS Scheme that has a large library of practical extensions called [Eggs](http://eggs.call-cc.org/5/). Egg are available for
  - [SRFI-203](https://srfi.schemers.org/srfi-203/srfi-203.html): a picture language in the style of the one used in SICP.
  - [SRFI-216](https://srfi.schemers.org/srfi-216/srfi-216.html): an SICP prerequisites library that aims to smooth over incompatibility of some SICP examples with modern Scheme
- [mit-scheme](https://www.gnu.org/software/mit-scheme/): Commonly used Scheme implementation
  The [Vagrant](#Vagrant) environment described below may be used to run mit-scheme.

_An 🍎 denotes native Apple Silicon support_.

All of the implementations listed above should be available for Linux and macOS (with or without native arm64 support). They are likely available for Windows, too---reach out to Jacob or Wes if you need help finding a suitable Scheme implementation on Windows.

#### Vagrant

This project contains configuration for [Vagrant](https://vagrantup.com) to provision an Ubuntu 22.04 VM with `mit-scheme` installed for working through the text.  If you'd like to use an isolated local VM for the purposes of the meetup, ensure Vagrant is installed on your machine.

To create the environment, run

```sh
vagrant up
```

Once provisioned, you can ssh into the local VM:

```sh
ssh -p 2222 vagrant@127.0.0.1
```

When prompted, use the password `vagrant`.

To destroy the environment, run

```sh
vagrant destroy
```

##### vim

The Vagrantfile copies a `~/.vimrc` file from your host environment to the VM and installs [vim-plug](https://github.com/junegunn/vim-plug).  If your vimrc contains directives to install your plugins via `Plug`, you can run `:PlugInstall` when first launching vim in the VM.


### Editor/REPL

Lisps are best written with a REPL at hand. It's even better when code can be sent directly to a running REPL from your editor. Here are some recommendations for editor/plugin combos with great Scheme REPL support.

#### Neovim
[Conjure](https://github.com/Olical/conjure) is an excellent neovim plugin that enables interactive development with a variety of Lisp-y languages. The wiki has guides for setting up [Scheme](https://github.com/Olical/conjure/wiki/Quick-start:-Scheme-(stdio)) (MIT, Chicken, Chez), [Guile](https://github.com/Olical/conjure/wiki/Quick-start:-Guile-(socket)), and [Racket](https://github.com/Olical/conjure/wiki/Quick-start:-Racket-(stdio))


#### Emacs
The quintessential Lisp ~~operating system~~ editor. [Geiser](http://geiser.nongnu.org/) provides interactive Scheme development support for Emacs.

If you're new to Emacs and want an easy way to get started, it's recommended to use an Emacs distribution:

- [Doom Emacs](https://github.com/doomemacs/doomemacs) is great for those who want Vim-flavored Emacs.
- [Spacemacs](https://www.spacemacs.org/) is another Emacs distribution with Vim-centric bindings and an opinionated "layer" system for functionality.
- [Prelude](https://github.com/bbatsov/prelude) provides a more traditional, but potentially easier to use Emacs experience.

All of the above offer interactive Scheme functionality with Geiser pre-configured.

If you're new to Emacs and are _not_ using a Vim/Evil keybinding configutation, [cua mode](https://www.emacswiki.org/emacs/CuaMode) may make Emacs more approachable with by enabling the common `Ctrl-C`, `Ctrl-V`, etc. keyboard shortcuts.

#### DrRacket (w/ Racket)
[DrRacket](https://docs.racket-lang.org/drracket/index.html) is the IDE that comes with the [Racket](https://racket-lang.org/) programming language

#### Editors without REPL Support

If your chosen editor doesn't have REPL support, the REPL can be used from the command line. Depending on your chosen Scheme implementation, you may be able to call the `load` procedure to load the procedure definitions in a Scheme file to call them interactively. Most Scheme implementations (like Guile) also allow execution of a Scheme file as a script.

## Contributing

If you'd like to contribute to this project, please feel free to make a pull request.
