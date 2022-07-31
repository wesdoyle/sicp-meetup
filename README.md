# Structure and Interpretation of Computer Programs Meetup

This repository contains notes, examples, and local environment provisioning for following along with Abelson and Sussman's __Structure and Interpretation of Computer Programs__ text, available in the public domain at [this website](https://mitpress.mit.edu/sites/default/files/sicp/index.html), hosted by MIT.

The repo reflects working through the text at a twice-monthly Meetup hosted in Madison, WI in 2022.

## Setup

### Vagrant

This project uses [Vagrant](https://vagrantup.com) to provision an Ubuntu 22.04 VM with `mit-scheme` installed for working through the text.  If you'd like to use an isolated local VM for the purposes of the meetup, ensure Vagrant is installed on your machine.

To create the environment, run

```sh
vagrant up
```

To destroy the environment, run

```sh
vagrant destroy
```

### vim

The Vagrantfile copies a `~/.vimrc` files from your host environment to the VM and installed [vim-plug](https://github.com/junegunn/vim-plug).  If your vimrc contains directives to install your plugins via `Plug`, you can run `:PlugInstall` when first launching vim in the VM.

## Contributing

If you'd like to contribute to this project, please feel free to make a pull request.
