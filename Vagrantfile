# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "generic/ubuntu2204"
  config.ssh.insert_key = false

  config.vm.synced_folder "./sicp-files", "/home/vagrant/sicp-files"
  config.ssh.username = "vagrant"
  config.ssh.password = "vagrant"
  config.vm.provision "file", source: "~/.vimrc", destination: "/home/vagrant/.vimrc"

  config.vm.provision "shell", inline: <<-SHELL
    apt-get update
    apt-get install -y mit-scheme
    mkdir -p /home/vagrant/sicp-files
    mkdir -p /home/vagrant/.vim/autoload
    sudo chown vagrant: /home/vagrant/.vim
    cd /home/vagrant/.vim/autoload && curl -O https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim && cd -
  SHELL
end
