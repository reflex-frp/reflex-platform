# -*- mode: ruby -*-
# vi: set ft=ruby:

Vagrant.configure(2) do |config|
    config.vm.box = "minimal/trusty64"
    config.vm.hostname = "reflex-platform.vagrant"
    config.vm.network "private_network", ip: "192.168.50.90"
    config.vm.provision "shell", inline: <<-SHELL
        U=vagrant
        apt-get update -y
        apt-get install -y apt-transport-https ca-certificates curl
        apt-key adv --keyserver hkp://ha.pool.sks-keyservers.net:80 \
            --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
        echo "deb https://apt.dockerproject.org/repo ubuntu-trusty main" | \
            tee /etc/apt/sources.list.d/docker.list
        apt-get update -y
        apt-cache policy docker-engine
        apt-get install -y docker-engine
        service docker start
        gpasswd -a $U docker
        mkdir -p /home/$U/code
        exit 0
    SHELL
    config.vm.synced_folder ".", "/home/vagrant/code", type: 'nfs'
    config.vm.provider :virtualbox do |vb|
        vb.customize ["modifyvm", :id, "--usb", "off"]
        vb.customize ["modifyvm", :id, "--usbehci", "off"]
        vb.customize ['modifyvm', :id, '--clipboard', 'bidirectional']
        vb.name = "reflex-platform.vagrant"
        vb.memory = 4096
        vb.cpus = 2
    end
end
