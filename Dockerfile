FROM debian:jessie-slim
ENV LAST_MODIFIED 08-31-2017
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys \
    575159689BEFB442
RUN echo 'deb http://download.fpcomplete.com/debian jessie main' | \
    tee /etc/apt/sources.list.d/fpco.list
RUN apt-get update -y && apt-get upgrade -y && apt-get install -y \
    software-properties-common python-dev curl bzip2 build-essential zlib1g \
    zlib1g-dev stack libtinfo-dev libncurses5-dev libncursesw5-dev
ENV U robot
RUN useradd -ms /bin/bash $U && groupadd nixbld && usermod -a -G nixbld $U
ENV HOME /home/$U
ENV USER $U
RUN mkdir -p /nix /code/reflex-platform && chown -R $U /nix /code
USER $U
WORKDIR /home/$U
RUN curl https://nixos.org/nix/install | sh
RUN . $HOME/.nix-profile/etc/profile.d/nix.sh && \
    nix-channel --add https://nixos.org/channels/nixpkgs-unstable && \
    nix-channel --update
RUN echo ". $HOME/.nix-profile/etc/profile.d/nix.sh" >> ~/.bashrc
ADD . /code/reflex-platform
WORKDIR /code/reflex-platform
USER root
RUN chown -R $U /code/reflex-platform
USER $U
RUN ./try-reflex
WORKDIR /code
