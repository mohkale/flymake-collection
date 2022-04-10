# Ubuntu 20.04 LTS supported until April 2025
FROM ubuntu:20.04

# Suppress some interactive prompts by answering them with environment
# variables.
ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC

# Install emacs. Instructions adapted from [[https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation][here]].
WORKDIR /build/emacs
RUN apt-get update && \
    apt-get install -y \
      apt-transport-https \
      ca-certificates \
      curl \
      gnupg-agent \
      software-properties-common \
      libjansson4 \
      libjansson-dev \
      git && \
    git clone -b "${EMACS_BRANCH:-emacs-28}" --single-branch git://git.savannah.gnu.org/emacs.git . && \
    sed -i 's/# deb-src/deb-src/' /etc/apt/sources.list && \
    apt-get update && \
    apt-get build-dep -y emacs && \
    ./autogen.sh && \
    ./configure && \
    make -j4 && \
    make install && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /build
COPY installers /build/installers
RUN apt-get update && \
    apt-get install -y \
      parallel python3.8 python3-pip moreutils git && \
    python3.8 -m pip install pyyaml && \
    find installers/ -type f -iname '*.bash' -exec {} \; && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install the latest available version of flymake for these tests.
RUN cd "$(mktemp -d)" && \
    git clone https://github.com/emacs-straight/flymake.git . && \
    mkdir /tmp/emacs && \
    cp flymake.el /etc/emacs/flymake.el && \
    rm -rf "$(pwd)"

WORKDIR /src
