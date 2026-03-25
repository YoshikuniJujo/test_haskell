FROM gentoo/stage3:latest

RUN emerge --sync
RUN emerge -v curl
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN . /root/.ghcup/env
ENV PATH=/root/.ghcup/bin:$PATH

RUN ghcup install ghc 9.10.3
RUN ghcup install ghc 9.14.1
RUN ghcup install cabal 3.16.1.0
RUN ghcup install stack 3.9.3

RUN mkdir /opt/try-hello

COPY try-world /opt/try-world
WORKDIR "/opt/try-world"
RUN stack install
ENV PATH=/root/.local/bin:$PATH

WORKDIR "/opt/try-hello"
RUN try-world-exe

COPY try-hello-haskell /opt/try-hello-haskell
WORKDIR "/opt/try-hello-haskell"
RUN stack install

WORKDIR "/opt/try-hello"
RUN makeFile

CMD ["try-hello-haskell-exe"]
