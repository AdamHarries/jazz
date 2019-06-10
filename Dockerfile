FROM debian:stretch

LABEL "repository"="http://github.com/AdamHarries/jazz"
LABEL "homepage"="http://github.com/AdamHarries/jazz"
LABEL "maintainer"="Adam Harries <harries.adam@gmail.com>"

RUN	apt-get update

# Just install enough to download MuseScore - this is the slowest step, so this should cache it properly.
RUN yes | apt-get install \
    jq \
    python3 \
    fuse \
    libglib2.0-0 \
    less \
    locales \
    locales-all \
    texlive-xetex \
    texlive-latex-recommended \
    libvorbisfile3 \
    libsndfile1 \
    ecasound \
    libxcb-xfixes0 \
    libwayland-client0 \
    libwayland-server0 \
    libgbm1 \
    fontconfig \
    tree \
    bash \
    ca-certificates \
    curl \
    jq \
    xvfb \
    libgl1-mesa-dri \
    wget

ENV LC_ALL en_GB.UTF-8
ENV LANG en_GB.UTF-8
ENV LANGUAGE en_GB.UTF-8

WORKDIR /home/root

RUN wget -q https://github.com/musescore/MuseScore/releases/download/v3.1/MuseScore-3.1.0-x86_64.AppImage

ADD make_books.py make_books.py
ADD src src
ADD drafts drafts
ADD MuseJazzText.otf MuseJazzText.otf
ADD general_style.mss general_style.mss

RUN chmod 644 MuseJazzText.otf && cp MuseJazzText.otf /usr/local/share/fonts && fc-cache -v

ADD entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
