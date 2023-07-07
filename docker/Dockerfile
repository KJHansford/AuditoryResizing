FROM rocker/verse:4.2.2

RUN apt-get update -qq
RUN apt-get -y install git

RUN install2.r -error \
--deps TRUE \
kableExtra