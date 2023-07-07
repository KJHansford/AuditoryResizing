FROM rocker/verse:4.2.2

RUN apt-get update -qq
RUN apt-get -y install git

RUN install2.r -error \
--deps TRUE \
rmarkdown \
bookdown \
kableExtra \
knitr \
rstatix \
osfr \
R.matlab \
ez \
lmerTest \
emmeans \
MuMIn \
pwr \
reshape2 \
gridExtra \
jpeg \
ggpubr
