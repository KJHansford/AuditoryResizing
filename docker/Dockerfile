FROM rocker/verse:4.2.2

RUN apt-get update -qq
RUN apt-get -y install git

RUN install2.r –error \
--deps TRUE \
rmarkdown \
bookdown \
kableExtra \
knitr \
rstatix \
osfr \
ez \
pwr \
reshape2 \
R.matlab \
ggpubr \
gridExtra \
jpeg \
grid \
plotrix \
lavaan \
cowplot \
readr \
caTools \
bitops
