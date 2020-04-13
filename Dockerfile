FROM rocker/geospatial:3.6.2

ARG GITHUB_PAT

RUN set -x && \
  echo "GITHUB_PAT=$GITHUB_PAT" >> /usr/local/lib/R/etc/Renviron

RUN set -x && \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    qpdf && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

RUN set -x && \
  install2.r --error --repos 'http://mran.revolutionanalytics.com/snapshot/2020-04-11' \
    assertr \
    here \
    ggforce \
    janitor \
    jpndistrict \
    lwgeom \
    parzer \
    pkgload \
    rnaturalearth \
    roxygen2 \
    roxygen2md \
    usethis && \
  installGithub.r \
    ropenscilabs/rnaturalearthhires && \
  rm -rf /tmp/downloaded_packages/ /tmp/*.rds
