FROM rocker/geospatial:4.0.2@sha256:bc95d044d7e7289055962cbbd7daf58daf680e8a930ba3bd19d85a90080a7e41

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
  install2.r --error --ncpus -1 --repos 'https://mran.revolutionanalytics.com/snapshot/2020-09-28' \
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
