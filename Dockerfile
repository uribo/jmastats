FROM rocker/tidyverse:3.5.0

RUN set -x && \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    liblwgeom-dev && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

RUN set -x && \
  install2.r --error \
    here \
    lwgeom \
    sealr \
    sf \
    usethis \
    units && \
  installGithub.r \
    "uribo/jpndistrict"
