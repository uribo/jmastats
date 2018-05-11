FROM rocker/tidyverse:3.5.0

RUN apt-get update && apt-get install -y --no-install-recommends \
  libudunits2-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN install2.r --error \
  here \
  usethis
  units
