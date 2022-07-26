% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/track.R
\name{read_rsmc_besttrack}
\alias{read_rsmc_besttrack}
\alias{track_combine}
\title{Read RSMC Tokyo best track data}
\usage{
read_rsmc_besttrack(path)

track_combine(
  data,
  group_vars = c("international_number", "storm_name"),
  keep_vars = NULL,
  geometry = geometry
)
}
\arguments{
\item{path}{path to best track data (.txt)}

\item{data}{Import data using \link{read_rsmc_besttrack}}

\item{group_vars}{To combine track variables.}

\item{keep_vars}{Keep variables.}

\item{geometry}{geometry column name (default \code{geometry}).}
}
\description{
Tidy formatting best track data and combine each point to line.
@details
\itemize{
\item \code{read_rsmc_besttrack()}: Read single best track data into \link[sf:sf]{sf}
contains observation record as point.
\item \code{track_combine()}: Combine track data to line by id
(such as international_number and storm_name).
}
}
\details{
See \url{https://www.jma.go.jp/jma/jma-eng/jma-center/rsmc-hp-pub-eg/RSMC_HP.htm} for more details.
}
