% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MRIP.catch.R
\name{calculate_catch}
\alias{calculate_catch}
\title{Calculate catch}
\usage{
calculate_catch(
  intdir = NULL,
  common = NULL,
  st = NULL,
  styr = NULL,
  endyr = NULL,
  dom = NULL
)
}
\arguments{
\item{intdir}{the path and main directory (in quotes) under which raw intercept sub-directories are stored.}

\item{common}{the common name of a species provided at the MRFSS website in quotes (e.g., "STRIPED BASS".). If common = NULL, quantities for all species are generated.}

\item{st}{state(s) code of data to include, as an integer.  A complete list of state codes is provided at the MRFSS website. FL = 12}

\item{styr}{Start year}

\item{endyr}{End year}

\item{dom}{optional custom domain}
}
\value{
a dataframe with catch estimates and PSEs
}
\description{
Calculate catch
}
\examples{
calculate_catch(intdir = system.file("extdata", package = "mripr"),
                         common = 'GAG',
                         st = 12,
                         styr = 2018,
                         endyr = 2018,
                         dom = list(mode_fx = list(c(1,2,3,4,5,7)),
                                    area_x = list(c(1,2,3,4,5)),
                                    sub_reg = list(c(7)))
                        )[c("Domain", "total.catch")]
                        
}
