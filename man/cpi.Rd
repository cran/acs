\name{cpi}
\alias{cpi}
\docType{data}
\title{Consumer Price Index data (1913-2015).
}

\description{ Contains data on the Consumer Price Index for All Urban
 Consumers (CPI-U) for the years 1913-2015 from the U.S. Bureau of Labor
 Statistics.  Used by the acs package for currency conversion functions.
 Scaled for base (100) to be 1982-84.}

\usage{
data(cpi)
}

\format{A named vector containing 103 observations, one for each year
  from 1913 through 2015.}

\source{
  \url{http://www.bls.gov/cpi/}
}
\seealso{
  \code{\link{currency.year}}

  \code{\link{currency.convert}}
    }
\keyword{datasets}
