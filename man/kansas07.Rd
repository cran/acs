\name{kansas07}
\alias{kansas07}
\docType{data}
\title{
County-level data from the 2007 American Community Survey for Kansas for
use in examples of acs package.
}
\description{
County-level data from the 2007 American Community Survey for Kansas.
Contains demographic data on sex, age, and citizenship.  Used for
examples in acs package.  \code{kansas07} and the corresponding
five-year survey data in \code{kansas09} provide acs objects to test
and demonstrate various functions in the package.
}
\usage{data(kansas07)}
\format{An acs-class object with 7 rows/geographies and 55 demographic
  variables, representing county-level ACS data for the state of Kansas.  Also includes
  geographic and other metadata.

  Note that in comparison to \code{kansas09}, \code{kansas07} has far
  fewer rows, which illustrates the fact that the Census only provides
  ACS one-year data for the largest counties (over 65,000 population).
  }


\source{
U.S. Census American Community Survey, 2007; \url{http://www.census.gov/}
}

\examples{
data(kansas07)
str(kansas07)
class(kansas07)

geography(kansas07)

# subsetting
kansas07[1:3,2:4]

# row-wise addition
kansas07[1,6]+kansas07[2,6]

# column-wise addition
kansas07[1:4,3]+kansas07[1:4,27]

}
\keyword{datasets}
