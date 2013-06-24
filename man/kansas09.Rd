\name{kansas09}
\alias{kansas09}
\docType{data}
\title{
County-level data from the 2005-2009 American Community Survey for Kansas for
use in examples of acs package.
}
\description{
County-level data from the 2005-2009 American Community Survey for Kansas.
Contains demographic data on sex, age, and citizenship.  Used for
examples in acs package.  \code{kansas09}, and the corresponding
one-year survey data in \code{kansas07}, provide acs objects to test
and demonstrate various functions in the package.
}
\usage{data(kansas09)}
\format{An acs-class object with 105 rows/geographies and 55 demographic
  variables, representing county-level ACS data for the state of Kansas.  Also includes
  geographic and other metadata.} 


\source{
U.S. Census American Community Survey, 2009; \url{http://www.census.gov/}
}

\examples{
data(kansas09)
str(kansas09)
class(kansas09)

geography(kansas09)

# subsetting
kansas09[1:3,2:4]

# row-wise addition
kansas09[1,6]+kansas09[2,6]

# column-wise addition
kansas09[1:4,3]+kansas09[1:4,27]

}
\keyword{datasets}
