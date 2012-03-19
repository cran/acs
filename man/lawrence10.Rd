\name{lawrence10}
\alias{lawrence10}
\docType{data}
\title{
Tract-level data from the 2006-2010 American Community Survey for
Lawrence, MA for use in examples of acs package.
}
\description{
Tract-level data from the 2006-2010 American Community Survey for
Lawrence, MA.  Contains median household income.  Used for
examples in acs package.
}
\usage{data(lawrence10)}

\format{An acs-class object with 18 rows/geographies and 1 variable,
  representing tract-level ACS data for the city of Lawrence, MA from
  2006-2010.  Also includes geographic and other metadata.}


\source{
U.S. Census American Community Surey, 2010; \url{http://www.census.gov/}
}

\examples{
data(lawrence10)
str(lawrence10)
class(lawrence10)

# subsetting
lawrence10[1:3,1]

# row-wise subtraction
lawrence10[1,1]+lawrence10[2,1]

}
\keyword{datasets}
