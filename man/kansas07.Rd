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
examples in acs package.  \code{kansas09} (and the corresponding
five-year survey data in \code{kansas07}) provides an acs object to test
and demonstrate various functions in the package.
}
\usage{data(kansas07)}
\format{
  The format is:
Formal class 'acs' [package ".GlobalEnv"] with 9 slots
  ..@ endyear       : int 2007
  ..@ span          : int 1
  ..@ geography     :'data.frame':	7 obs. of  4 variables:
  .. ..$ Geography.Identifier    : chr [1:7] "05000US20045" "05000US20091" "05000US20103" "05000US20161" ...
  .. ..$ Geography.Identifier.1  : int [1:7] 20045 20091 20103 20161 20173 20177 20209
  .. ..$ Geographic.Summary.Level: int [1:7] 50 50 50 50 50 50 50
  .. ..$ Geography               : chr [1:7] "Douglas County, Kansas" "Johnson County, Kansas" "Leavenworth County, Kansas" "Riley County, Kansas" ...
  ..@ acs.colnames  : chr [1:55] "Universe...TOTAL.POPULATION..Total" "Universe...TOTAL.POPULATION..Male" "Universe...TOTAL.POPULATION..Male..Under.5.years" "Universe...TOTAL.POPULATION..Male..5.to.9.years" ...
  ..@ modified      : logi FALSE
  ..@ acs.units     : Factor w/ 5 levels "count","dollars",..: 1 1 1 1 1 1 1 1 1 1 ...
  ..@ currency.year : int 2007
  ..@ estimate      : num [1:7, 1:55] 113488 526319 74018 69083 476026 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:7] "Douglas County, Kansas" "Johnson County, Kansas" "Leavenworth County, Kansas" "Riley County, Kansas" ...
  .. .. ..$ : chr [1:55] "Universe...TOTAL.POPULATION..Total" "Universe...TOTAL.POPULATION..Male" "Universe...TOTAL.POPULATION..Male..Under.5.years" "Universe...TOTAL.POPULATION..Male..5.to.9.years" ...
  ..@ standard.error: num [1:7, 1:55] 0 0 1375 0 0 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:7] "Douglas County, Kansas" "Johnson County, Kansas" "Leavenworth County, Kansas" "Riley County, Kansas" ...
  .. .. ..$ : chr [1:55] "Universe...TOTAL.POPULATION..Total" "Universe...TOTAL.POPULATION..Male" "Universe...TOTAL.POPULATION..Male..Under.5.years" "Universe...TOTAL.POPULATION..Male..5.to.9.years" ...
}

\source{
U.S. Census American Community Surey, 2007; \url{http://www.census.gov/}
}

\examples{
data(kansas07)
str(kansas07)
class(kansas07)

# subsetting
kansas07[1:3,2:4]

# row-wise addition
kansas07[1,6]+kansas07[2,6]

# column-wise addition
kansas07[1:4,3]+kansas07[1:4,27]

}
\keyword{datasets}
