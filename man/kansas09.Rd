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
examples in acs package.  \code{kansas07} (and the corresponding
five-year survey data in \code{kansas09}) provides an acs object to test
and demonstrate various functions in the package.
}
\usage{data(kansas09)}
\format{
  The format is:
Formal class 'acs' [package ".GlobalEnv"] with 9 slots
  ..@ endyear       : int 2009
  ..@ span          : int 5
  ..@ geography     :'data.frame':	105 obs. of  4 variables:
  .. ..$ Geography.Identifier    : chr [1:105] "05000US20001" "05000US20003" "05000US20005" "05000US20007" ...
  .. ..$ Geography.Identifier.1  : int [1:105] 20001 20003 20005 20007 20009 20011 20013 20015 20017 20019 ...
  .. ..$ Geographic.Summary.Level: int [1:105] 50 50 50 50 50 50 50 50 50 50 ...
  .. ..$ Geography               : chr [1:105] "Allen County, Kansas" "Anderson County, Kansas" "Atchison County, Kansas" "Barber County, Kansas" ...
  ..@ acs.colnames  : chr [1:55] "Universe...TOTAL.POPULATION..Total" "Universe...TOTAL.POPULATION..Male" "Universe...TOTAL.POPULATION..Male..Under.5.years" "Universe...TOTAL.POPULATION..Male..5.to.9.years" ...
  ..@ modified      : logi FALSE
  ..@ acs.units     : Factor w/ 5 levels "count","dollars",..: 1 1 1 1 1 1 1 1 1 1 ...
  ..@ currency.year : int 2009
  ..@ estimate      : num [1:105, 1:55] 13403 7900 16469 4714 27654 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:105] "Allen County, Kansas" "Anderson County, Kansas" "Atchison County, Kansas" "Barber County, Kansas" ...
  .. .. ..$ : chr [1:55] "Universe...TOTAL.POPULATION..Total" "Universe...TOTAL.POPULATION..Male" "Universe...TOTAL.POPULATION..Male..Under.5.years" "Universe...TOTAL.POPULATION..Male..5.to.9.years" ...
  ..@ standard.error: num [1:105, 1:55] 0 0 0 0 0 0 0 0 0 0 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:105] "Allen County, Kansas" "Anderson County, Kansas" "Atchison County, Kansas" "Barber County, Kansas" ...
  .. .. ..$ : chr [1:55] "Universe...TOTAL.POPULATION..Total" "Universe...TOTAL.POPULATION..Male" "Universe...TOTAL.POPULATION..Male..Under.5.years" "Universe...TOTAL.POPULATION..Male..5.to.9.years" ...
}

\source{
U.S. Census American Community Surey, 2009; \url{http://www.census.gov/}
}

\examples{
data(kansas09)
str(kansas09)
class(kansas09)

# subsetting
kansas09[1:3,2:4]

# row-wise addition
kansas09[1,6]+kansas09[2,6]

# column-wise addition
kansas09[1:4,3]+kansas09[1:4,27]

}
\keyword{datasets}
