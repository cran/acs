\name{acs-package}
\alias{acs-package}
\alias{acs}
\docType{package}
\title{
Download, manipulate, and present data from the US Census ACS
}
\description{

  Provides a general toolkit for downloading, managing,
  analyzing, and presenting data from the U.S. Census American Community Survey
  (ACS).  Confidence intervals provided with the
  data are converted to standard errors to be bundled with estimates
  as complex acs objects.  Package provides new methods to
  conduct standard operations and tests of significance on acs objects in statistically
  appropriate ways.  Current version 1.2 +/- .05.
}
\details{
\tabular{ll}{
Package: \tab acs\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2014-01-20\cr
License: \tab GPL-3\cr
Depends: \tab stringr, methods, plyr, XML\cr
}

The package defines a new "acs" class (containing estimates, standard
  errors, and metadata for tables from the U.S. Census American
  Community Survey), with methods to deal appropriately with common
  tasks, such as combining subgroups or geographies, mathematical
  operations on estimates, tests of significance, and computing (and
  plotting) confidence intervals.

}
\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}
\references{
  \enumerate{
  
\item{"A Compass for Understanding and Using American Community Survey
Data: What State and Local Governments Need to Know." Washington, DC:
U.S. Census Bureau.
2009. \url{http://www.census.gov/acs/www/Downloads/handbooks/ACSstateLocal.pdf}.}

\item{"acs.R: An R Package for Neighborhood-Level Data from the
  U.S. Census." Ezra Haber Glenn, Department of Urban Studies and
  Planning, Massachusetts Institute of Technology.  Presented at the
  Computers in Urban Planning and Urban Management Conference, July 6,
  2011.
  \url{http://dusp.mit.edu/sites/all/files/attachments/publication/glenn_acs_cupum_jou.pdf}.}

\item{"Working with acs.R (version 1.0)", Ezra Haber Glenn.
  \url{http://eglenn.scripts.mit.edu/citystate/wp-content/uploads/2013/06/wpid-working_with_acs_R3.pdf}}

\item{CityState webpage: \url{http://eglenn.scripts.mit.edu/citystate/}}

\item{User Group Mailing List: \url{http://mailman.mit.edu/mailman/listinfo/acs-r}} 
}
}
\keyword{ package }
\keyword{ manip }
