\name{acs-package}
\alias{acs-package}
\alias{acs}
\docType{package}
\title{
Download and manipulate data from the US Census ACS
}
\description{

  Provides a general toolkit for downloading, managing,
  and analyzing data from the U.S. Census American Community Survey
  (ACS).  Confidence intervals provided with the
  data are converted to standard errors to be bundled with estimates
  as complex acs objects.  Package provides new methods to
  conduct standard operations and tests of significance on acs objects in statistically
  appropriate ways.  Current version 0.8 +/- .12.
}
\details{
\tabular{ll}{
Package: \tab acs\cr
Type: \tab Package\cr
Version: \tab 0.8\cr
Date: \tab 2012-02-14\cr
License: \tab GPL-3\cr
Depends: \tab stringr, methods\cr
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

Maintainer: Ezra Haber Glenn \email{eglenn@mit.edu}

}
\references{
  \enumerate{
  
\item{A Compass for Understanding and Using American Community Survey
Data: What State and Local Governments Need to Know. Washington, DC:
U.S. Census Bureau.
2009. \url{http://www.census.gov/acs/www/Downloads/handbooks/ACSstateLocal.pdf}.}

\item{acs.R: An R Package for Neighborhood-Level Data from the
  U.S. Census. Ezra Haber Glenn, Department of Urban Studies and
  Planning, Massachusetts Institute of Technology.  Presented at the
  Computers in Urban Planning and Urban Management Conference, July 6,
  2011.
  \url{http://web.mit.edu/dusp/dusp_extension_unsec/people/faculty/eglenn/glenn_acs_cupum_jou.pdf}.}


}
}
\keyword{ package }
\keyword{ manip }
