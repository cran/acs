\name{acs-package}
\alias{acs-package}
\alias{acs}
\docType{package}
\title{
  Download, Manipulate, and Present American Community Survey and
  Decennial Data from the US Census
}
\description{

Provides a general toolkit for downloading, managing, analyzing, and
  presenting data from the U.S. Census, including SF1 (Decennial
  "short-form"), SF3 (Decennial "long-form"), and the American Community
  Survey (ACS).  Confidence intervals provided with ACS data are
  converted to standard errors to be bundled with estimates in complex
  acs objects.  Package provides new methods to conduct standard
  operations on acs objects and present/plot data in statistically
  appropriate ways.}

\details{ \tabular{ll}{ Package: \tab acs\cr Type: \tab Package\cr
  Version: \tab 2.1.2\cr Date: \tab 2017-09-25\cr License: \tab GPL-3\cr
  Depends: \tab stringr, methods, XML\cr }

The package defines a new "acs" class (containing estimates, standard
  errors, geography, and metadata for tables from the U.S. Census
  American Community Survey), with methods to deal appropriately with
  common tasks, such as combining subgroups or geographies, mathematical
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
2009. \url{http://www.census.gov/library/publications/2009/acs/state-and-local.html}.}

\item{"acs.R: An R Package for Neighborhood-Level Data from the
  U.S. Census." Ezra Haber Glenn, Department of Urban Studies and
  Planning, Massachusetts Institute of Technology.  Presented at the
  Computers in Urban Planning and Urban Management Conference, July 6,
  2011.
  \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2171390}.}

\item{"Working with acs.R (June 2013)", Ezra Haber Glenn.
  \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2552524}}

\item{CityState webpage: \url{http://eglenn.scripts.mit.edu/citystate/}}

\item{User Group Mailing List: \url{http://mailman.mit.edu/mailman/listinfo/acs-r}} 
}
}
\keyword{ package }
\keyword{ manip }
