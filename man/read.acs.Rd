\name{read.acs}
\alias{read.acs}

\title{
Reads a comma-delimited file from the American Community
Survey and creates an acs object with data, standard errors, and
associated metadata.
}
\description{
When passed a comma-delimited file downloaded from the U.S. Census American Community
Survey (typically downloaded via the FactFinder website and unzipped),
read.acs returns an acs object  with data, standard errors, and
associated metadata.  
}
\usage{
read.acs(filename, endyear = "auto", span = "auto", acs.units = "auto",
geocols = "auto", skip = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
the csv (or .txt) file to be input 
}
  \item{endyear}{
an integer indicating the latest year of the data in the survey (e.g.,
for data from the 2005-2009 5-year ACS data, endyear would be 2009)
}
  \item{span}{
an integer (should be 1, 3, or 5), or "auto" to have read.acs guess the
span from the filename (e.g., for data from the 2005-2009 5-year ACS
data, span would be 5)

}
  \item{acs.units}{
a vector of factors indicating what sort of data is contained within
  each column of data ("count","dollars","proportion", "ratio", "other")}

  \item{geocols}{
a vector of integers (length must be 4)  indicating which columns
contain the geographic header information; defaults to "auto", which is
the same as 1:4}

\item{skip}{an integer indicating how many rows to skip before
  processing the csv file; defaults to 1}
}

\details{

  After executing a query on the U.S. Census American FactFinder site
  (\url{http://factfinder2.census.gov}), users can download their
  results as a zip file containing data in comma-delimited file
  format(for example, "dt_acs_2009_5yr_g00__data1.txt").  read.acs
  simplifies the creation of new acs objects from these files.

  At present, \code{read.acs} is somewhat limited, and much work needs
  to be done to improve import of data.  Currently, the function expects
  the first four columns of filename to include geographic headers (this
  can be overidden through the \code{geocols} option), and by default
  skips the first row of data (which usually contains the Census
  Bureau's non-descriptive table codes).  Column names are extracted
  from the second row of the file, as would normally happen with
  \code{read.csv(skip=1)}.  When no other values are provided,
  \code{read.acs} will attempt to determine endyear and span from the
  filename.

  (The Census Bureau is also in the process of converting from "legacy"
  FactFinder to "FactFinder2," which has introduced additional confusion
  about the exact export format to use as a standard.  Future versions
  are intended to resolve/avoid this problem.)
  
}
\value{
Returns a valid acs-class object with estimates, standard errors
(derived from the census 90\% margins of error), and metadata associated
with the survey, 
}

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

