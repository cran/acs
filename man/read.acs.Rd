\name{read.acs}
\alias{read.acs}

\title{
Reads a comma-delimited file from the American Community
Survey and creates an acs object with estimates, standard errors, and
associated metadata.
}
\description{
When passed a comma-delimited file from the U.S. Census American Community
Survey (typically downloaded via the FactFinder website and unzipped),
read.acs returns an acs object  with estimates, standard errors, and
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

\item{endyear}{ an integer (or "auto") indicating the latest year of the
data in the survey (e.g., for data from the 2005-2009 5-year ACS data,
endyear would be 2009) }

\item{span}{ an integer (should be 1, 3, or 5), or "auto" to have
read.acs guess the span from the filename (e.g., for data from the
2005-2009 5-year ACS data, span would be 5) }

\item{acs.units}{ a vector of factors indicating what sort of data is
contained within each column of data ("count","dollars","proportion",
"ratio", "other")}

  \item{geocols}{ a vector of integers indicating which columns contain
the geographic header information; defaults to "auto", which is the same
as 3:1, which seems to be the standard for FactFinder-2 downloads; by
default, \code{read.acs} will use column three columns as geographic
names to label estimates}

\item{skip}{an integer indicating how many rows to skip before
  processing the csv file; defaults to 1}
}

\details{

  After executing a query on the U.S. Census American FactFinder site
  (\url{http://factfinder2.census.gov}), users can download their
  results as a zip file containing data in comma-delimited file format
  (for example, "ACS_10_5YR_B19013_with_ann.csv").  \code{read.acs}
  simplifies the creation of new acs objects from these files.  The
  function uses some rudimentary algorithms to guess intelligently about
  values for metadata (such as \code{endyear} and \code{geography}),
  based on current file-format used by the Census "AmericanFactinder 2"
  download site. 

  As a default, \code{read.acs} assumes the first three columns will
  contain geographic header information, which seems to be the standard
  for the new Census American Factfinder download site. Users can also
  set different values for the \code{geocols=} to specify other columns
  for this geographic information.  (Note: for files downloaded via the
  Census "legacy" version of FactFinder, users will probably want to
  specify \code{geocols=4:1}.)

  As for column names, by default \code{read.acs} skips the first row of
  the file (which usually contains the Census Bureau's non-descriptive
  table codes), but users are strongly encouraged to inspect their
  downloaded csv files and set the \code{skip=} option explicitly;
  FactFinder sometimes uses three, four, or even more rows to label
  columns.  After the "skipped" rows, \code{read.acs} extracts
  \code{acs.colnames} from the next row of the file (similar to
  executing \code{read.csv(skip=1)} to create a dataframe).  (These new
  \code{acs.colnames} are also used to try to determine \code{acs.units}
  for the columns, but currently all this includes is a check for the
  word "dollars" in the names; if this is not found, the columns are
  assumed to be "counts".)

  When no other values are provided, \code{read.acs} will attempt to
  determine \code{endyear} and \code{span} from the filename.
  
}
\value{
Returns a new acs-class object with estimates, standard errors
(derived from the census 90\% margins of error), and metadata associated
with the survey, 
}

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

