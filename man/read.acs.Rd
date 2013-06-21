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
read.acs(filename, endyear = "auto", span = "auto", col.names= "auto", acs.units = "auto",
geocols = "auto", skip = "auto")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
the name of the \code{.csv}, \code{.zip}, or \code{.txt} file to be input 
}

\item{endyear}{ an integer (or "auto") indicating the latest year of the
data in the survey (e.g., for data from the 2005-2009 5-year ACS data,
endyear would be 2009) }

\item{span}{ an integer (should be 1, 3, or 5), or "auto" to have
read.acs guess the span from the filename (e.g., for data from the
2005-2009 5-year ACS data, span would be 5) }

\item{col.names}{a vector of column names to be used as
  \code{acs.colnames} for the object; defaults to "auto", which will
  result in auto-generated names from the headers lines of the input file}

\item{acs.units}{ a vector of factors indicating what sort of data is
  contained within each column of data ("count","dollars","proportion",
  "ratio", "other")}

  \item{geocols}{ a vector of integers indicating which columns contain
the geographic header information; defaults to "auto", which is the same
as 3:1, which seems to be the standard for FactFinder-2 downloads}

\item{skip}{an integer indicating how many rows to skip before
  processing the csv file; defaults to "auto", which will try to guess
  the proper value}
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

  The specified \code{filename} can be an actual \code{.csv} file, or
  can be the name of a \code{.zip} file downloaded from the FactFinder
  site.  If the latter, \code{read.acs} will extract the necessary data
  and leave the compressed zipfile in place.

  As a default, \code{read.acs} assumes the first three columns will
  contain geographic header information, which seems to be the standard
  for the new Census American Factfinder download site. Users can also
  set different values for the \code{geocols=} to specify other columns
  for this geographic information.  The function will use the first of
  these columns for geographic rownames to label estimates.  (By
  default, then, this would be the third column of the actual file,
  since \code{geocols=3:1}.  For files downloaded via the Census
  "legacy" version of FactFinder prior to 2012, users will probably want
  to specify \code{geocols=4:1}.

  As for column names, by default \code{read.acs} will scan the file to
  determine how many of the initial rows contain "header" information,
  and will generate new \code{acs.colnames} by concatenating information
  found in these rows.  Note that this can result in \emph{very long}
  variable names, and users may want to modify the contents of
  \code{acs.colnames} after creation.

  Alternatively, users can inspect downloaded csv files prior to import
  and specify the \code{skip=} option explicitly, as with
  \code{read.csv} and other \code{read.XXX} functions (i.e., the value
  of skip is equal to the number of rows prior to the last header row).
  Regardless of whether \code{skip=} is set or "auto", however, the
  column names will be created using all of the rows at the top of the
  file, \emph{even the "skipped" ones}.

  Finally, these new \code{acs.colnames} are used to guess intelligently
  about values for \code{acs.units}, but currently all this includes is
  a check for the word "dollars" in the names; if this is not found, the
  columns are assumed to be "counts".

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

