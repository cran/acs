\name{acs.tables.install}
\alias{acs.tables.install}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Downloads and stores XML tables used to lookup variable codes, table
names, and other metadata associated with acs package.
}

\description{ To obtain variable codes and other metadata needed to
  access the Census API, both \code{acs.fetch} and \code{acs.lookup}
  must consult various XML lookup files, which are provided by the
  Census with each data release.  To keep the acs package-size small, as
  of version 2.0 these files are accessed online at run-time for each
  query.  As an alternative, users may use \code{acs.tables.install} to
  download and archive all current tables (approximately 10MB, as of
  version 2.0 release).

  Use of this function is completely optional and the package should
  work fine without it (assuming the computer is online and is able to
  access the lookup tables), but running it once may result in faster
  searches and quicker downloads for all subsequent sessions.  (The
  results are saved and archived, so once a user has run the function,
  it is unnecessary to run again, unless the acs package is re-installed
  or updated.)  }

\usage{
acs.tables.install() }

\value{
Downloads the files and saves them to the package's "extdata"
directory; return an error if no files found.
}
\references{
  \url{http://www.census.gov/data/developers/data-sets.html}
  }
\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

\seealso{
\code{\link{acs.fetch}}
\code{\link{acs.lookup}}}
