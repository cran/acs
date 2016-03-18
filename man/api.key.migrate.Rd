\name{api.key.migrate}
\alias{api.key.migrate}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
After updating the acs package, installs an archived API key from a
previous installation.
}
\description{
The \code{acs.fetch} function requires an "API key" to use when
downloading data from the US Census API.  Rather than pass this rather
long string to the function each time, users can save the key as part of
the package installation, using the \code{api.key.install} function.
Once installed, an api key is saved on the system and available for use
in future sessions.  (To replace a key, simply call the function again
with the new key.)

During the update process, this key may be lost or left in the wrong
location.  A call to \code{api.key.migrate()} can help restore an
archived key, if found.
}
\usage{
api.key.migrate()
}
%- maybe also 'usage' for other objects documented here.

\value{
Migrates the key (if found) and exits silently; return an error if no
archived key is found.
}
\references{
To request an API key, see \url{http://www.census.gov/developers/}
}
\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

\seealso{
\code{\link{acs.fetch}}
\code{\link{api.key.install}}}
