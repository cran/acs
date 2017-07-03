\name{api.key.install}
\alias{api.key.install}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Installs an API key from the US Census to use with calls
to \code{acs.fetch}.
}
\description{
The \code{acs.fetch} function requires an "API key" to use when
downloading data from the US Census API.  Rather than pass this rather
long string to the function each time, users can save the key as part of
the package installation, using the \code{api.key.install} function.
Once installed, an api key is saved on the system and available for use
in future sessions.  (To replace a key, simply call the function again
with the new key.)
}
\usage{
api.key.install(key, file = "key.rda")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{key}{
The API key provided to the user upon registering with the US Census
Developer's page.  A string.
}
  \item{file}{
An alternate name to use when storing key; reserved for future use. 
}
}

\value{
Saves the key and exits silently, unless an error is encountered.
}
\details{

The requirement for a key seems to be laxly enforced by the Census API,
but is nonetheless coded into the acs package.  Users without a key may
find success by simply installing a blank key (i.e., key="") via
\code{api.key.install(key="")}; similarly, calls to \code{acs.fetch} and
\code{geo.make(..., check=T)} may succeed with a \code{key=""} argument.
Note that while this may work today, it may fail in the future if the
API decides to start enforcing the requirement.

  }
\references{
To request an API key, see \url{http://www.census.gov/developers/}
}
\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

\seealso{
\code{\link{acs.fetch}}
}
