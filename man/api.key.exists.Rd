\name{api.key.exists}
\alias{api.key.exists}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Determines whether a US Census API key has been installed with
\code{api.key.install()}.
}
\description{
The \code{acs.fetch} function requires an "API key" to use when 
downloading data from the US Census API. Other packages that depend 
on \code{acs.fetch()} can use this function to determine whether they 
\code{acs.fetch()} can be called. This is especially useful for other 
packaages to test integration with the acs package, and automatically 
determine if the integration test should be skipped until the 
user completes acs setup.
}
\usage{
if( ! acs::api.key.exists() ) {
	# Skip test...
}
}
\value{
Returns True if a key has been set. Does not test whether the key is valid.
}
\references{
To request an API key, see \url{http://www.census.gov/developers/}
}
\author{
Josiah Johnston \email{josiah.johnston@berkeley.edu}
}

\seealso{
\code{\link{api.key.install}}
}
