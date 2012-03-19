\name{prompt.acs}
\alias{prompt.acs}
\alias{prompt}
\title{Prompt for new values for metadata in an acs object.
}
\description{
\code{prompt} provides a helper function to interactively set new values
for row- and column-names in an acs object.
}
\usage{

\method{prompt}{acs}(object, filename=NA, name=NA, what="acs.colnames",
geocols="all", ...)

}

\arguments{

  \item{object}{an acs object}

  \item{filename}{not used; provided for S3 generic/method consistency}

  \item{name}{not used; provided for S3 generic/method consistency}

  \item{what}{which acs-class metadata slot to prompt for; either
    "acs.colnames" (the default), "acs.units", or "geography"}
  
  \item{geocols}{a vector, or "all", specifying which columns from the
    geography metadata to prompt for (optional; defaults to "all"; ignored
    when \code{what="acs.colnames"})}
  
  \item{...}{not used; provided for S3 generic/method consistency}
  
}

\details{ The acs package provides this S3 \code{prompt} method for
  acs-class objects, primarily as a "helper" function to use in calls to
  \code{geography(object)<-}, \code{acs.units(object)<-}, or
  \code{acs.colnames(object)<-}.  \code{prompt} provides an interactive
  interface, \code{prompt}ing the user for new metadata values based on
  the existing ones.

  When \code{what="geography"} and \code{geocols} is not "all",
  \code{prompt} will only prompt for replacements of the values of
  geocols, but will still return values for all geography columns,
  suitable for passing to \code{geography(object)<-}.

  Anytime during the interactive \code{prompt()} session, a user may enter a
  blank line to terminate, returning only the changed values
  up to that point (along with the unchanged values for remaining entries.)}


\value{ Returns a value of the same class and dimensions as the current
  geography, acs.units, or acs.colnames of object, but with new names,
  suitable for passing to one of the replacement methods
  (\code{acs.colnames<-},   (\code{acs.units<-}, or \code{geography<-}).}

\examples{

data(kansas07)

acs.colnames(kansas07)=prompt(kansas07, what="acs.colnames")

geography(kansas07)=prompt.acs(kansas07, what="geography")


}
\seealso{

  \code{geography<-}

  \code{acs.colnames<-}

  \code{acs.units<-}

}
\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

