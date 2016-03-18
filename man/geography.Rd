\name{geography}
\alias{geography<-}
\alias{geography}
\alias{geography,acs-method}
\alias{geography<-,acs-method}
\title{Return or replace geography metadata of an acs object.
}

\description{ \code{geography()} will return the geography of an acs
  object, as a dataframe.  Depending on the format of the data at import
  (and possibly the values of \code{geocols=}, if the object was created
  with \code{read.acs}), this may have multiple columns, but the number
  of geographic rows should be the same as the number of rows of the acs
  estimates and standard errors.

  When used for assignment, \code{geography<-} will change the values
  contained in the metadata, replacing the existing dataframe with a new
  one.  To replace a single value or a limited subset, call with
  subsetting (e.g., \code{geography(object)[i,j]<-value} or
  \code{geography(object)[[i]]<-value}; note that the brackets should
  occur \emph{outside} the call -- you are subsetting the dataframe, not
  the object).

  To help with replacement operations, the package provides a new
  \code{prompt} method, which can be used to interactively set new
  values for geography (as well as other metadata); see
  \code{prompt.acs}. }

\usage{
geography(object)

geography(object)<-value

}

\arguments{
  \item{object}{ an acs object
  }
  \item{value}{a dataframe containing geographic metadata; must contain
  the same number of rows as the object }
}

\value{
  Returns (or replaces) a dataframe containing the \code{geography} slot of an object.
}

\seealso{

  \code{\link{prompt.acs}}, a helper function to interactively generate
  a new geography dataframe to be used for replacement.

  \code{\link{acs-class}}

  }
\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

\examples{
data(lawrence10)
geography(lawrence10)
str(geography(lawrence10))
}
