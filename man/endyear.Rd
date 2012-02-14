\name{endyear}
\alias{endyear<-}
\alias{endyear}
\alias{endyear,acs-method}
\alias{endyear<-,acs-method}
\title{Return or replace endyear value from the metadata of an acs object.
}

\description{ \code{endyear()} will return the (integer) value of the
latest year of the object (for example, for the 2005-2009 ACS survey,
\code{endyear} = 2009.)  When used for assignment, \code{endyear<-} will
change the value of the endyear slot in an acs object, warning the user
that this is an unusual thing to do.  }

\usage{
endyear(object)

endyear(object)<-value

}

\arguments{
  \item{object}{ an acs object
  }
  \item{value}{an integer to use as the new endyear}
}

\details{ Normal operations on acs objects should not involve altering
the value of endyear (although users may wish to change the value of
currency.year for comparisons with other objects).  Sometimes endyear
may be set incorrectly when data is imported, in which case
\code{endyear<-} may be necessary.}

\value{
  Returns (or replaces) an integer value from the \code{endyear} slot of an object.
}

\seealso{

  \code{currency.year}, which is often what users will be intending to
  modify
  }
\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

