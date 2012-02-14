\name{currency.year}
\alias{currency.year}
\alias{currency.year<-}
\alias{currency.year,acs-method}
\alias{currency.year<-,acs-method}
\title{Return (or change) currency.year value from the metadata of an acs object.
}

\description{ Standard accessor/replacement method for metadata
  contained within S4 acs-class objects.  }

\usage{
currency.year(object)

currency.year(object)<-value

}

\arguments{
  \item{object}{ an acs object}
  \item{value}{an integer value to be used in replacement }
}

\details{ \code{currency.year} will return the (integer) value of the
  dollar-year of object.

  Assigning a new value to curency.year (through
  \code{currency.year(object)<-value} or
  \code{currency.year(object)=value}) will change the value of
  \code{currency.year} in the object's metadata and also modify all
  dollar values of the object (as determined by
  \code{acs.units(object)=="dollars"}) to be in the dollars of the
  desired new year.

  A related function, \code{currency.convert} provides a helper function
  to create a new copy of an acs-class object with a modified
  currency.year and converted dollar values without altering the
  original object.  When \code{rate="auto"} (the default),
  \code{currency.convert} will look up values from the \code{cpi}
  database to use in conversion.  When a numeric rate is provided
  through this option, actual \code{cpi} values are ignored.  When
  \code{verbose=T}, currency.convert will provide additional information
  about the rates of conversion and the acs.colnames converted.}


\value{
  Returns (or replaces) an integer value from the "currency.year" slot of an object.
}
\seealso{

  \code{\link{cpi}}

  \code{\link{currency.convert}}
  }

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

