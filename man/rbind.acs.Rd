\name{rbind.acs}
\alias{rbind.acs}
\alias{rbind}
\alias{cbind.acs}
\alias{cbind}

\title{Combine acs Objects by Rows or Columns
}

\description{ Take a pair of acs objects and combine by _c_olumns or
     _r_ows, respectively.
}

\usage{
          \method{rbind}{acs}(e1, e2)
          \method{cbind}{acs}(e1, e2)
          }

\arguments{
  \item{e1, e2}{two acs-class objects}
}

\details{ When passed two acs-class objects, rbind (and cbind) will
first check to confirm whether the objects contain compatible data: same
endyear and span; same column names (for rbind) or geography (for
cbind).  If so, the function will return a new acs object that has
resulted from combining the two arguments row-wise or column-wise.  The
effect is essentially the same as rbind (or cbind) on the underlying
estimate and standard.error matricies, with all the additional acs
metadata tended to.}

\value{ Returns a single new acs object with all of the data contained
  in the two arguments.  }

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

