\name{flatten.geo.set}
\alias{flatten.geo.set}
\title{
Convenience function to "flatten" a nested geo.set object.
}
\description{
In the acs package, geo.set objects may contain nested levels of geo.set
objects, which is often desired (to organize complex sets and subsets of
geography).  Sometimes, however, when combining these sets, users may
prefer to remove the nesting levels.  This convenience function will
recursively prowl through a geo.set and return a single flat geo.set
(one level deep) containing of the composite geographies.
}
\usage{
flatten.geo.set(x)
}

\arguments{
  \item{x}{ the geo.set to be flattened

}
}

\value{
  Returns a new geo.set object.
  
}

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}


\seealso{
\code{\link{geo.set-class}}
}
\examples{

# a multiple-county geo.set
psrc=geo.make(state="WA", county=c(33,35,53,61))

# combine geo.sets
north.mercer.island=geo.make(state=53, county=33, tract=c(24300,24400))
optional.tract=geo.make(state=53, county=33, tract=24500)
# add in one more tract to create new, larger geo
north.mercer.island.plus=north.mercer.island + optional.tract

# created a nested geo.set
 my.nested.geo.set=c(north.mercer.island.plus, psrc)
str(my.nested.geo.set)
length(my.nested.geo.set)

# .. and flatten in out
#    note difference in structure and length
my.flat.geo.set=flatten.geo.set(my.nested.geo.set)
str(my.flat.geo.set)
length(my.flat.geo.set)
}

