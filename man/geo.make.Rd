\name{geo.make}
\alias{geo.make}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a new geo.set object for use with the acs package.
}

\description{ The \code{geo.make} function is used to create new
user-specified geographies for use with the \code{acs.fetch} function of
the acs package.  At the most basic level, a user specifies some
combination of existing census levels (state, county, county
subdivision, place, tract, and/or block group), and the function returns
a new geo.set object holding this information.

When specifying the state, county, county subdivision, and/or place,
\code{geo.make} will accept either FIPS codes or common geographic
names, and will try to match on partial strings; there is also limited
support for regular expressions, but by default the searches are case
sensitive and matches are expected at the start of names. (For example,
\code{geo.make(state="WA", county="Kits")} should find Kitsap County,
and the more adventurous \code{yakima=geo.make(state="Washi",
county=".*kima")} should work to create the same Yakima county geo.set
as above.)  

Important: when creating new geographies, each set of arguments must
match with exactly one known Census geography: if, for example, the
names of two places (or counties, or whatever) would both match, the
\code{geo.make} function will return an error. (To the development team,
this seemed preferable to simply including both matches, since all sorts
of place names might match a string, and it is doubtful a user really
wants them all.)  The one exception to this "single match" rule is that
for the smallest level of geography specified, a user can enter "*" to
indicate that all geographies at that level should be selected.

"tract" and "block.group" can only be specified by FIPS code number (or
"*" for all); they don't really have names to use.  (Tracts should be
specified as six digit numbers, although initial zeroes may be removed;
often trailing zeroes are removed in common usage, so a tract referred
to as "tract 243" is technically FIPS code 24300, and "tract 3872.01"
becomes 387201.)

When creating new geographies, note, too, that not all combinations are
valid.  In particular the package attempts to follow paths through the
Census summary levels (such as summary level 140: "state-county-tract"
or summary level 160: "state-place").  So when specifying, for example,
state, county, and place, the county will be ignored.

}
\usage{
geo.make(state, county, county.subdivision, place, tract, block.group,
combine = F, combine.term = "aggregate", check = FALSE, key = "auto")
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{state}{either the two-digit numeric FIPS code for the state, the
    two-letter postal abbreviation, or a character string to match in
    the state name}

  \item{county}{either the numeric FIPS code for the county or a character
    string to match in the county name }

  \item{county.subdivision}{either the numeric FIPS code for the county
    subdivision or a character string to match in the county subdivision
    name}

  \item{place}{either the numeric FIPS code for the place or a character string
    to match in the place name}

\item{tract}{a six digit numeric FIPS code for the census tract,
  including trailing zeroes; remove decimal points; leading zeroes may
  be omitted; see description.  }

\item{block.group}{ the numeric FIPS code for the block.group}

\item{combine}{a logical flag to indicate whether the component
  geographies of the geo.set are to be combined when data is downloaded;
  see details.
}
  \item{combine.term}{a character string to provide a label for
  aggregate geography, if data is combined
}
  \item{check}{logical flag indicating whether to run a check for valid
  geographies with Census API; defaults to FALSE; when TRUE, a current
  API key must be provided or installed
}
  \item{key}{when check=T and no API key has been previously installed
  through \code{api.key.install}, a string key may be provided here
}

} 

\details{ Note: when a geo.set with "combine=T" is passed to
\code{acs.fetch}, downloaded data will be aggregated in the resulting
acs abject.  Some users may therefore wish to specify "one.zero=T" as an
additional argument to \code{acs.fetch}; see \code{\link{sum-methods}}.
  }
  
\value{
Returns a geo.set class object.
}

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

\seealso{
\code{\link{geo.set-class}}
}
\examples{

# some single-state geo.sets
washington=geo.make(state=53)
alabama=geo.make(state="Alab")

# a county match
yakima=geo.make(state="WA", county="Yakima")
yakima

# a multiple-county geo.set
psrc=geo.make(state="WA", county=c(33,35,53,61))
psrc

# combine geo.sets
north.mercer.island=geo.make(state=53, county=33, tract=c(24300,24400))
optional.tract=geo.make(state=53, county=33, tract=24500)
# add in one more tract to create new, larger geo
north.mercer.island.plus=north.mercer.island + optional.tract

}

