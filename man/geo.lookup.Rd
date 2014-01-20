\name{geo.lookup}
\alias{geo.lookup}

\title{
Search Census geographies 
}
\description{

When working with the acs package and the \code{acs.fetch} and
\code{geo.make} functions, it can be difficult to find exactly the right
geographic units: \code{geo.make} expects single matches to the groups
of arguments it is given, which can be problematic when trying to find
names for places or county subdivisions, which are unfamiliar to many
users (and often seem very close or redundant: e.g., knowing whether to
look for "Moses Lake city" vs. "Moses Lake CDP").  To help, the
\code{geo.lookup} function will search on the same arguments as
\code{geo.make}, but outputs all the matches for your inspection.

}
\usage{
geo.lookup(state, county, county.subdivision, place,
  american.indian.area, school.district, school.district.elementary,
  school.district.secondary, school.district.unified)
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

  \item{place}{either the numeric FIPS code for the place or a character
    string to match in the place name}

  \item{american.indian.area}{either the numeric FIPS code for the
    American Indian Area/Alaska Native Area/Hawaiian Home Land, or a
     character string to match in the names of these Census areas}
   
   \item{school.district}{either the numeric FIPS code for the state
     school district (any type), or a character string to search for in
     the names of the school districts.}
   
   \item{school.district.elementary}{either the numeric FIPS code for
    the state school district (elementary), or a character string to
    search for in the names of these elementary school districts.}

  \item{school.district.secondary}{either the numeric FIPS code for the
    state school district (secondary), or a character string to search
    for in the names of these secondary school districts.}

  \item{school.district.unified}{either the numeric FIPS code for the
    state school district (unified), or a character string to search for
    in the names of these unified school districts.}

} \details{ Unlike \code{geo.make}, \code{geo.lookup} searches for
matches anywhere in geographic names (except when dealing with state
names), and will output a dataframe showing candidates that match some
or all of the arguments.  (When multiple arguments are provided, the
logic is a little complicated: basically, with the exception of American
Indian Areas, to be included all geographies must match the given state
name; when a county and a subdivision are both given, both must match;
otherwise, geographies are included that match any --- but not
necessarily all --- of the other arguments.)  } \value{ Returns a
dataframe of the matching geographies, with one column for each of the
given search terms.  }

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

\seealso{
\code{\link{geo.make}}
}
\examples{
geo.lookup(state="WA", county="Ska", county.subdivision="oo")
geo.lookup(state="WA", county="Kit", place="Ra")

# find all counties in WA or OR with capital M or B in name
geo.lookup(state=c("WA", "OR"), county=c("M","B"))

# find all unifed school districts in Kansas with "Ma" in name
geo.lookup(state="KS", school.district.unified="Ma")

# find all american indian areas with "Hop" in name
geo.lookup(american.indian.area="Hop")
}
