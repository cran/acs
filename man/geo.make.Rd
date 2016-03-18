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
subdivision, place, tract, block group, msa, csa, puma, and more -- see
arguments), and the function returns a new geo.set object holding this
information.

When specifying state, county, county subdivision, place, american
indian area, and/or any of the state school district arguments,
\code{geo.make} will accept either FIPS codes or common geographic
names, and will try to match on partial strings; there is also limited
support for regular expressions, but by default the searches are case
sensitive and matches are expected at the start of names. (For example,
\code{geo.make(state="WA", county="Kits")} should find Kitsap County,
and the more adventurous \code{yakima=geo.make(state="Washi",
county=".*kima")} should work to create the a geo.set for Yakima
county.)

Other geographies (including tract, block.group, csa, msa, region,
division, urban.area, necta, puma, zip.code. and/or
congressional.district) can only be specified by FIPS codes (or "*" for
all).

Tracts should be specified as six digit numbers, although initial zeroes
may be removed; note that trailing zeroes are often removed in common
usage, so a tract that may be referred to as "tract 243" is technically
FIPS code 24300; likewise "tract 3872.01" is FIPS code 387201 for the
purposes of geo.make.
}
\usage{
geo.make(us, region, division, state, county, county.subdivision,
  place, tract, block.group, msa, csa, necta, urban.area,
  congressional.district, state.legislative.district.upper,
  state.legislative.district.lower, puma, zip.code,
  american.indian.area, school.district.elementary,
  school.district.secondary, school.district.unified,
  combine = F, combine.term = "aggregate", check = FALSE, key = "auto")
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{us}{either the number 1, the character "*", or TRUE, indicating
    whether the geo.set should contain data for the entire U.S.; if
    selected, no other geography options may be specified; setting us
    corresponds to using census summary level 010.}

  \item{region}{a numeric code (or wildcard "*" for all) corresponding
    to the desired FIPS region (e.g., region=1 for Census Northeast
    Region); if selected, no other geography options may be specified;
    setting region corresponds to using census summary level 020.}

  \item{division}{a numeric code (or wildcard "*" for all) corresponding
    to the desired FIPS division (e.g., division=4 for Census West North
    Central Division); if selected, no other geography options may be
    specified; setting division corresponds to using census summary
    level 030.}

  \item{american.indian.area}{either the numeric code (or wildcard "*"
    for all) corresponding to the desired FIPS American Indian
    Area/Alaska Native Area/Hawaiian Home Land, or a character string to
    match in the names of these Census areas; if selected, no other
    geography options may be specified; setting american.indian.area
    corresponds to using census summary level 250.}

  \item{state}{either the two-digit numeric FIPS code for the state, the
    two-letter postal abbreviation, or a character string to match in
    the state name (or wildcard "*" for all); setting state without
    other options corresponds to using census summary level 040, but it
    may be used in conjunction with other summary levels below.}

  \item{county}{either the numeric FIPS code (or wildcard "*" for all)
    for the county or a character string to match in the county name;
    setting state and county without other options corresponds to using
    census summary level 050, but they may be used in conjunction with
    other summary levels below.}

  \item{county.subdivision}{either the numeric FIPS code (or wildcard
    "*" for all) for the county subdivision or a character string to
    match in the county subdivision name; setting state, county, and
    county.subdivision without other options corresponds to using census
    summary level 060.}

  \item{place}{either the numeric FIPS code (or wildcard "*" for all)
    for the place or a character string to match in the place name;
    setting state and place without other options corresponds to using
    census summary level 160.}

  \item{tract}{a six digit numeric FIPS code (or wildcard "*" for all)
    for the census tract, including trailing zeroes; remove decimal
    points; leading zeroes may be omitted; see description; tract may be
    used with state and county to create geo.sets for census summary
    levels 140, and with state, county, and block.group for summary
    level 150.}

  \item{block.group}{the numeric FIPS code (or wildcard "*" for all) for
    the block.group; block.group may be used with state, county, and
    tract to create geo.sets for census summary levels 150.}

  \item{msa}{a numeric code (or wildcard "*" for all) corresponding to
    the desired FIPS metropolitan statistical area/micropolitan
    statistical area (e.g., msa=10100 for Aberdeen, SD micropolitan
    statistical area); setting msa without other options corresponds to
    using census summary level 310, but it may be used in conjunction
    with state for summary level 320.}

  \item{csa}{a numeric code (or wildcard "*" for all) corresponding to
    the desired FIPS combined statistical area (e.g., csa=104 for Census
    Albany-Schenectady-Amsterdam, NY CSA); setting csa without other
    options corresponds to using census summary level 330, but it may be
    used in conjunction with state for summary level 340.}

  \item{necta}{a numeric code (or wildcard "*" for all) corresponding to
    the desired FIPS New England City and Town Area (e.g., necta=77650
    for Rutland, VT Micropolitan NECTA); if selected, no other
    geography options may be specified; setting necta corresponds to
    using census summary level 350.}

  \item{urban.area}{a numeric code (or wildcard "*" for all)
    corresponding to the desired FIPS urban area (e.g., urban.area=3169
    for Aromas, CA Urban Cluster); if selected, no other
    geography options may be specified; setting urban.area corresponds
    to using census summary level 400.}

  \item{congressional.district}{a numeric code (or wildcard "*" for all)
    corresponding to the desired FIPS congressional district (e.g.,
    state="ME" and congressional.district=1 for Maine's first
    congressional district); setting state and congressional.district
    without other options corresponds to using census summary level 500,
    but they may be used in conjunction with county for summary level
    510.}

  \item{state.legislative.district.upper}{a numeric or character code
    (or wildcard "*" for all) corresponding to the desired FIPS state
    legislative district (upper chamber); these codes vary from state to
    state, and are sometimes numbers (1, 2, 3, etc. in Massachusetts)
    and sometimes letters ("A", "B", "C", etc. in Alaska); setting state
    and state.legislative.district.upper without other options
    corresponds to using census summary level 610.}

  \item{state.legislative.district.lower}{a numeric or character code
    (or wildcard "*" for all) corresponding to the desired FIPS state
    legislative district (lower chamber); these codes vary from state to
    state, and are sometimes numbers (1, 2, 3, etc. in Massachusetts)
    and sometimes letters ("A", "B", "C", etc. in Alaska); setting state
    and state.legislative.district.lower without other options
    corresponds to using census summary level 620.}

  \item{puma}{a numeric code (or wildcard "*" for all) corresponding to
    the desired FIPS public use microdata area (e.g., state=10 and
    puma=103 for PUMA 103 in Delaware); setting state and puma
    without other options corresponds to using census summary level
    795.}

  \item{zip.code}{a numeric code (or wildcard "*" for all) corresponding
    to the desired zip code tabulation area (e.g., zip.code=91303 for
    zip code 91303); if selected, no other geography options may be
    specified; setting zip.code corresponds to using census summary
    level 860.}

  \item{school.district.elementary}{a numeric code (or wildcard "*" for
    all) corresponding to the desired FIPS state school district
    (elementary), or a character string to search for in the names of
    these districts; setting state and school.district.elementary
    without other options corresponds to using census summary level
    950.}

  \item{school.district.secondary}{a numeric code (or wildcard "*" for
    all) corresponding to the desired FIPS state school district
    (secondary), or a character string to search for in the names of
    these districts; setting state and school.district.secondary
    without other options corresponds to using census summary level
    960.}

  \item{school.district.unified}{a numeric code (or wildcard "*" for
    all) corresponding to the desired FIPS state school district
    (unified), or a character string to search for in the names of these
    districts; setting state and school.district.unified without other
    options corresponds to using census summary level 970.}

  \item{combine}{a logical flag to indicate whether the component
    geographies of the geo.set are to be combined when data is
    downloaded; see details.}

  \item{combine.term}{a character string to provide a label for
    aggregate geography, if data is combined}
  
  \item{check}{logical flag indicating whether to run a check for valid
    geographies with Census API; defaults to FALSE; when TRUE, a current
    API key must be provided or installed}

  \item{key}{when check=T and no API key has been previously installed
    through \code{api.key.install}, a string key may be provided here}

} 

\details{In addition to creating individual combinations of census
  geographies, users can pass vector arguments (with recycling) to
  geo.make to create sets of geographies.  Important: each set of
  arguments must match with exactly one known Census geography: if, for
  example, the names of two places (or counties, or whatever) would both
  match, the \code{geo.make} function will return an error. (To the
  development team, this seemed preferable to simply including both
  matches, since all sorts of place names might match a string, and it
  is doubtful a user really wants them all.)  The one exception to this
  "single match" rule is that for the smallest level of geography
  specified, a user can enter "*" to indicate that all geographies at
  that level should be selected.

  When creating new geographies, note, too, that not all combinations
  are valid.  In particular the package attempts to follow paths through
  the Census summary levels (such as summary level 140:
  "state-county-tract" or summary level 160: "state-place").  So when
  specifying, for example, state, county, and place, the county will be
  ignored.

  Note: when a geo.set with "combine=T" is passed to \code{acs.fetch},
  downloaded data will be aggregated in the resulting acs abject.  Some
  users may therefore wish to specify "one.zero=T" as an additional
  argument to \code{acs.fetch}; see \code{\link{sum-methods}}.  

The following table may be helpful in figuring out which options to set
for which Census summary levels.  For more information on which datasets
and endyear/span combinations are available for each summary level, see
\url{http://www.census.gov/data/developers/data-sets.html} (click each
dataset and search for "Examples and Supported Geography").

\tabular{cl}{
  SUMMARY LEVEL \tab ARGUMENTS REQUIRED \cr
  010  \tab us \cr
  020  \tab region   \cr
  030 \tab  division \cr
  040 \tab  state \cr
  050  \tab state, county \cr
  060 \tab state, county, county.subdivision \cr
  140  \tab state, county, tract \cr
  150  \tab state, county, tract, block.group  \cr
  160 \tab  state, place \cr
  250 \tab american.indian.area \cr
  310 \tab msa \cr
  320 \tab state, msa  \cr
  330 \tab csa \cr
  340 \tab state, csa  \cr
  350  \tab necta   \cr
  400  \tab urban.area  \cr
  500  \tab state, congressional.district \cr
  510    \tab state, congressional.district, county  \cr
  610    \tab state, state.legislative.district.upper  \cr
  620    \tab state, state.legislative.district.lower  \cr
  795    \tab state, puma  \cr
  860  \tab zip.code  \cr
  950  \tab state, school.district.elementary  \cr
  960  \tab state, school.district.secondary  \cr
  970  \tab state, school.district.unified  \cr
}

All other arguments/combinations will either be ignored or result in a
failure.  } \value{ Returns a geo.set class object.  }

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}
\references{
  \enumerate{
  
    \item{"acs.R: An R Package for Neighborhood-Level Data from the
      U.S. Census." Ezra Haber Glenn, Department of Urban Studies and
      Planning, Massachusetts Institute of Technology.  Presented at the
      Computers in Urban Planning and Urban Management Conference, July
      6, 2011.
      \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2171390}.}

    \item{Census API Supported Geography:
      \url{http://www.census.gov/data/developers/data-sets.html}}}}

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

# using wildcards

# all unified school districts in Kansas
geo.make(state="KS", school.district.unified="*")

# all state house districts in Alaska
geo.make(state="AK", state.legislative.district.lower="*")

# all tracts in Kings County, NY
geo.make(state="NY", county="King", tract="*")

}

