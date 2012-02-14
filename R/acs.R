
library(stringr)

# .acs.unit.levels
#
# includes all valid types of units for acs estimates

.acs.unit.levels=c("count","dollars","proportion", "ratio", "other")


# .acs.dimnames()
#
# ensures that a returned acs object includes proper row (geography)
# and column (col.names) labels

.acs.dimnames=function(acs.obj){
  dimnames(acs.obj@estimate)=list(acs.obj@geography[[1]],acs.obj@acs.colnames)
  dimnames(acs.obj@standard.error)=list(acs.obj@geography[[1]],acs.obj@acs.colnames)
  acs.obj}

# .acs.combine.headers()
# 
# create metadata and row/column names for operations that merge two
# acs units into one

.acs.combine.headers=function(e1, e2, operator) {
  if (endyear(e1)==endyear(e2)) 
    ENDYEAR=endyear(e1)
  else
    ENDYEAR=NA_integer_
  if (currency.year(e1)==currency.year(e2)) 
    CURRENCY.YEAR=currency.year(e1)
  else
    CURRENCY.YEAR=NA_integer_
  if (span(e1)==span(e2))
    SPAN=span(e1)
  else
    SPAN=NA_integer_
  if (identical(geography(e1), geography(e2))) GEOGRAPHY=geography(e1)
  else {
    GEOGRAPHY=geography(e1)
    for (i in 1:length(geography(e1))){
      if (!identical(geography(e1)[[i]], geography(e2)[[i]]))
        GEOGRAPHY[[i]]=paste("(", GEOGRAPHY[[i]], operator, geography(e2)[[i]], ")", sep=" ")
    }
  }
  if (identical(acs.colnames(e1), acs.colnames(e2)))
    {ACS.COLNAMES=acs.colnames(e1)
     ACS.UNITS=acs.units(e1)}
  else
    {ACS.COLNAMES=paste("(", acs.colnames(e1), operator, acs.colnames(e2), ")", sep=" ")
     ACS.UNITS=factor("other", levels=.acs.unit.levels)
   }
  header=list(endyear=ENDYEAR, span=SPAN, currency.year=CURRENCY.YEAR , geography=GEOGRAPHY,
    acs.colnames=ACS.COLNAMES, acs.units=ACS.UNITS)
  header
}


# .acs.identify.units()
# 
# used to set units in acs object; initially assumes all units are
# "counts", then changes some to "dollars" is the word "dollars"
# matches in colnames.

.acs.identify.units=function(acs.colnames)
{
  acs.units=rep("count", length(acs.colnames))
  dollar.index=grep(pattern="dollars",x=acs.colnames)
  acs.units[dollar.index]="dollars"
  factor(acs.units, levels=.acs.unit.levels)
}


# .acs.make.constant.object()
# 
# use this to create an acs object with some constant value in the
# estimate and 0 for all the standard errors -- helpful, for example,
# to add a certain number to every value.

.acs.make.constant.object=function(value, template){
  acs.obj=template
# if given a vector, replaces by row, not by column
  acs.obj@estimate[,]=value
  acs.obj@standard.error[,]=0
  acs.obj@acs.colnames[1:length(acs.colnames(acs.obj))]=as.character(value)
  acs.obj=.acs.dimnames(acs.obj)
  acs.obj
    }

setClass(Class="acs", representation =
  representation(endyear="integer", span="integer",
  geography="data.frame", acs.colnames="character", modified="logical"
  , acs.units="factor", currency.year="integer", estimate="matrix",
  standard.error="matrix"), prototype(endyear=NA_integer_,
  span=NA_integer_, acs.units=factor(levels=.acs.unit.levels),
  currency.year=NA_integer_, modified=F))
  
is.acs=function(object){
  if (class(object)=="acs") {TRUE}
  else{FALSE}}

read.acs=function(filename, endyear="auto", span="auto",
acs.units="auto", geocols="auto", skip=1)
{
  # helper function to read and clean data
  get.acs=function(filename, geocols, skip) {
    in.data=read.csv(filename, skip=skip, na.strings=c("-", "**", "***"), stringsAsFactors=F)
    # trick to get geocols to be first four columns
    datacols=1:length(in.data)
    datacols=datacols[-geocols]
    in.data=in.data[,c(geocols,datacols)]
    in.data[in.data=="*****"]=0
    for (i in (length(geocols)+1):length(in.data)) {
      in.data[[i]]=gsub(",", "", in.data[[i]])
      in.data[[i]]=as.numeric(in.data[[i]])  
    }
    in.data
  }

  # set geocols
  if (identical(geocols, "auto"))
    {geocols=3:1
     warning("Using first three columns as geographic headers.", call.=F)}
  else
    geocols=geocols
  
  # Attempt to automatically determine endyear from filename if
  # necessary.
  
  if (endyear=="auto")
    { # try to guess end year from filename
      endyear=2000+as.integer(str_extract(str_extract(filename, "ACS_[0-9][0-9]_"), "[0-9][0-9]"))
      if(is.na(endyear)) endyear=as.integer(str_extract(filename, "200[0-9]"))
      if (is.na(endyear) | endyear<2000 | endyear>2012)
        {
          warning("Can't determine endyear from filename;\nplease set manually before proceeding.\nSetting endyear to default of 2010.\nOperations with this acs object may not be reliable...")
          endyear=2010}
      else {
        warning("Guessing endyear of " , endyear, " based on filename...", call.=F)
      }
    }
  # Attempt to automatically determine span from filename if
  # necessary.

  if (span=="auto")
    {
      span=as.integer(str_extract(str_extract(filename, "_[0-9]YR"), "[0-9]"))
      if (is.na(span)) span=as.integer(substr(str_extract(filename, "[0-9]yr"),1,1))
      if (is.na(span) | span>5) {
        warning("Can't determine span from filename;\nplease set manually before proceeding.\nSetting span to default of 1.\nOperations with this acs object may not be reliable...")
        span=1
      }
      else
        {
          warning("Guessing span of ", span, " based on filename...", call.=F)
        }
    }
  span=as.integer(span)
  endyear=as.integer(endyear) 
  if (span > 5 | span < 1){
    warning("Span out of range; returning NA")
    return(NA)}
  in.data=get.acs(filename, geocols=geocols, skip=skip)
  acs.colnames=sub(colnames(in.data[seq((length(geocols)+1),length(in.data),2)]), pattern="..Estimate.", replacement="")
                                        # create new acs object
  if (acs.units=="auto") {
                                        # try to guess variable types from filename
    acs.units=.acs.identify.units(acs.colnames)
  }
  acs.units=factor(acs.units, levels=.acs.unit.levels)
  acs.obj=new(Class="acs",
    endyear=endyear,
    span=span, 
    geography=in.data[,1:length(geocols)],
    acs.colnames=acs.colnames, 
    acs.units=acs.units,
    currency.year=endyear,
    standard.error=as.matrix(in.data[,seq((length(geocols)+2),length(in.data), 2)]),
    modified=F,
    estimate=as.matrix(in.data[,seq((length(geocols)+1),length(in.data), 2)]
      )
    )
  
# convert 90% MOE into standard error, correct for 2005 flaw
  if (endyear(acs.obj)<=2005)
    acs.obj@standard.error=acs.obj@standard.error/1.65
  else
    acs.obj@standard.error=acs.obj@standard.error/1.645
  acs.obj=.acs.dimnames(acs.obj)
  acs.obj
}

if (!isGeneric("endyear")) {
  setGeneric("endyear", def=function(object){standardGeneric("endyear")})}else{}
setMethod("endyear", "acs", function(object) object@endyear)

if (!isGeneric("span")) {
  setGeneric("span", def=function(object){standardGeneric("span")})}else{}
setMethod("span", "acs", function(object) object@span)

if (!isGeneric("geography")) {
  setGeneric("geography", def=function(object){standardGeneric("geography")})}else{}
setMethod("geography", "acs", function(object) object@geography)

if (!isGeneric("acs.colnames")) {
  setGeneric("acs.colnames", def=function(object){standardGeneric("acs.colnames")})}else{}
setMethod("acs.colnames", "acs", function(object) object@acs.colnames)

if (!isGeneric("currency.year")) {
  setGeneric("currency.year", def=function(object){standardGeneric("currency.year")})}else{}
setMethod("currency.year", "acs", function(object) object@currency.year)

if (!isGeneric("modified")) {
  setGeneric("modified", def=function(object){standardGeneric("modified")})}else{}
setMethod("modified", "acs", function(object) object@modified)

if (!isGeneric("acs.units")) {
  setGeneric("acs.units", def=function(object){standardGeneric("acs.units")})}else{}
setMethod("acs.units", "acs", function(object) object@acs.units)

if (!isGeneric("estimate")) {
  setGeneric("estimate", def=function(object, which, conf.lev, ...){standardGeneric("estimate")})}else{}
setMethod("estimate", "acs", function(object) object@estimate)

if (!isGeneric("standard.error")) {
  setGeneric("standard.error", def=function(object){standardGeneric("standard.error")})}else{}
setMethod("standard.error", "acs", function(object) object@standard.error)

setMethod(f="[", signature="acs", definition=function(x,i,j,...,drop=FALSE){
  if (missing(i)) i=1:dim(x@estimate)[1]
  if (missing(j)) j=1:dim(x@estimate)[2]
  new(Class="acs",
      endyear=endyear(x),
      span=span(x),
      geography=geography(x)[i,],
      acs.colnames=acs.colnames(x)[j],
      modified=modified(x),
      acs.units=acs.units(x)[j],
      currency.year=currency.year(x),
      estimate=estimate(x)[i,j, drop=F],
      standard.error=standard.error(x)[i,j, drop=F])
})

setReplaceMethod(f="[", signature="acs",
    definition=function(x,i,j,value){
      if (missing(i)) i=1:dim(x)[1]
      if (missing(j)) j=1:dim(x)[2]
# is value acs object? ## still need to check for metadata being the same
      if (is.acs(value) && all(dim(value)==c(length(i),length(j)))){
        if (endyear(x) != endyear(value)) {
          warning("original and replacement do not have same endyear;\nkeeping original value", call.=F)}
        if (span(x) != span(value)) {
          warning("original and replacement do not have same span;\nkeeping original value", call.=F)}
        if (currency.year(x) != currency.year(value)) {
          warning("original and replacement do not have same currency.year;\nkeeping original value", call.=F)}
        x@estimate[i,j]=value@estimate
        x@standard.error[i,j]=value@standard.error
# check for mismatch geo when not all cols changed
        if (!all(geography(x[i,j])==geography(value))){  # if not identical geogs
          if (dim(x)[2]<=length(j)){      # if changing all cols or more
            x@geography[i,]=geography(value)           # change all geo
            warning("geographies do not match but all columns changed;\nusing new geographies", call.=F)
          }else{
            warning("geographies do not match but some columns retained;\nkeeping original geography values", call.=F)
        }
        }
        if (!all(acs.colnames(x[i,j])==acs.colnames(value))){   # if not identical colnames
          if (dim(x)[1]<=length(i)){             # if not changing all rows or more
            x@acs.colnames[j]=acs.colnames(value)
            warning("acs.colnames do not match but all rows changes;\nusing new acs.colnames", call.=F)
          }else{
            warning("acs.colnames do not match but some rows retained;\nkeeping original acs.colnames", call.=F)
          }
        }
                                            # is value two item list?
      } else if (is.list(value) && length(value)==2) {
          if (is.null(value$estimate)) x@estimate[i,j]=value[[1]]
          else x@estimate[i,j]=value$estimate
          if (is.null(value$standard.error)){
            if (is.null(value$error)) x@standard.error[i,j]=value[[2]]
            else x@standard.error[i,j]=value$error
          } else x@standard.error[i,j]=value$standard.error
          ## standard.error --> 0
                                      # is value a single number?
          ## } else if (is.numeric(value) && (length(value)==1)) {
          ##   x@estimate[i,j]=value
          ##   x@standard.error[i,j]=0
          ##                                 # is value a vector of numbers?
          ## } else if (is.numeric(value) && (length(value)==length(x[i,j]))){
          ##   x@estimate[i,j]=value
          ##   x@standard.error[i,j]=0
          ## next stanza does the work of both of the previous:
        } else if (is.numeric(value)) {
          x@estimate[i,j]=value
          x@standard.error[i,j]=0
        } else {stop("incompatible objects or dimensions;\nunable to parse for replacement", call.=F)}
      x@modified=T
      x=.acs.dimnames(x)   # in case geography or acs.colnames changed
      validObject(x)
      return(x)
    })

cbind.acs=function(e1, e2) {
  if (e1@endyear != e2@endyear | e1@span != e2@span) {
    warning("** acs objects x and y must have same endyear and span;\nreturning NA **")
    return(NA)}
  if (identical(geography(e1), geography(e2))) GEOGRAPHY=geography(e1)
  else {warning( "geographies do not appear to match; using first geography")
        GEOGRAPHY=geography(e1)}
  NEW.ESTIMATE=cbind(estimate(e1), estimate(e2))
  NEW.ERROR=cbind(standard.error(e1), standard.error(e2))
  acs.obj=new(Class="acs", endyear=endyear(e1), span=span(e1), modified=T, geography=GEOGRAPHY, acs.units=factor(c(acs.units(e1),acs.units(e2)), levels=.acs.unit.levels), currency.year=currency.year(e1), acs.colnames=c(acs.colnames(e1), acs.colnames(e2)), estimate=NEW.ESTIMATE, standard.error=NEW.ERROR)
  acs.obj=.acs.dimnames(acs.obj)
  acs.obj
}

rbind.acs=function(e1, e2) {
  if (e1@endyear != e2@endyear | e1@span != e2@span) {
    warning("** acs objects x and y must have same endyear and span;\nreturning NA **")
    return(NA)}
  if (identical(acs.colnames(e1), acs.colnames(e2))) ACS.COLNAMES=acs.colnames(e1)
  else {warning( "columns do not appear to match; using first colnames")
        ACS.COLNAMES=acs.colnames(e1)}
  GEOGRAPHY=rbind(geography(e1), geography(e2))
  NEW.ESTIMATE=rbind(estimate(e1), estimate(e2))
  NEW.ERROR=rbind(standard.error(e1), standard.error(e2))
  acs.obj=new(Class="acs", endyear=endyear(e1), span=span(e1), modified=T, geography=GEOGRAPHY, acs.units=acs.units(e1), currency.year=currency.year(e1), acs.colnames=acs.colnames(e1), estimate=NEW.ESTIMATE, standard.error=NEW.ERROR)
  acs.obj=.acs.dimnames(acs.obj)
  acs.obj
}

setMethod("+", signature(e1 = "acs", e2 = "acs"), function(e1, e2) {
  header=.acs.combine.headers(e1,e2,"+")
  NEW.ESTIMATE=estimate(e1)+estimate(e2)
  NEW.ERROR=sqrt(standard.error(e1)^2+standard.error(e2)^2)
  acs.obj=new(Class="acs",
    endyear=header$endyear,
    span=header$span,
    modified=T,
    geography=header$geography,
    acs.units=header$acs.units,
    currency.year=header$currency.year,
    acs.colnames=header$acs.colnames,
    estimate=NEW.ESTIMATE,
    standard.error=NEW.ERROR)
  acs.obj=.acs.dimnames(acs.obj)
  acs.obj
}
         )

setMethod("-", signature(e1 = "acs", e2 = "acs"), function(e1, e2) {
  header=.acs.combine.headers(e1, e2, "-")
  NEW.ESTIMATE=estimate(e1)-estimate(e2)
  NEW.ERROR=sqrt(standard.error(e1)^2+standard.error(e2)^2)
  acs.obj=new(Class="acs",
    endyear=header$endyear,
    span=header$span,
    modified=T,
    geography=header$geography,
    acs.units=header$acs.units,
    currency.year=header$currency.year,
    acs.colnames=header$acs.colnames,
    estimate=NEW.ESTIMATE,
    standard.error=NEW.ERROR)
  acs.obj=.acs.dimnames(acs.obj)
  acs.obj
}
         )

.acs.divider=function(num, den, proportion) {
  if (proportion==T) header=.acs.combine.headers(num, den, "/")
  else header=.acs.combine.headers(num, den, ":")
  p=estimate(num)/estimate(den)
  if (proportion==T & all((p^2 * standard.error(den)^2)>0)){
    header$acs.units=factor("proportion", levels=.acs.unit.levels)
    warning("** using formula for PROPORTIONS, which assumes that numerator is a SUBSET of denominator **")
    NEW.ERROR=sqrt(standard.error(num)^2 - (p^2 * standard.error(den)^2))/estimate(den)}
  else {
#   a recommended correction when term under sqrt is negative
    if (proportion==T){
      warning("** due to the nature of some of the errors, using the more conservative formula for RATIOS, which assumes that numerator is not a subset of denominator **")}
    header$acs.units=factor("ratio", levels=.acs.unit.levels)
    NEW.ERROR=sqrt(standard.error(num)^2 + (p^2 * standard.error(den)^2))/estimate(den)
  }
  acs.obj=new(Class="acs",
    endyear=header$endyear,
    span=header$span,
    modified=T,
    geography=header$geography,
    acs.units=header$acs.units,
    currency.year=header$currency.year,
    acs.colnames=header$acs.colnames,
    estimate=p,
    standard.error=NEW.ERROR)
  acs.obj=.acs.dimnames(acs.obj)
  acs.obj}
    
setMethod("/", signature(e1 = "acs", e2 = "acs"), function(e1, e2) {
# by default, use more conservative "ratio-style" dividing
  warning("** using the more conservative formula for ratio-type
  dividing, which does not assume that numerator is a subset of
  denominator; for more precise results when seeking a PROPORTION
  and not a ratio, see FUNCTION-I-NEED-TO-WRITE **")
  .acs.divider(num=e1, den=e2, proportion=F)
})

setMethod("*", signature(e1 = "acs", e2 = "acs"), function(e1, e2) {
  header=.acs.combine.headers(e1, e2, "*")
  NEW.ESTIMATE=estimate(e1)*estimate(e2)
  NEW.ERROR=sqrt((estimate(e1)^2*standard.error(e2)^2)+(estimate(e2)^2*standard.error(e1)^2))
  acs.obj=new(Class="acs",
    endyear=header$endyear,
    span=header$span,
    modified=T,
    geography=header$geography,
    acs.units=header$acs.units,
    currency.year=header$currency.year,
    acs.colnames=header$acs.colnames,
    estimate=NEW.ESTIMATE,
    standard.error=NEW.ERROR)
  acs.obj=.acs.dimnames(acs.obj)
  acs.obj}
)

setMethod("+", signature(e1 = "acs", e2 = "numeric"), function(e1, e2) {
  e2=.acs.make.constant.object(value=e2, template=e1)
  e1+e2
  }
         )

# and reverse classes...

setMethod("+", signature(e1 = "numeric", e2 = "acs"), function(e1, e2) {
  e1=.acs.make.constant.object(value=e1, template=e2)
  e1+e2
  }
         )

setMethod("-", signature(e1 = "acs", e2 = "numeric"), function(e1, e2) {
  e2=.acs.make.constant.object(value=e2, template=e1)
  e1-e2
  }
         )

# ditto...

setMethod("-", signature(e1 = "numeric", e2 = "acs"), function(e1, e2) {
  e1=.acs.make.constant.object(value=e1, template=e2)
  e1-e2
  }
         )

setMethod("/", signature(e1 = "acs", e2 = "numeric"), function(e1, e2) {
  e2=.acs.make.constant.object(value=e2, template=e1)
  e1/e2
  }
         )

# ditto...

setMethod("/", signature(e1 = "numeric", e2 = "acs"), function(e1, e2) {
  e1=.acs.make.constant.object(value=e1, template=e2)
  e1/e2
  }
         )

setMethod("*", signature(e1 = "acs", e2 = "numeric"), function(e1, e2) {
  e2=.acs.make.constant.object(value=e2, template=e1)
  e1*e2
  }
         )

# ditto...

setMethod("*", signature(e1 = "numeric", e2 = "acs"), function(e1, e2) {
  e1=.acs.make.constant.object(value=e1, template=e2)
  e1*e2
  }
         )

setMethod("show", signature(object = "acs"), function(object) {
  if(is.na(span(object)) | is.na(endyear(object))) years="NO MEANINGFUL YEAR"
  else
    if(span(object)==1) years=endyear(object)
    else years=paste(endyear(object)-span(object)+1,"--",endyear(object))
  cat("ACS DATA: \n", years, "; ")
  cat("currency in", currency.year(object), "dollars\n\n")
  cat("Estimates w/90% confidence intervals; for different intervals, see confint\n")
  est=estimate(object)
  err=standard.error(object)
  output=matrix(paste(est, "+/-", 1.645*err), nrow=nrow(est))
  dimnames(output)=dimnames(est)
  print(output, quote=FALSE)
})

setMethod("summary", signature(object = "acs"), function(object) {
  if(span(object)==1) years=endyear(object)
  else years=paste(endyear(object)-span(object)+1,"--",endyear(object))
  cat("ACS DATA: \n", years,"\n")
  cat("----------\nESTIMATES:\n")
  print(summary(estimate(object)))
  cat("----------\n90% MARGINS OF ERROR:\n")
  print(summary(standard.error(object)))
})

dim.acs=function(x){
  dim(estimate(x))}

length.acs=function(x){
  length(estimate(x))}

confint.acs=function(object, parm="all", level=.95, alternative="two.sided",...) {
  if (parm[1]=="all") parm=1:dim(object)[2]
  z.upper=switch(alternative, two.sided=qnorm((1+level)/2), greater=Inf, less=qnorm(level))
  z.lower=switch(alternative, two.sided=qnorm((1+level)/2), greater=qnorm(level), less=Inf)
  labels=switch(alternative, two.sided=c((1-level)/2 , 1 - (1-level)/2), less=c(0,level), greater=c(1-level,1))
  labels=paste(100*labels, "%", sep=" ")
  RESULT=list()
  for (i in parm) {
    conf.int.lower=estimate(object[,i])-standard.error(object[,i])*z.lower
    conf.int.upper=estimate(object[,i])+standard.error(object[,i])*z.upper
    RESULT[[acs.colnames(object)[i]]]=data.frame(conf.int.lower, 
            conf.int.upper,
            row.names=geography(object)[[1]])
    names(RESULT[[acs.colnames(object)[i]]])=labels
  }
  RESULT
}

setMethod("sum", signature(x = "acs"), function(x,
  agg.term=c("aggregate", "aggregate"), ..., na.rm=FALSE) {
  if(length(agg.term)<2){agg.term[2]=agg.term[1]}
  est=estimate(x)
  err=standard.error(x)
  if (dim(est)[1]==1){ geography=geography(x)} # single row
  else {
    geography=geography(x[1,1])
    for (i in 1:length(geography)){
      geography[,i]=agg.term[1]}
  }
  if (dim(est)[2]==1){acs.units=acs.units(x) # single column
                      acs.colnames=acs.colnames(x)
                    }
  else {acs.units=factor(levels=.acs.unit.levels)
                      acs.colnames=agg.term[2]}
  ESTIMATE=as.matrix(sum(est))
  ERROR=as.matrix(sqrt(sum(err^2)))
  acs.obj=new(Class="acs",
    endyear=endyear(x),
    span=span(x),
    modified=T,
    geography=geography,
    acs.units=acs.units,
    currency.year=currency.year(x),
    acs.colnames=acs.colnames,
    estimate=ESTIMATE,
    standard.error=ERROR)
  acs.obj=.acs.dimnames(acs.obj)
  acs.obj
})

.apply.acs=function(X, MARGIN, FUN, ...) 
   {
     FUN=match.fun(FUN)
     if (identical(MARGIN, 1)){       # apply row-wise
        acs.obj=FUN(X[,1], ...)
        if(dim(X)[2]>1){
       for (i in 2:dim(X)[2]){
         acs.obj=cbind(acs.obj, FUN(X[,i], ...))
      }}
      }
      if (identical(MARGIN, 2)){     # apply col-wise
        acs.obj=FUN(X[1,], ...)
        if (dim(X)[1]>1){
          for (i in 2:dim(X)[1]){
          acs.obj=rbind(acs.obj, FUN(X[i,], ...))
        }}
      }
 # I think the next part works, except it fails because the stuff above
 # doesn't like single rows or single columns...
 #    if (identical (MARGIN, c(1,2))){
 #      acs.obj=apply(apply(X, MARGIN=1, FUN=FUN), MARGIN=2,
 #      FUN=FUN)}

# or maybe this...?
     if (all(MARGIN==c(1,2))){
       acs.obj=FUN(apply(X, MARGIN=2, FUN=FUN, ...), ...)}
     acs.obj}
     

if (!isGeneric("apply")) {
  setGeneric("apply", def=function(X, MARGIN, FUN, ...){standardGeneric("apply")})}else{}
setMethod("apply", signature="acs", def=function(X, MARGIN, FUN, ...){.apply.acs(X, MARGIN, FUN, ...)})

if (!isGeneric("acs.colnames<-")) {
          setGeneric("acs.colnames<-", def=function(x, value){standardGeneric("acs.colnames<-")})}else{}
        
        #  setGeneric("acs.colnames<-", function(x, value) standardGeneric("acs.colnames<-"))
          
        setReplaceMethod(f="acs.colnames", signature="acs",
                         definition=function(x,value){
                           x@acs.colnames=value
                           x=.acs.dimnames(x)
                           x@modified=T
                           validObject(x)
                           return(x)
                         })
        
        if (!isGeneric("geography<-")) {
          setGeneric("geography<-", def=function(object, value){standardGeneric("geography<-")})}else{}
        
        setReplaceMethod(f="geography", signature="acs",
                         definition=function(object,value){
                           object@geography=value
                           object=.acs.dimnames(object)
                           object@modified=T
                           validObject(object)
                           return(object)
                         })
        
        
        if (!isGeneric("endyear<-")) {
          setGeneric("endyear<-", function(object, value) standardGeneric("endyear<-"))}else{}
        
        setReplaceMethod(f="endyear", signature="acs",
                         definition=function(object,value){
                           warning(paste(
                                         "Changing value of endyear from ",
                                         endyear(object),
                                         " to ",
                                         value,
                                         ".\nThis is an unusual thing to do, unless the original value was incorrect.\nAlso changing value of currency.year to",
                                         value,
                                         ", without converting currency values.\nPlease see ?endyear and ?currency.year for more information",
                                         sep="")
                                   , call.=F) 
                           object@endyear=as.integer(value)
                           object@currency.year=as.integer(value)
                           object@modified=T
                           validObject(object)
                           return(object)
                         })
        
if (!isGeneric("span<-")) {
  setGeneric("span<-", function(x, value) standardGeneric("span<-"))}else{}
        
setReplaceMethod(f="span", signature="acs",
                 definition=function(x,value){
                   warning(paste("Changing value of span from ",
                                 span(x), " to ", value, ".\nThis is an unusual
                         thing to do, unless the original value was
                         incorrect.\nSee ?acs for more information",
                                 sep=""), call.=F) 
                   x@span=as.integer(value)
                   x@modified=T
                   validObject(x)
                   return(x)
                 })
       
if (!isGeneric("acs.units<-")) {
  setGeneric("acs.units<-", function(x, value) standardGeneric("acs.units<-"))}else{}
        
setReplaceMethod(f="acs.units", signature="acs",
                 definition=function(x,value){
                   x@acs.units=factor(value, levels=.acs.unit.levels)
                   x@modified=T
                   validObject(x)
                   return(x)
                 })

if (!isGeneric("currency.year<-")) {
  setGeneric("currency.year<-", def=function(object, value){standardGeneric("currency.year<-")})}else{}

setReplaceMethod(f="currency.year", signature="acs",
                 definition=function(object, value){
                   currency.convert(object, rate="auto", newyear=value)
                 })
      
currency.convert=function(object, rate="auto", newyear=NA_integer_, verbose=F){
  if (rate=="auto"){
    .env=environment()
    data("cpi", envir=.env)
    new.rate="cpi[as.character(newyear)]"
    new.rate=eval(parse(text=new.rate))
    curr.rate="cpi[as.character(currency.year(object))]"
    curr.rate=eval(parse(text=curr.rate))
    rate=new.rate/curr.rate}
  dollar.cols=which(acs.units(object)=="dollars")
  if (verbose) {
    if (!missing(newyear)){
      output=c(paste("CPI (base 1982-84) for ", currency.year(object),
  " = ", curr.rate, sep=""),
      "\n",
      paste("CPI (base 1982-84) for ", newyear, " = ", new.rate, sep=""),
      "\n",
      paste("$1.00 in ", currency.year(object), " dollars = $", round(rate,2), " in ", newyear, " dollars.", sep=""),
        "\n")} else {output=c(paste("$1.00 in ",
                       currency.year(object),
                       " dollars = $",
                       round(rate,2),
                       " in converted dollars.",
                       sep=""),
                       "\n")}
    output=c(output, "Converting the following columns:", "\n",
  paste(acs.colnames(object)[dollar.cols], "\n", sep=""))
    warning(output, call.=F)
  }
  for (i in dollar.cols){
    object@estimate[,i]=object@estimate[,i]*rate
    object@standard.error[,i]=object@standard.error[,i]*rate
  }
  object@currency.year=as.integer(newyear)
  object@modified=T
  validObject(object)
  return(object)
}
      
      # helper function for replacing geography or acs.colnames
      
      prompt.acs=function(object, filename=NA, name=NA,
        what="acs.colnames", geocols="all", ...){
        print("To end session, enter a blank line.")
        if (what=="geography"){
          if (geocols=="all") geocols=1:dim(geography(object))[2]
          value=geography(object)
          for (j in geocols){
            for (i in 1:dim(geography(object))[1]){
              line.input=readline(prompt=paste("Change ", value[i,j]," to: \n", sep=""))
              if (line.input=="") {break} else {value[i,j]=line.input}
            }
          }
        }
        else if (what=="acs.colnames"){
          value=acs.colnames(object)
          for (i in 1:length(acs.colnames(object))){
            line.input=readline(prompt=paste("Change ", value[i]," to: \n", sep=""))
            if (line.input=="") {break} else {value[i]=line.input}
          }
        } else if (what=="acs.units"){
          value=acs.units(object)
          input=rep("", length(value))
          print("Type [c]ount, [d]ollars, [p]roportion, [r]atio, or [o]ther.")
          for (i in 1:length(value)){
            line.input=readline(prompt=paste(acs.colnames(object)[i], " is currently in these units: ", value[i],".  Change to what units?: (c,d,p,r,o)\n", sep=""))
            if (line.input=="") {break} else {input[i]=line.input}            
          }
          for (i in .acs.unit.levels){
            value[input==substr(start=1, stop=1,i)]=i}
        } else{
          value=NA
          warning(paste("prompt can only prompt for \"geography\", \"acs.units\", or \"acs.colnames\", not \""
                        , what, "\"", sep=""))}
        value
      }

setMethod("plot",
    signature(x = "acs"),
    function (x, conf.level=.95, err.col="red", err.lwd=1,
    err.pch="-", err.cex=2, err.lty=2, x.res=300, labels="auto", by="geography",...) 
    {
      # internal plot function to plot individual x-y plots with conf
      # intervals, assume that either i or j or length 1
      plot.xy.acs=function(x, object, conf.int.upper, conf.int.lower, estimates, labels, xlab, ylab, ...){
        if(missing(xlab)) xlab=""
        if(missing(ylab)) ylab=""       
        plot(rep(x,2), c(conf.int.upper, conf.int.lower), type="n", xaxt="n", xlab=xlab, ylab=ylab,...)
        axis(side=1, labels=labels, at=x, ...)
        lines(x=matrix(c(x,x,rep(NA, length(x))), ncol=length(x), byrow=T), matrix(c(conf.int.lower, conf.int.upper, rep(NA, length(x))), ncol=length(x), byrow=T), col=err.col, lwd=err.lwd, lty=err.lty)
        points(x, conf.int.upper, pch=err.pch, cex=err.cex, col=err.col)
        points(x, conf.int.lower, pch=err.pch, cex=err.cex, col=err.col)
        points(x, estimates,...)
      }
      acs.density.plot=function(x, type="l", xlim,
        xlab=acs.colnames(x), ylab="Density Distribution",
        conf.level, col="black", err.col, err.lwd, err.lty,
        x.res, ...){
        est=estimate(x)
        err=standard.error(x)
        if (missing(xlim)) xlim=c(est-(4*err), est+(4*err))
        x.vals=seq(from=xlim[1], to=xlim[2], by=(xlim[2]-xlim[1])/x.res)
        plot(x.vals, dnorm(x.vals, mean=est, sd=err), type=type,
        xlab=xlab, ylab=ylab, col=col,...)
      if (conf.level) {abline(v=qnorm(mean=est, sd=err, p=c(((1-conf.level)/2), (1-((1-conf.level)/2)))), col=err.col, lwd=err.lwd, lty=err.lty)}
        }
      i=dim(x)[1]
      j=dim(x)[2]
      if (length(x)==1) {
        acs.density.plot(x, conf.level=conf.level, err.col=err.col,
        err.lwd=err.lwd, err.lty=err.lty, x.res=x.res, ...)
      } else if (i == 1 | j == 1){
        con=confint(x, level=conf.level)
        conf.int.upper=NA
        conf.int.lower=NA
        estimates=NA
        if (i == 1) {                               # one row
          if (identical(labels, "auto")) labels=acs.colnames(x)
          for (k in 1:j){
            conf.int.upper[k]=as.numeric(con[[k]][2])
            conf.int.lower[k]=max(0, as.numeric(con[[k]][1]))
            estimates[k]=estimate(x)[1,k]
          }}
        else        {
          if (identical(labels, "auto")) labels=geography(x)[[1]]
          for (k in 1:i){                       # one column
            conf.int.upper[k]=as.numeric(con[[1]][k,2])
            conf.int.lower[k]=max(0, con[[1]][k,1])
            estimates[k]=estimate(x)[k,1]
          }}
        plot.xy.acs(x=1:max(i,j), object=x, conf.int.upper=conf.int.upper, conf.int.lower=conf.int.lower, estimates=estimates, labels=labels,...)
      } else {
        if (by=="geography"){
          par(mfrow=c(i, 1))
        for (k in 1:i){
          plot(x[k,], sub=geography(x)[k,1], conf.level=conf.level, err.col=err.col, err.lwd=err.lwd, err.pch=err.pch, err.cex=err.cex, err.lty=err.lty, labels=labels,...)
        }        
        } else if (by=="acs.colnames") {
         par(mfrow=c(1,j))
        for (k in 1:j){
          plot(x[,k], sub=acs.colnames(x)[k], conf.level=conf.level,
      err.col=err.col, err.lwd=err.lwd, err.pch=err.pch,
      err.cex=err.cex, err.lty=err.lty, labels=labels,...)
       }        
       }
      }
    }
          )
