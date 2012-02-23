
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
  dimnames(acs.obj@estimate)=list(acs.obj@geography$Geography,acs.obj@acs.colnames)
  dimnames(acs.obj@standard.error)=list(acs.obj@geography$Geography,acs.obj@acs.colnames)
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
    GEOGRAPHY[[1]]=paste("(", GEOGRAPHY[[1]], operator, geography(e2)[[1]], ")", sep=" ")
    GEOGRAPHY[[2]]=paste("(", GEOGRAPHY[[2]], operator, geography(e2)[[2]], ")", sep=" ")
    GEOGRAPHY[[3]]="--"
    GEOGRAPHY[[4]]=paste("(", GEOGRAPHY[[4]], operator, geography(e2)[[4]], ")", sep=" ")}
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
  
# an older version also included this next line, but I think it can be
# deleted.

# TODO: check this.

# acs.obj=new(Class="acs", endyear=header$endyear, span=header$span,
# modified=T, geography=header$geography, acs.units=header$acs.units,
# currency.year=header$currency.year,
# acs.colnames=header$acs.colnames, estimate=NEW.ESTIMATE,
# standard.error=NEW.ERROR)

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
      for (i in 5:length(in.data)) {
        in.data[[i]]=gsub(",", "", in.data[[i]])
        in.data[[i]]=as.numeric(in.data[[i]])  
      }
      in.data
    }
  
    # set geocols
    if (geocols=="auto")
      {geocols=1:4
       warning("Using first four columns as geographic headers.", call.=F)}
    else
      if (length(geocols)==4)
        geocols=geocols
      else{
        warning("Can't parse geocols for geographic header info; returning NA")
        return(NA)}   
      
# Attempt to automatically determine endyear from filename if
    # necessary.
    
    if (endyear=="auto")
      { # try to guess end year from filename
        endyear=as.integer(str_extract(filename, "200[0-9]"))
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
        span=as.integer(substr(str_extract(filename, "[0-9]yr"),1,1))
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
    acs.colnames=sub(colnames(in.data[seq(5,length(in.data),2)]), pattern="..Estimate.", replacement="")
                                          # create new acs object
    if (acs.units=="auto") {
                                          # try to guess variable types from filename
      acs.units=.acs.identify.units(acs.colnames)
    }
    acs.units=factor(acs.units, levels=.acs.unit.levels)
    acs.obj=new(Class="acs",
      endyear=endyear,
      span=span, 
      geography=in.data[,geocols],
      acs.colnames=acs.colnames, 
      acs.units=acs.units,
      currency.year=endyear,
      standard.error=as.matrix(in.data[,seq(6,length(in.data), 2)]),
      modified=F,
      estimate=as.matrix(in.data[,seq(5,length(in.data), 2)]
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
# old way  -- prob with subsets
#        estimate=matrix(estimate(x)[i,j], nrow=length(i), dimnames=list(geography(x)[i,4], acs.colnames(x)[j])),
        estimate=estimate(x)[i,j, drop=F],
# old way -- prob with subsets
#        standard.error=matrix(standard.error(x)[i,j], nrow=length(i), dimnames=list(geography(x)[i,4], acs.colnames(x)[j])))
        standard.error=standard.error(x)[i,j, drop=F])

  })

setReplaceMethod(f="[", signature="acs",
  definition=function(x,i,j,value){
    if (is.null(value$estimate)) x@estimate[i,j]=value[[1]]
    else x@estimate[i,j]=value$estimate
    if (is.null(value$standard.error)){
      if (is.null(value$error)) x@standard.error[i,j]=value[[2]]
      else x@standard.error[i,j]=value$error
    } else x@standard.error[i,j]=value$standard.error
    x@modified=T
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
  cat("ACS DATA: \n", years,"\n\n")
  cat("ESTIMATES W/90% CONFIDENCE INTERVALS; FOR DIFFERENT INTERVALS, SEE confint()\n")
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
            row.names=geography(object)$Geography)
    names(RESULT[[acs.colnames(object)[i]]])=labels
  }
  RESULT
}
