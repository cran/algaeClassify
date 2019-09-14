#' Transform a phytoplankton timeseries into a matrix of abundances for ordination
#'
#' @param phyto.df Name of data.frame object
#' @param abundance.var Character string: field containing abundance data.
#'	Can be NA if the dataset only contains a species list for each sampling date.
#' @param summary.type 'abundance' for a matrix of aggregated abundance,'presence.absence'
#'	for 1 (present) and 0 (absent).
#' @param taxa.name Character string: field containing taxonomic identifiers.
#' @param date.name Character string: field containing date.
#' @param format Character string: POSIX format string for formatting date column.
#' @param time.agg Character string: time interval for aggregating abundance. default is day.
#' @param fun function for aggregation. default is mean, excluding NA's.
#'
#' @export date_mat
#'
#' @return A matrix of phytoplankton abundance, with taxa in rows and time in columns.
#'         If time.agg = 'monthyear', returns a 3dimensional matrix (taxa,month,year).
#'         If abundance.var = NA, matrix cells will be 1 for present, 0 for absent
#'
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#'
#' geneva.mat1<-date_mat(lakegeneva,time.agg='month',summary.type='presence.absence')
#' geneva.mat2<-date_mat(lakegeneva,time.agg='month',summary.type='abundance')
#'
#' geneva.mat1
#' geneva.mat2

date_mat<-function(phyto.df,abundance.var='biovol_um3_ml',summary.type='abundance',taxa.name='phyto_name',
                   date.name='date_dd_mm_yy',format='%d-%m-%y',
                   time.agg=c('day','month','year','monthyear'),fun=mean_naomit)
{
  time.agg<-time.agg[1]

  if(summary.type=='presence.absence' & !is.na(abundance.var)) #option to create a presence/absence matrix even if abundances are known
  {
	phyto.df$presence=0
	phyto.df$presence[!is.na(phyto.df[[abundance.var]]) & phyto.df[[abundance.var]]>0]=1
	abundance.var='presence'
  }

  if(is.na(abundance.var)) #if only a species list, create a presence/absence matrix
  {
    phyto.df$presence=1
    abundance.var='presence'
	summary.type='presence.absence'
  }

  phyto.df[[date.name]]=as.character(phyto.df[[date.name]])
  phyto.df$date_dd_mm_yy=as.POSIXct(phyto.df$date_dd_mm_yy,format=format)

  phyto.df[[taxa.name]]=as.character(phyto.df[[taxa.name]])

  if(time.agg=='month')
  {
    phyto.df$month=lubridate::month(phyto.df$date_dd_mm_yy)
    phyto.agg=stats::aggregate(stats::formula(paste(abundance.var,'~',taxa.name,'+ month')),data=phyto.df,FUN=fun)

    phyto.mat=tapply(phyto.agg[[abundance.var]],list(phyto.agg$month,phyto.agg[[taxa.name]]),fun)
    phyto.mat[is.na(phyto.mat)]=0

    return(phyto.mat)
  }else if(time.agg=='year')
  {
    phyto.df$year=lubridate::year(phyto.df$date_dd_mm_yy)
    phyto.agg=stats::aggregate(stats::formula(paste(abundance.var,'~',taxa.name,'+ year')),data=phyto.df,FUN=fun)

    phyto.mat=tapply(phyto.agg[[abundance.var]],list(phyto.agg$year,phyto.agg[[taxa.name]]),fun)
    phyto.mat[is.na(phyto.mat)]=0

    return(phyto.mat)
  }else if(time.agg == 'yearmonth')
  {
    phyto.df$year=lubridate::year(phyto.df$date_dd_mm_yy)
    phyto.df$month=lubridate::month(phyto.df$date_dd_mm_yy)

    phyto.agg=stats::aggregate(stats::formula(paste(abundance.var,'~',taxa.name,'+ year +month')),data=phyto.df,FUN=mean.nona)

    phyto.mat=tapply(phyto.agg[[abundance.var]],list(phyto.agg$month,phyto.agg[[taxa.name]],phyto.agg$year),mean.nona)
    phyto.mat[is.na(phyto.mat)]=0

    return(phyto.mat)
  }

  mean.nona=function(x) mean(x[!is.na(x)])

  phyto.agg=stats::aggregate(stats::formula(paste(abundance.var,'~',taxa.name,'+',date.name)),data=phyto.df,FUN=mean.nona)

  phyto.mat=tapply(phyto.agg[[abundance.var]],list(phyto.agg[[date.name]],phyto.agg[[taxa.name]]),mean.nona)
  phyto.mat[is.na(phyto.mat)]=0

  return(phyto.mat)

}

#' Compute mean value while ignoring NA's
#'
#' @param x A numeric vector that may contain NA's
#'
#' @export mean_naomit
#'
#' @return the mean value
#'
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#'
#' mean_naomit(lakegeneva$biovol_um3_ml)

mean_naomit<-function(x) {mean(x[!base::is.na(x)])}
