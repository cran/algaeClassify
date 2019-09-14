#'Split a dataframe column with binomial name into genus and species columns.
#'Plots change in species richness over time, generates species accumulation curve, and
#'compares SAC against simulated idealized curve assuming all unique taxa have equal probability
#'of being sampled at any point in the time series. (author Dietmar Straile)
#'
#' @param b_data Name of data.frame object
#' @param phyto_name Character string: field containing phytoplankton id (species, genus, etc.)
#' @param column column name or number for field containing abundance (biomass,biovol, etc.).
#'               Can be NA if the dataset only contains a species list for each sampling date.
#' @param n number of simulations for randomized ideal species accumulation curve
#' @param save.pdf TRUE/FALSE- should plots be displayed or saved to a pdf?
#' @param lakename optional character string for adding lake name to pdf output
#' @param datename character string name of b_data field containing date
#' @param dateformat character string: posix format for datename column
#'
#' @export accum
#'
#' @return a two panel plot with trends in richness on top, and cumulative richness vs. simulated
#'         accumulation curve on bottom
#'
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#' head(lakegeneva)
#'
#' accum(b_data=lakegeneva,column='biovol_um3_ml',n=10,save.pdf=FALSE)

accum = function(b_data, phyto_name='phyto_name',column=NA, n=100, save.pdf=FALSE,lakename='',
                 datename='date_dd_mm_yy',dateformat='%d-%m-%y') {

	b_data[[datename]]=as.character(b_data[[datename]])

  b_data$date_dd_mm_yy = as.POSIXct(b_data[[datename]],format=dateformat)
  graphics::par(mfcol=c(2,1))
  graphics::par(mar=c(3,4,1,1))
  if(is.na(column))
  {
    b_data$presence=1
    column=grep('presence',names(b_data))
  }
  b_data = subset(b_data, b_data[, column] > 0)

  ##assigning phytoplankton id column to phyto_name
  b_data[[phyto_name]]=as.character(b_data[[phyto_name]])

  ntaxa0 = as.data.frame(table(b_data$date_dd_mm_yy, b_data[[phyto_name]]))

  ntaxa0 = subset(ntaxa0, ntaxa0$Freq > 0)

  ntaxa = as.data.frame(table(ntaxa0$Var1))
  ntaxa$date_dd_mm_yy = as.POSIXct(as.character(ntaxa$Var1),format='%Y-%m-%d')

  if(save.pdf)
  {
    grDevices::pdf(paste('phytoSAC',lakename,'.pdf',sep=''))
    graphics::par(mfcol=c(2,1))
    graphics::par(mar=c(3,4,1,1))
  }
  graphics::plot(ntaxa$Freq ~ ntaxa$date_dd_mm_yy, pch = 19, ylab = 'number of taxa', xlab = '')

  sbio = stats::aggregate(b_data[, column] ~ b_data[[phyto_name]] + b_data[[datename]],
                         FUN=sum)

  abundance.var=ifelse(is.numeric(column),names(b_data)[column],column)
  names(sbio)=c('phyto_name','date_dd_mm_yy',abundance.var)

  sbio$date_dd_mm_yy=as.POSIXct(sbio$date_dd_mm_yy,format='%Y-%m-%d')

  rich = as.data.frame(table(sbio$date_dd_mm_yy),stringsAsFactors = F)
  rich$Var1=as.POSIXct(rich$Var1,format='%Y-%m-%d')

  rich=rich[match(unique(sbio$date_dd_mm_yy),rich$Var1),]
  first = stats::aggregate(sbio$date_dd_mm_yy ~ sbio$phyto_name, data = sbio, min)
  names(first)=c('phyto_name','date_dd_mm_yy')

  first$n = 0


  num.taxa=length(rich$Var1)
  freq=rich$Freq

  row.end=cumsum(rep(sum(freq),n))

  row.start=1
  for(i in 2:n)
  {
    row.start[i]=row.end[i-1]+1
  }

  simul.length=n * sum(freq)


  nruns = data.frame (phyto_name=vector(length=simul.length),date_dd_mm_yy = vector(length=simul.length),n=vector(length=simul.length))

  for (j in 1:n){
    tt = data.frame( phyto_name = NULL,date_dd_mm_yy = NULL)
    for (i in 1:length(rich$Var1)) { #for each date
      x1 = subset(sbio, sbio$date_dd_mm_yy == unique(sbio$date_dd_mm_yy)[i])
      #take aggregate data from that date

      ttt =  data.frame(phyto_name = sample(first$phyto_name, as.numeric(table(x1$date_dd_mm_yy)), replace = F), date_dd_mm_yy = unique(sbio$date_dd_mm_yy)[i],stringsAsFactors = F)
      #sample randomly from list of all taxa seen, draw number of unique taxa seen that day

      tt = rbind(tt, ttt)
      #repeat for each date
    }
    tt$n=j
    nruns[row.start[j]:row.end[j],] = tt
  }
  nruns$date_dd_mm_yy = as.POSIXct(nruns$date_dd_mm_yy, origin = '1970-01-01')
  nruns$n=rep(1:n,each=sum(freq))
  mue = rbind(first, nruns)

  mue = mue[order(mue$date_dd_mm_yy),]

  graphics::plot(c(1:dim(subset(mue, n == 0))[1]) ~ date_dd_mm_yy, data = subset(mue, n == 0), ylab  = 'cum number of taxa', xlab = '')
  mue = subset(mue, mue$date_dd_mm_yy > 0)

  for(i in 1:n){
    xx = subset(mue, mue$n == i)
    xx$date_dd_mm_yy=as.POSIXct(xx$date_dd_mm_yy,origin = '1970-01-01')

    xxx = stats::aggregate(date_dd_mm_yy ~ phyto_name, data = xx, min)
    xxx = xxx[order(xxx$date_dd_mm_yy),]

    graphics::lines(c(1:dim(xxx)[1]) ~ date_dd_mm_yy, data = xxx, type = 'l', col = 'grey')
  }

  if(save.pdf)
  {
    grDevices::dev.off()
  }

  graphics::par(mfcol=c(1,1))
  graphics::par(mar=c(5.1, 4.1 ,4.1, 2.1))

}
