#' Visually assess change in sampling effort over time (author: Dietmar Straile)
#'
#' @param b_data Name of data.frame object
#' @param column column name or number for field containing abundance (biomass,biovol, etc.)
#'               can be NA for presence absence
#' @param save.pdf TRUE/FALSE Should the output plot be saved to a file? defaults to FALSE
#' @param lakename Character string for labeling output plot
#' @param datecolumn Character String or number specifying dataframe field with date information 
#' @param dateformat Character string specifying POSIX data format
#' 
#' @export sampeff
#' 
#' @return a time-series plot of minimum relative abundance over time. This should change 
#'         systematically with counting effort.
#' 
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#' 
#' sampeff(lakegeneva,column=6) #column 6 contains biovolume

sampeff = function(b_data,column,save.pdf=F,lakename='',datecolumn='date_dd_mm_yy',dateformat='%d-%m-%y') {
  abundance=b_data[[column]]
  b_data$abundance=abundance
  
  if(is.numeric(datecolumn)){datecolumn=names(b_data)[datecolumn]} #convert from number to name
  
  b_data[[datecolumn]]=as.POSIXct(b_data[[datecolumn]],format=dateformat)
  
  b_data = subset(b_data, abundance > 0)  
  
  sbio = stats::aggregate(stats::formula(paste('abundance ~',datecolumn)) , data = b_data, sum)
  names(sbio)[2] = 'sabundance'
  sbio2 = merge(b_data, sbio, by = datecolumn, all = T)
  sbio2$relabundance = sbio2$abundance /sbio2$sabundance
  minbio = stats::aggregate(stats::formula(paste('relabundance ~',datecolumn)), data = sbio2, min)

  if(save.pdf){grDevices::pdf(paste('sampeff',lakename,'.pdf',sep=''))}
  
  graphics::plot(I(100 * relabundance) ~ date_dd_mm_yy, data = minbio, log = 'y', xlab = '', ylab = 'Minimum contribution [%]')
  
  if(save.pdf){grDevices::dev.off()}
}