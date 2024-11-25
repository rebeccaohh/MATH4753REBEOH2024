#' myread
#'
#' @param csv excel file
#'
#' @return csv file that can be read in
#' @export
#'
#' @examples
#' \dontrun{fin.df=myread("FINTUBES.csv")}
#'
#' @importFrom utils read.table
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
