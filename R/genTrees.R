#' @importFrom data.table data.table
NULL

#' Trees data generation
#'
#' Generates Trees data set from external data extracted from the database
#'
#' @return Trees data set
#' @export
#'
#' @examples
#' summary(genTrees())
#'
genTrees <- function(){

  # Opening trees
  path <- system.file('extdata', 'paracou_p1_15.csv', package = 'Paracou')
  tree <- read.table(path, header = T, sep = ",", dec = ".")

  # Data overview
  summary(tree)
  length(unique(tree$i_arbre[which(tree$espece == 'Indet,')])) / length(unique(tree$i_arbre)) * 100 # Indet percentage

  # Correction
  tree$dbh <- tree$circonf/pi
  tree$status <- as.numeric(tree$code_vivant=="VRAI")
  tree <- data.table(tree)

  detect <- function(X, tm) {
    C<-"no"
    if(length(X)>1){
      # cresc : annual diameter increment
      cresc <- diff(X)/diff(tm)
      cresc[is.na(cresc)] <- 0
      if(max(cresc, na.rm=T)>7.5) {
        C<-"excessive growth"
      } else if (min(cresc*diff(tm), na.rm=T)<(-2)) {
        C<-"excessive decrease"
      } }
    return(C)
  }

  correc <- tree[,.(detect(dbh, campagne)), by=.((i_arbre)) ]
  table(correc$V1)
  correc <- correc[correc$V1!="no",]

  path <- system.file('scripts', 'dbh_correction.R', package = 'Paracou')
  source(path)
  corrected_dbh <- tree[,.(correction(dbh, campagne, status, code_mesure)), by=.(i_arbre) ]
  tree$dbh_c <- corrected_dbh$V1

  tree <- data.frame(tree)

  return(tree)
}
