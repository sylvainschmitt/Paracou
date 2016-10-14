#' @importFrom parallel parSapply
NULL

#' Birth and death table
#'
#' Generates birth and death table
#'
#' @return birth and death table
#' @export
#'
#' @examples
#'
bd_table <- function(tree){

  # Getting birth and death
  ID <- sort(unique(tree$id))
  cl <- makeCluster((detectCores()-1))
  clusterExport(cl, list('ID', 'tree'))
  bd <- parSapply(cl, ID, function(id){
    years <- sort(subset(tree, id == id)$census)
    return(c(years[1], tail(years,1)+1))
  })
  stopCluster(cl)
  rm(cl)

  # Building result table
  bd <- data.frame(t(bd))
  names(bd) <- c('birth', 'death')
  row.names(bd) <- ID
  bd <- cbind(bd, tree[match(ID, tree$id), c('treatment', 'plot', 'vern', 'genus', 'species')])
  bd$SpCode <- paste(bd$genus, bd$species)
  bd$SpCode[grep('Indet', bd$genus)] <- bd$vern[grep('Indet', bd$genus)]
  bd$death[bd$death == 2016] <- NA

  return(bd)
}
