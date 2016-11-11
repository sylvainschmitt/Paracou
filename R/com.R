#' Community
#'
#' Build a community from the tree and the birth and death tables
#'
#' @return community table
#' @export
#'
#' @examples
#'
com <- function(tree, bd, y, l){

  com <- apply(bd, 1, function(v){
    b <- v[9]
    d <- v[7]
    if(is.na(d)){
      d <- Inf
    }

    if(y >= b & b > l & y >= d){com <- 'DR'}
    if(y >= b & b > l & y < d){com <- 'AR'}
    if(y >= b & b <= l & y >= d){com <- 'DS'}
    if(y >= b & b <= l & y < d){com <- 'AS'}
    if(y < b){com <- NA}

    return(com)
  })
  names(com) <- row.names(bd)

  return(com)
}
