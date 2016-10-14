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
    b <- v[1]
    d <- v[2]
    if(is.na(d)){
      d <- Inf
    }

    if(y > b & y >= d & d <= l){com <- 'DS'}
    if(y > b & y >= d & d > l){com <- 'DR'}
    if(y > b & y < d & b <= l){com <- 'OS'}
    if(y > b & y < d & b > l){com <- 'NS'}
    if(y == b & b > l){com <- 'NR'}
    if(y == b & b <= l){com <- 'OS'}
    if(y < b){com <- NA}

    return(com)
  })
  names(com) <- row.names(bd)

  return(com)
}
