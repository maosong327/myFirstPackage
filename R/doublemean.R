#' My doublemean function.
#'
#' My doublemean returns a numeric or a string "It's significant".
#'
#' @param x A numeric vector.
#'
#' @return A numeric or a string "It's significant"
#' @export
#'
#' @examples
#' doublemean(1:6)

doublemean <- function(x){
  n <- 1:3
  z <- stats::ks.test(x, n)
  if (z$p.value > 0.05){
    y <- mean(x) * 2
    return(y)
  }else{
    print("It's significant")
  }
}

