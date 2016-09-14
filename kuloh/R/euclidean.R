#' Euclidean Algorithm
#' 
#' @description An algorithm to find the greatest common divisor of two numbers.
#' 
#' 
#' @param a is numeric or integer of length 1
#' @param b is numeric or integer of length 1
#' @return The greatest commom divisor betwen a and b
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
#' @export
euclidian <- function(a, b) {
  if((!is.numeric(a) | length(a)!=1) | (!is.numeric(b) | length(b)!=1)){
    stop("Insert a numeric value of lenght 1")
  } else {
    if(b > a){
      temp_a <-a
      temp_b <- b
      a <- temp_b
      b <- temp_a
    }
    r_1 <- a;
    r_2 <- b;
    while(r_2 != 0) {
      r   <- r_1 %% r_2
      r_1 <- r_2
      r_2 <- r
    }
    return(r_1)
  }
}
