#' @title Induce Multicollinearity
#' @description The function induces multicollinearity into a given numeric matrix. It collinearises a matrix, that is. It does this using the specified correlation coefficient or correlation matrix.
#' @name collinearise
#' @param x The numeric the matrix onto which collinearity is to be induced
#' @param r The correlation coefficient
#' @details The absolute value of the correlation coefficient, r, must always be less than or equal to 1.
#'
#' The matrix or data frame x must contain only numeric values
#' @return Returns a dataframe with a desired correlation. This paves way for studies on the effects of different levels of multicollinearity on statistical models or techniques.
#' @export
#' @example /man/example
#' @references Chang X, Paige CC. New perturbation analyses for the Cholesky factorization. IMA J Numer Anal. 1996;16(4):457-484
#' @author Chibuike Ngene Nnamani <cnnnamani2@live.utm.my>
#'
#' Dr. Norhaiza Ahmad <norhaiza@utm.my>
#'
#'
collinearise <- function(x, r = 0.50){
  for (i in r) {
    if (abs(i) > 1)
      stop(message = "No value of a correlation coefficient should be greater than 1 in absolute value")
  }
  if (length(dim(x)) != 2)
    stop(message = "Please provide a conformable numeric matrix for collinearity inducement")
    if (length(dim(r)) > 2) stop(message = "The correlation matrix should not have more than  two dimensions")
  else if (length(dim(r)) < 2) {
    if (abs(r) > 1)
      stop(message = "The absolute value of a correlation coefficient cannot be greater than 1")
    r <- matrix(r, nrow = dim(x)[2], ncol = dim(x)[2])
    for (i in 1:dim(x)[2]){
      r[i,i] <- 1
    }
  }
  if (dim(x)[2] != dim(r)[2]) stop(message = "The number of columns of the matrix must be the same as the size of the correlation matrix")
  x <- t(t(chol(r))%*%t(x))
  #dimnames(x) <- list(paste0(("Row", 1:dim(x)[1]),paste0("Col", 1:dim(x)[2]))
  return(x)
}
