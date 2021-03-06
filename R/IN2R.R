#' Fetch parameters from CAT-ROMA file
#' 
#' @param f NULL, file location, or chooser if left NULL.
#' @return MCAT_itembank
#' @import stringr
#' @export
IN2R <- function(f = NULL) {
  if (is.null(f)) f <- file.choose()
  require(stringr)
  
  lines <- readLines(file(f))
  x <- strsplit(str_trim(lines),"\\s+")
  
  # first 5 elements on second line
  N <- as.integer(x[[2]][1])
  M <- as.integer(x[[2]][2])
  Q <- as.integer(x[[2]][3])
  K <- as.integer(x[[2]][4])
  test.length <- as.integer(x[[2]][5])
  
  covar <- matrix(NA,Q,Q)
  mean <- numeric(Q)
  
  # lines 3-(3+Q) are variance covariance matrix and mean vector
  for (i in 3:(Q+2)){
    q <- i-2
    line <- as.numeric(x[[i]])
    mean[q] <- line[1]
    covar[q,] <- line[2:(Q+1)]
  }
  
  a <- matrix(NA,K,Q)
  b <- matrix(NA,K,M)
  m <- integer(K)
  
  start <- 3 + 2*Q # offset for mean/covariance + starting items.
  end <- length(x)
  
  # keep track of item number
  k <- -1
  
  for (i in start:end){
    line <- as.numeric(x[[i]])
    
    if (line[1] > k){ # this is a new item
      k <- line[1]
      m[k] <- line[2]
      a[k,] <- line[3:(2+Q)]
    } else { # this is a category
      b[k,line[2]] <- line[3]
    }
  }
  
  out <- list(alpha=a,beta=b,guessing=matrix(0,K,1),mean=mean,covar=covar,m=m,N=N,M=M,Q=Q,K=K)
  return(invisible(out))
}