#' MIRT items (.PAR) file to an R data.frame.
#' @param run Name of MIRT run, without any extention. Assumes working dir is set correctly.
#' @param model Type of model, MIRT will run up to 3 estimations - Rasch (1PL/PCM), Birnbaum/Lord (2PL/GPCM) and a multidimensional version of each.
#'              Leaving model blank will return all available sets of parameter estimates in a list, specifying a model will return that specific set of parameters.
#' @return (list of) list with itembank parameters, a, b and m, where applicable. (m is the number of cats in an item, a and b are KxQ and KxM matrices, where M is max(m))
items2R <- function(run, model = NULL){
  if (is.null(model)) model  <- c('1PL','2PL','multi')
  res <- list()
  f <- paste0(run,".MIR")
  xt<-readLines(file(f))
  
  if('1PL' %in% model){
    # Determine which part of the data is Rasch
    startR<- which(grepl("MML-PARAMETER ESTIMATION RASCH-TYPE-MODEL",xt))+4 # hard-code string to look for (!! this may change in MIRT versions? !!) 
    eindeR<- which(grepl(" --------------------------------------------------------------------------------",xt))-1 # end of estimates.
    eindeR<- eindeR[eindeR>startR] # first end after start of Rasch is end of Rasch
    rowsR<- eindeR[1]-startR
    
    tryCatch({ # it could always break, be graceful about this.     
      # get data an name it
      par<-read.table(f,skip=startR,fill=T,nrow=rowsR,row.names=NULL,header=F,stringsAsFactors=F,flush=T)
      names(par) <- c('ID','label','par','cat','est','SE')
      
      # fetch important numbers
      Q <- 1 # dims
      M <- max(par$cat[par$par=='B']) # max cats
      K <- max(par$ID)                 # items
      
      # set up output
      a <- matrix(1,K,Q)
      b <- matrix(NA,K,M)
      
      # fill beta matrix
      par.b <- par[par$par=='B',]
      for (i in 1:nrow(par.b)){
        row <- par.b[i,]
        b[row$ID,row$cat] <- row$est 
      } 
      
      # create m vector
      m <- as.integer(by(par.b$cat,par.b$ID,max,FALSE))
      
      # attach item names
      names <- unique(par$label)
      rownames(a) <- rownames(b) <- names
      
      # return stuff
      res[["1PL"]] <- list(a=a,b=b,m=m)
    }, error = function(e) cat('1PL failed. Are you sure there\'s data here? \n'), finally = NULL)
  }
  
  if("2PL" %in% model){
    startL<- which(grepl("MML-PARAMETER ESTIMATION LORD-TYPE-MODEL",xt))+4
    eindeL<- which(grepl(" --------------------------------------------------------------------------------",xt))-1
    eindeL<- eindeL[eindeL>startL]
    rowsL<- eindeL[1]-startL
    
    tryCatch({
      par<-read.table(f,skip=startL,fill=T,header=F,nrow=rowsL, row.names=NULL,stringsAsFactors=F,flush=T)
      names(par) <- c('ID','label','par','cat','est','SE','t_est','t_SE')
      
      # fetch important numbers
      Q <- 1 # dims
      M <- max(par$cat[par$par=='B']) # max cats
      K <- max(par$ID)                 # items
      
      # set up output
      a <- matrix(0,K,Q)
      b <- matrix(NA,K,M)
      
      # fill alpha matrix
      par.a <- par[par$par=='A',]
      for (i in 1:nrow(par.a)){
        row <- par.a[i,]
        a[row$ID,1] <- row$est # output file gives 0 for dimension.
      } 
      
      # fill beta matrix
      par.b <- par[par$par=='B',]
      for (i in 1:nrow(par.b)){
        row <- par.b[i,]
        b[row$ID,row$cat] <- row$est 
      } 
      
      # create m vector
      m <- as.integer(by(par.b$cat,par.b$ID,max,FALSE))
      
      # attach item names
      names <- unique(par$label)
      rownames(a) <- rownames(b) <- names
      
      # return stuff
      res[["2PL"]] <- list(a=a,b=b,m=m)
      
    }, error = function(e) cat('2PL failed. Are you sure there\'s data here? \n'), finally = NULL)
  }
  
  if("multi" %in% model){
    startM<- which(grepl("MML-PARAMETER ESTIMATION MULTIDIMENSIONAL IRT LORD-TYPE",xt))+5
    eindeM<- which(grepl(" --------------------------------------------------------------------------------",xt))-1
    eindeM<- eindeM[eindeM>startM]
    rowsM<- eindeM[1]-startM
    
    tryCatch({
    par<-read.table(f,skip=startM,fill=T,header=F,nrow=rowsM, row.names=NULL,stringsAsFactors=F,flush=T)
    names(par) <- c('ID','label','par','cat','est','SE','t_est','t_SE')
    
    # fetch important numbers
    Q <- max(par$cat[par$par=='A']) # dims
    M <- max(par$cat[par$par=='B']) # max cats
    K <- max(par$ID)                 # items
    
    # set up output
    a <- matrix(0,K,Q)
    b <- matrix(NA,K,M)
    
    # fill alpha matrix
    par.a <- par[par$par=='A',]
    for (i in 1:nrow(par.a)){
      row <- par.a[i,]
      a[row$ID,row$cat] <- row$est 
    } 
    
    # fill beta matrix
    par.b <- par[par$par=='B',]
    for (i in 1:nrow(par.b)){
      row <- par.b[i,]
      b[row$ID,row$cat] <- row$est 
    } 
    
    # create m vector
    m <- as.integer(by(par.b$cat,par.b$ID,max,FALSE))
    
    # attach item names
    names <- unique(par$label)
    rownames(a) <- rownames(b) <- names
    
    # return stuff
    res[["multi"]] <- list(a=a,b=b,m=m)
    
    }, error = function(e) cat('Multi failed. Are you sure there\'s data here? \n'), finally = NULL)
  }
  if (length(res) == 1) res <- res[[1]]
  return(res)
}