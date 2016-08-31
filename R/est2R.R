#' MIRT items (.PAR) file to an R data.frame.
#' @param run Name of MIRT run, without any extention. Assumes working dir is set correctly.
#' @param model Type of model, MIRT will run up to 3 estimations - Rasch (1PL/PCM), Birnbaum/Lord (2PL/GPCM) and a multidimensional version of each.
#'              Leaving model blank will return all available sets of parameter estimates in a list, specifying a model will return that specific set of parameters.
#' @return (list of) list with itembank parameters, a, b and m, where applicable. (m is the number of cats in an item, a and b are KxQ and KxM matrices, where M is max(m))
items2R <- function(run, model = c("1PL", "2PL", "M2PL"), method = c("EAP", "WML", "ML")){
  res <- list()
  for (mod in model) {
    res[[mod]] <- list()
    
    ### Get results from MIRT output. Obviously the different methods write output in diferent ways.
    if ("EAP" %in% method) {
        
    }
  }  
}