% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Items2R.R, R/est2R.R
\name{items2R}
\alias{items2R}
\title{MIRT items (.PAR) file to an R data.frame.}
\usage{
items2R(run, model = c("1PL", "2PL", "M2PL"), method = c("EAP", "WML",
  "ML"))

items2R(run, model = c("1PL", "2PL", "M2PL"), method = c("EAP", "WML",
  "ML"))
}
\arguments{
\item{run}{Name of MIRT run, without any extention. Assumes working dir is set correctly.}

\item{model}{Type of model, MIRT will run up to 3 estimations - Rasch (1PL/PCM), Birnbaum/Lord (2PL/GPCM) and a multidimensional version of each.
Leaving model blank will return all available sets of parameter estimates in a list, specifying a model will return that specific set of parameters.}

\item{run}{Name of MIRT run, without any extention. Assumes working dir is set correctly.}

\item{model}{Type of model, MIRT will run up to 3 estimations - Rasch (1PL/PCM), Birnbaum/Lord (2PL/GPCM) and a multidimensional version of each.
Leaving model blank will return all available sets of parameter estimates in a list, specifying a model will return that specific set of parameters.}
}
\value{
(list of) list with itembank parameters, a, b and m, where applicable. (m is the number of cats in an item, a and b are KxQ and KxM matrices, where M is max(m))

(list of) list with itembank parameters, a, b and m, where applicable. (m is the number of cats in an item, a and b are KxQ and KxM matrices, where M is max(m))
}
\description{
MIRT items (.PAR) file to an R data.frame.

MIRT items (.PAR) file to an R data.frame.
}

