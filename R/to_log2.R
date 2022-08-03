#' Change Values to Log-2 Fold Change
#'
#' The default behavior of Presto 
#' (https://www.rdocumentation.org/packages/RPresto/versions/1.3.7) is to report 
#' the natural log of the expression change plus one (adding one before taking 
#' the natural log allows for processing of zeros, yielding zero instead
#' of 'undefined'). Many cell biology disciplines use log-2 fold changes 
#' instead of natural logs; due to this, communication of results would be 
#' optimized by transforming the natural log-fold changes in Presto to log-2 fold
#' changes. to_log2() will take the natural log-fold changes from Presto and
#' convert them to a log-2 fold format. 
#'
#' @param value Log-fold change values in the "LogFC" column of the default 
#' Presto output
#'
#' @return Returns Presto values in a log-2 fold change format.
to_log2 <- 
  function(value){
    log2(exp(value))
  }
