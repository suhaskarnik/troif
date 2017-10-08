require("dplyr")

#' Executes Aggregate functions on the previous N records, similar to SQL Window functions
#'
#' @param vec vector to aggregate 
#' @param by number of previous records to aggregate
#' @param aggfun the aggregate function. max, min, mean are all possible values
#' @param ignoreNA optional boolean. True by default. Set to FALSE if you do NOT want results when the window is less than "by" at the edges of your vector
#' @param includeCurrent optional boolean, FALSE by default. Set to TRUE if you want to include the current row in the calculations
#' @return a vector containing the lagged aggregate
#'
#' @examples
#' y = c(5,7,2,9,3,8,1,7,4)
#' aggPrev(y,5,mean)
#' @export
aggPrev = function(vec,by=1,aggfun,ignoreNA=TRUE,includeCurrent=FALSE){
  rollAgg(vec,by=by,aggfun,ignoreNA=TRUE,includeCurrent,roller=dplyr::lag)
}

#' Executes Aggregate functions on the next N records, similar to SQL Window functions
#'
#' @param vec vector to aggregate 
#' @param by number of next records to aggregate
#' @param aggfun the aggregate function. max, min, mean are all possible values
#' @param ignoreNA optional boolean. True by default. Set to FALSE if you do NOT want results when the window is less than "by" at the edges of your vector
#' @param includeCurrent optional boolean, FALSE by default. Set to TRUE if you want to include the current row in the calculations
#' @return a vector containing the leading aggregate
#'
#' @examples
#' y = c(5,7,2,9,3,8,1,7,4)
#' aggNext(y,5,mean)
#' @export
aggNext = function(vec,by=1,aggfun,ignoreNA=TRUE,includeCurrent=FALSE){
  rollAgg(vec,by=by,aggfun,ignoreNA=TRUE,roller=dplyr::lead,includeCurrent)
}


#' Generic function to perform leading or lagged Aggregate functions
#'
#' @param vec vector to aggregate 
#' @param by number of previous records to aggregate
#' @param aggfun the aggregate function. max, min, mean are all possible values
#' @param ignoreNA optional boolean. True by default. Set to FALSE if you do NOT want results when the window is less than "by" at the edges of your vector
#' @param roller A function that produces the lagged or leading value. Set to dplyr::lag by default
#' @param includeCurrent optional boolean, FALSE by default. Set to TRUE if you want to include the current row in the calculations
#' @return a vector containing the lagged aggregate
#'
#' @examples
#' y = c(5,7,2,9,3,8,1,7,4)
#' aggPrev(y,5,mean)
#' @export
rollAgg = function(vec,by=1,aggfun,ignoreNA=TRUE, roller=dplyr::lag,includeCurrent=FALSE){
  start = ifelse(includeCurrent,0,1)

  roll=sapply(start:by, function(k) roller(vec,k)) 
  
  apply(roll,
        1, # go row-wise  
        function(r){
          ifelse(ignoreNA,aggfun(r[!is.na(r)]) ,aggfun(r))
        } 
  )
}