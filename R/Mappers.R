getMap <- function(model = 'SimpleModel'){
  if(model == 'SimpleModel'){
    map <- function(x){1/(1+exp(-(x-0)))} #logistic function with 0 intercept
  } else{
    map <- function(x){return(x)}
  }
  return(map)
}
