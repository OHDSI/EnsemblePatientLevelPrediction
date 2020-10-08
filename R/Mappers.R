getMap <- function(model = 'SimpleModel'){
  if(model == 'SimpleModel'){
    map <- function(x){1/(1+exp(-(x-0)))} #logistic function with 0 intercept
  } else{
    mapLoc <- system.file("settings", paste0(model,'_finalMap.rds' ), package = "SkeletonExistingPredictionModelStudy")
    map <- readRDS(mapLoc)
  }
  return(map)
}
