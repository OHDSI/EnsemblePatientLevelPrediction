addExtras <- function(fname, list, cohortDatabaseSchema, cohortTable){
  if(fname%in%c('createCohortCovariateSettings', 'measurementCohortCovariateSettings')){
    list$cohortDatabaseSchema <- cohortDatabaseSchema
    list$cohortTable <- cohortTable
  }
  return(list)
}

evalFunction <- function(list){
  ind <- names(list)%in%c('scaleMap')
  
  if(sum(ind)>0){
    for( i in which(ind)){
      list[[i]] <- eval(str2lang(paste0(list[[i]], collapse = ' ')))
    }
  }
  return(list)
}

processModelJson <- function(json, cohortDatabaseSchema, cohortTable){

  covariateSettings <- list()
  length(covariateSettings) <- sum(unlist(lapply(1:length(json$covariateSettings), function(i) ifelse(class(json$covariateSettings[[i]][[1]])!='list', 1, length(json$covariateSettings[[i]])))))
  k <- 1
  for (i in 1:length(json$covariateSettings)) {
    fname <- names(json$covariateSettings)[i]
    if(class(json$covariateSettings[[i]][[1]])=='list'){
      for(j in 1:length(json$covariateSettings[[i]])){
        json$covariateSettings[[i]][[j]] <- addExtras(fname, json$covariateSettings[[i]][[j]], cohortDatabaseSchema, cohortTable)
        json$covariateSettings[[i]][[j]] <- evalFunction(json$covariateSettings[[i]][[j]])
        covariateSettings[[k]] <- do.call(get(fname), 
                                          json$covariateSettings[[i]][[j]])
        k <- k + 1
      }
    } else {
      json$covariateSettings[[i]] <- addExtras(fname, json$covariateSettings[[i]], cohortDatabaseSchema, cohortTable)
      json$covariateSettings[[i]] <- evalFunction(json$covariateSettings[[i]])
      covariateSettings[[k]] <- do.call(get(fname), 
                                        json$covariateSettings[[i]])
      k <- k + 1
    }
    
  }
  
  result <- list(details = json$details,
                 model = json$model,
                 covariateSettings = covariateSettings,
                 cohorts = json$cohorts)
  
  return(result)
}