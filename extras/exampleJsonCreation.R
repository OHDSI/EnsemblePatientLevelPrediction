# example json creation:

createdBy <- 'Jenna Reps'
organizationName <- 'OHDSI'
outputLocation <- 'D:/existingModel'

baseUrl <- Sys.getenv('baseUrl')

settings <- data.frame(targetCohortId = c(1373,1455),
                       targetCohortName = c('Combination of major surgery inpatient',
                                             'Combination of major surgery inpatient with creatinine'),
                       outcomeId = c(1456),
                       outcomeName = c('Earliest of AMI cardiac arrest or death'))


populationSetting <- PatientLevelPrediction::createStudyPopulationSettings(binary = T, 
                                                                           includeAllOutcomes = F, 
                                                                           firstExposureOnly = T, 
                                                                           washoutPeriod = 365, 
                                                                           removeSubjectsWithPriorOutcome = T, 
                                                                           priorOutcomeLookback = 9999, 
                                                                           requireTimeAtRisk = T, 
                                                                           minTimeAtRisk = 1, 
                                                                           riskWindowStart = 1, 
                                                                           startAnchor = 'cohort start', 
                                                                           endAnchor = 'cohort start', 
                                                                           riskWindowEnd = 90, 
                                                                           verbosity = 'INFO')

modelList <- list()
#length(modelList) <- 2
modelList[[1]] <- createModelJson(modelname = 'rcri_with_creatinine', 
                                  modelFunction = 'glm',
                                  standardCovariates = NULL,
                                  cohortCovariateSettings = list(atlasCovariateIds = c(1374,1293,1292,1287,
                                                                                       1291,1290),
                                                                 atlasCovariateNames = c('High risk surgery', 
                                                                                         'Ischemic heart disease',
                                                                                         'Heart failure',
                                                                                         'Cerebrovascular disease',
                                                                                         'Insulin treatment',
                                                                                         'Elevated creatinine'),
                                                                 analysisIds = c(456,456,456,456,456,456),
                                                                 startDays = c(0,-9999,-9999,-9999,-60,-30),
                                                                 endDays = c(0,-1,-1,-1,0,0),
                                                                 points = c(1,1,1,1,1,1),
                                                                 count = rep(F, 6),
                                                                 ageInteraction = rep(F, 6),
                                                                 lnAgeInteraction = rep(F, 6)
                                  ),
                                  
                                  measurementCovariateSettings = NULL, 
                                  measurementCohortCovariateSettings = NULL, 
                                  ageCovariateSettings = NULL,
                                  
                                  finalMapping = 'function(x){xTemp = x; xTemp[x==0] <- 0.039; xTemp[x==1] <- 0.06;xTemp[x==2] <- 0.101;xTemp[x>=3] <- 0.15;return(xTemp) }',
                                  predictionType = 'survival'
)


jsonSet <- createStudyJson(packageName = 'exampleStudy',
                packageDescription = 'an example of the skeleton',
                createdBy = createdBy ,
                organizationName = organizationName,
                settings = settings,
                baseUrl = baseUrl,
                populationSetting = populationSetting ,
                modelList = modelList,
                outputLocation = outputLocation)


jsonSet <- Hydra::loadSpecifications(file.path(outputLocation,'existingModelSettings.json'))
Hydra::hydrate(specifications = jsonSet, 
               outputFolder = 'D:/test/study')

