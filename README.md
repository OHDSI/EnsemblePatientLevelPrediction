EnsemblePatientLevelPrediction
========================================================

[![Build Status](https://github.com/OHDSI/EnsemblePatientLevelPrediction/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/EnsemblePatientLevelPrediction/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/EnsemblePatientLevelPrediction/coverage.svg?branch=main)](https://codecov.io/github/OHDSI/EnsemblePatientLevelPrediction?branch=main)

EnsemblePatientLevelPrediction is part of [HADES](https://ohdsi.github.io/Hades).

Introduction
============

EnsemblePatientLevelPrediction is an R package for building and validating ensemble patient-level predictive models using data in the OMOP Common Data Model format.  The package expands the OHDSI R PatientLevelPrediction package to enable ensemble learning.

In our study [here](https://www.researchsquare.com/article/rs-1217376/v1) we found that combining models developed using different databases resulted in models that had better discrimination performance compared to the level 1 models (single database) when transported to new data.  However, calibration was poor.  This has prompted the EnsemblePatientLevelPrediction package where users can combine models developed on the same database or models developed on different databases.


User Documentation
==================

Vignette: [EnsemblePatientLevelPrediction](https://raw.githubusercontent.com/OHDSI/EnsemblePatientLevelPrediction/main/inst/doc/BuildingEnsembleModels.pdf)

Website: Documentation can be found on the [package website](https://ohdsi.github.io/EnsemblePatientLevelPrediction).

Package manual: [EnsemblePatientLevelPrediction.pdf](https://github.com/OHDSI/EnsemblePatientLevelPrediction/blob/master/extras/EnsemblePatientLevelPrediction.pdf)


Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/EnsemblePatientLevelPrediction/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package. 
 
License
=======
EnsemblePatientLevelPrediction is licensed under Apache License 2.0

Development
===========
EnsemblePatientLevelPrediction is being developed in R Studio.

Beta

# Acknowledgements

- The package is maintained by Jenna Reps
