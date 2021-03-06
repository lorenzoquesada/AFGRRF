# AFGRRF CODE

Area and Feature Guided Regularized Random Forest (AFGRRF)  is a R toolbox to select the best features that miminize the area while maximizing the overall accuracy.
L. C. Quesada Ruiz, V. Rodríguez Galiano, R. Zurita-Milla and E. Izquierdo-Verdiguier (Copyright (c) 2021).
The R toolbox has been developed by Lorenzo C. Quesada Ruiz and Víctor Rodríguez Galiano based on the code developed and used in Izquierdo-Verdiguier, E., and Zurita-Milla, R. (2020). "An evaluation of Guided Regularized Random Forest for classification and regression tasks in remote sensing". International Journal of Applied Earth Observation and Geoinformation, 88, 102051.


**Project**: Teledetección y clima: análisis de series temporales para la evaluación de la vulnerabilidad forestal al cambio climático y la estimación de cosechas en Andalucía -TelClim- (Ref: US-1262552)

Code associate to the manuscript "AREA AND FEATURE GUIDED REGULARISED RANDOM FOREST: A NOVEL METHOD FOR PREDICTIVE MODELLING OF BINARY PHENOMENA"

**Authors**: Lorenzo Carlos Quesada-Ruiz*, Victor Rodriguez-Galiano, Raúl Zurita-Milla, Emma Izquierdo-Verdiguier

**Script first purpose**: New machine learning feature selection method applied to the mapping of binary geographic  phenomenon (occurrence vs absence).                         
**Script second purpose**:Selecting a feature subset that optimise both the overall accuracy and the potentially occurrence area, using the Success Rate                           

**Run time**: the script may run daily  or may be used when needed

**Authors code**: Lorenzo C. Quesada Ruiz, Víctor Rodríguez Galiano, Raúl Zurita-Milla, Emma Izquierdo-Verdiguier  

**Organization**: Departamento de Geografía física y Análisis Geográfico Regional de la Unviersidad de Sevilla/Faculty of Geo-Information Science and Earth Observation (ITC) of the University of Twente

**Address**: Doña María de Padilla, 40002, Seville, Spain

**Project Contact**: Víctor Rodríguez Galiano

**Email**: lquesada@us.es

**Phone number**: (+34) 6303035751366


**Script filename**: AFGRRF.R

**Version Date**: November, 2020



**I N S T R U C T I O N S**

This script consists of several parts and sub-parts.

**Part 0 **   Load the neccesaries r packages. 

**Part 1**    is the setting section, in which the User should replace the marked values (e.g. directory paths)

**Part 1.1**  defines environmental variables, such as raw data. The user should modify these values.

**Part 1.2**  Inclusion of gamma and lambda values

**Part 1.3**  Arrays placeholder to keep feature selection results from the different combinations of gamma and lambda values

**Part 2**    Obtain the Random Forest embedded importance

**Part 3**    Guide Grid search regularisation (Initialization loop). 

**a)** Initialize an empty subset of selected features and a threshold gain (G^*=0)

**b)** Fix the values of λ and γ to calculate α.

**c)** 	Computation of G_GRRF

**d)** If G_GRRF (Xj,ν)> G^*  the feature j is selected, and the threshold gain is updated to the GRRF gain. Otherwise, the feature is not selected.

**Part 3.1**  Guide Grid search regularisation

**Part 3.2**  Models generation since features selected. Multiple soft RF models are built from the different feature subsets.

**Part 3.3**  Success Rate calculation. 

**a.**	Every soft map is reclassified into multiple binary hard maps considering different percentages of affected area (pixel quantiles).

**b.**	True Posite Ratios is computed for all binary maps at increasing areal percentages for every feature subset.

**Part 4**    Summary tables. Model selection based on a trade-off between True Posite Ratios and minimal area from Succes Rate.


**Note:     When we refer to occurrence areas it corresponds to affected areas in the study case.

