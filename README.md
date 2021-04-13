Project overview
=================

The goal of this project is to explore the relationship between multisensory perception and cognitive function in ageing using data from The Irish Longitudinal Study on Ageing (TILDA).
All statistical analyses were performed within the R statistical programming environment, version 3.5.2 (R CoreTeam, 2018). The full analysis can be run in 3 parts; selection, cross-sectional analyses, longitudinal analyses (kml to identify trajectories followed by mixed models). 

Files
-----------------

***Analysis scripts***

*Inclusion/Exclusion criteria [participant_selection]*
*	*select_cross_sectional.R* - apply exclusion criteria to cross-sectional (wave 3) TILDA dataset. 
*	*select_longitudinal.R* - select participants who also have data available from wave 1 to 5 of TILDA and combine cognitive measures across TILDA waves.

*Cross-sectional analysis [cross_sectional_analysis]*
*	*cognitive-mixed-models-CS.R* - run logistic mixed effects models to assess if cognitive measures are related to SIFI cross-sectionally at wave 3. 

*Longitudinal analysis [longitudinal_analysis]*
*	*cognitive-kml.R* - use the KML package to make clusters for longitudinal cognitive trajectories.
*	*cognitive-mixed-models-LD.R* - run logistic mixed effects models to predict SIFI from cognitive trajectories



Data availability
------------------

For information on accessing the TILDA data set see https://tilda.tcd.ie/data/.

References
------------------

R CoreTeam. (2018). R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing. http://www.r-project.org/

