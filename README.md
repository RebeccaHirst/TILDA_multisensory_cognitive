Project overview
=================

The goal of this project is to explore the relationship between multisensory perception and cognitive function in ageing using data from The Irish Longitudinal Study on Ageing (TILDA).


***Scripts***

The analysis was run using the following scripts in the order presented below:

*	*select_cross_sectional.R* - apply exclusion criteria to cross-sectional (wave 3) TILDA dataset. 
*	*select_longitudinal.R* - select and combine cognitive measures across TILDA waves (wave 1 - 5). 
*	*cognitive-kml.R* - use the KML package to make clusters for longitudinal trajectories.
*	*cognitive-mixed-models.R* - run logistic mixed effects models to predict SIFI from cognitive trajectories


Other:

*	*cognitive-clustering.R* - exploration of clustering analysis using other packages (k-means, clvalid) for longitudinal cognitive measures. 


Data availability
------------------

For information on accessing the TILDA data set see https://tilda.tcd.ie/data/.