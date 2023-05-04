# Spatial_Github

All code in this repository has been used to conduct analyses for the following study. This is the latest working draft of an unpublished paper as of May 1st, 2023, and is the second chapter of my dissertation.

# Title
Comparative ecological niche analysis of two congeners reveals differences in their responses to urbanization

# Abstract

Urbanization has altered ecosystems around the world, and will continue to do so over the next century. While avian responses to urbanization at the community level have been characterized in several studies, we lack comparative frameworks for understanding differences in spatial distributions in response to urbanization at the species level. We present a new method for testing differences in distribution between species using MaxENT. We then deploy that method to test for spatial differences between two congers, northern cardinals (Cardinalis cardinalis) and pyrrhuloxia (Cardinalis sinuatus) across the urban ecosystem of Tucson, Arizona and across the entire state of Arizona.

# Introduction
Urbanization has changed landscapes throughout the globe over the last two centuries, and these changes will continue to create extreme pressures on native species. Cities exhibit novel patterns of resource distributions, temperature gradients, and ecological communities alongside unique anthropogenic disturbances like noise pollution and concrete landscapes. Not all species respond similarly to these changes, with species expanding, contracting, or shifting their ranges after urbanization of a region. While we have some evidence that illustrates the broad mechanisms that drive species responses at the community level (Lerman et al. 2021, Warren et al. 2019), we lack an understanding of the more fine scale mechanisms that drive these patterns.
The majority of work into the effect of urbanization on species distributions has been either at the community level, and is focused largely on comparisons of generalist and specialist species or of native and invasive species (Lerman et al. 2020). While these studies illustrate broad ecological patterns, we lack an evolutionary understanding of the drivers that facilitate urban occupancy of some species but not of others. No study to date has compared distributions between species with common niches and evolutionary histories to test for differences in their responses to urbanization. 

Models of a species’ distribution across a region can be constructed by using occurrence data of the species and spatial data of relevant environmental variables across the region of interest. The probable spatial distribution of the species based on the association between occurrence and the environmental variables is called either a spatial distribution model (SDM) or an environmental niche model (ENM; see Peterson & Soberón 2012 for a terminology discussion). For simplicity, we use SDM in this paper. Contemporary methods for constructing these models include generalized additive models, maximum entropy models, random forest models, regularized regression models, and others, but MaxENT (a maximum entropy modeling approach, Phillips et al. 2004, 2006) is both widely used and among the top performing approaches (Valavi et al. 2022). While MaxENT has often been used to model species ranges under various human activities such as climate change (e.g. Jose & Nameer 2020), it has only rarely been applied to urban areas (but see Davis et al. 2012, Ito et al. 2020, Sallam et al 2017, and Wiese et al. 2019). Similarly, MaxENT has only rarely been used to compare species distributions, and has never been used within a hypothesis testing framework to identify regions where species differ in their predicted occupancies. The software ENMTools implements tests to determine if two species have identical distributions or if they have more similar distributions than would be expected by chance, but no test exists to determine areas of significant difference between species.

Tucson, Arizona resides in a valley in the Sonoran Desert between the Santa Catalina Mountains to the north east, the Rincon Mountains to the east, the Tucson Mountains to the west, the Tortolita Mountains to the north, and the Santa Rita Mountains to the south. While the region has had human inhabitants for thousands of years, the impacts of recent urbanization have radically changed this ecosystem over the course of the last 75 years. A canal system was developed in this region by the Hohokam people between 750 A.D. and 1100 A.D. (CITE), and the area was first settled by European colonists in 1692. The population of Tucson was only 7,532 in the 1900 U.S. Decennial Census. Between 1950 and 1960, the population jumped from 45,454 to 212,892, and the city has grown steadily to its present day population of 542,629. It is the second largest city in Arizona behind Phoenix, and the metropolitan area has a total population of 1,043,433. This area is characterized by a mild winter and an extremely hot summer with an annual rainfall around 10.61 inches. The Rillito River flows along the northern border of Tucson, and the Santa Cruz River flows along the west side of the city, although due to water diversion from the Santa Cruz it is now dry in this area except during exceptional rains.

Northern cardinals (Cardinalis cardinalis) and pyrrhuloxia (Cardinalis sinuatus) are similarly distributed throughout much of the Sonoran Desert and were estimated to have diverged around 6 million years ago (Kaiya Provost pers. comm., Barker et al. 2015, Hooper and Price 2017, Jetz et al. 2012). The southwestern northern cardinal subspecies (C. c. igneus) is a distinct population that is thought to have diverged from the nominate subspecies ~2.4 million years ago (Smith et al. 2011). However, there is not perfect overlap between the two Cardinalis species. The range of the southwestern northern cardinal subspecies extends further north than the range of pyrrhuloxia, and while both species are commonly seen around the Tucson metropolitan area, only northern cardinals are commonly seen around the Phoenix metropolitan area, although both are much more sparsely distributed than the eastern population of northern cardinals. The northern expansion of northern cardinals is believed to have only occurred after European colonization as a result of land change uses due to human activities, and the historic ranges of the two species were likely much more similar in the Arizonan region.

In this study, we used community-science data (eBird; CITE) to compare the distributions of northern cardinals and pyrrhuloxia across two regions: the state of Arizona and across the Tucson metropolitan area. We also investigated differences in the biotic, bioclimatic, and socioeconomic factors that predict distributions between species within studied regions, and within species between urban and statewide analyses. We present a novel approach to studying urban effects on congeneric species, and novel findings about the different responses of species to urbanization.


# Hypotheses:
H10: The two species do not differ in their distributions across an urban environment.
H1A: The species that has a broader statewide range (northern cardinals) will also have a broader range in the urban environment than their congener (pyrrhuloxia).

H20: The environmental variables that predict the distributions of the two species will not differ in the city.
H2A: The environmental variables that predict the distributions of the two species in an urban environment will be different from those across the broader range.

# Methods
## Species Occurrence Data
We used observations of birds from the Tucson Bird Count (TBC; 2001-2020) and eBird (2017-2021). We used all years available from TBC, and we filtered eBird data to only keep 5 years of data. We made this decision because eBird has increased in popularity over time, and some of the datasets from earlier years may have been more biased by cultural differences in accessibility of eBird (CITE). We filtered the dataset to keep only observations during the breeding season, which we conservatively approximated to be April and May (pers. obs.), and to only keep one observation per cell of each species.

## Environmental Data:

As input into our model of the distributions of these species, we used environmental files representing elevation, 19 bioclimatic variables from the World Clim database, the 2016 USFS NLCD Tree Canopy Cover file, the 2019 NLCD Percent Developed Imperviousness (CONUS) file, and the 2019 National Land Cover Database (NLCD) categories of land cover variables. We split the NLCD file into separate tiff files, each representing one of 11 of the 20 variables in the NLCD file, excluding the 4 that exclusively pertain to Alaska, 1 that is irrelevant to the low desert (perennial ice/snow), and all 4 variables relating to urban development (Developed, Open Space; Developed, Low Intensity; Developed, Medium Intensity; and Developed, High Intensity). We excluded the development variables because they are categorical representations of the percent of impervious surfaces in an area, which would be redundant with and less informative than the NLCD Percent Developed Impervious file. The cells in each of the files generated from the NLCD land cover file represented either the presence of that variable with a 0 value, or the distance from that cell to the nearest cell containing that variable. These represented distance from open water, barren land, deciduous forest, evergreen forest, mixed forest, shrub/scrub, grassland/herbaceous, pasture/hay, cultivated crops, woody wetlands, and emergent herbaceous wetlands.

Prior to analysis, each tiff file was reprojected to the World Geodetic System 84 (WGS 84) coordinate reference system, cropped to the boundaries of the state of Arizona (CITE vector file source), and converted to an ASCII file, which is required for input into Maxent. We then cropped these ASCII files for an analysis across the city of Tucson using the extent of a minimum longitude of -111.183682, a max of -110.720903, a minimum latitude of 32.034553, and a maximum latitude of 32.554540. These were selected by determining the boundaries of the urban area from the US Census urban area spatial file (CITE). 

For the maxent analysis of each species across the entire state of Arizona, we randomly selected 10,000 points across the region for use as the background environmental conditions, randomly selected 50% of the observations for training data and used the remaining for model testing, following methods from https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md. Our analysis across Tucson used the same methods but only used 2,500 background points. We replicated these methods using subsets of observational data across the state using only eBird data, and across Tucson using only eBird or only TBC data and found similar results so we only present the model that used the entire available data for observations. This produced our empirical models representing the distributions of northern cardinals and pyrrhuloxia across the state of Arizona and across the city of Tucson.

To test whether the two species exhibited any differences in their distributions across the city of Tucson, we removed the species designations associated with each observation in the dataset of raw observations. Then, we randomly assigned each of the observations to one of the two species, keeping the number of total observations for each species equal to the true number of observations of that species. We then ran the maxent model using the same parameters as we did for our empirical analysis, and then subtracted the model for the spatial distribution of the probability of occurrence of pyrrhuloxia from the model for the spatial distribution of the probability of occurrence of northern cardinals. We repeated this process 1,000 times with different random permutations of the data to generate a null distribution of the differences between the distributions of the two species given the number of observations of each species. Finally, we subtracted the empirical model of the distribution of pyrrhuloxia from the empirical model of the distribution of northern cardinals and compared this file to the 1,000 null files. We used a significance level of 0.05, so we kept any cell in the empirical difference file that demonstrated an absolute difference between the probabilities of the two species that is greater than 950 of the null models. Every other cell was converted to 0. We repeated this process using models across the state of Arizona to identify regions across the state where the two species differ. We visualized the spatial distribution of the significant differences in probability of occurrences of the two species in QGIS.

We also compared permutation importance values of the empirical maxent models. We used an arbitrary cutoff of a minimum of 5% to determine which variables contributed to the model, and then compared between species and regions to identify factors that differ in determining the distributions of the species.

Finally, to test for differences in the environmental variables associated with each species, we extracted values from the raster files of each environmental variable for each observation of either species. We then ran t-tests on each environmental variable to compare for differences between species across the entire state of Arizona and across the Tucson region. 

# Results
## Figure 1
<img width="889" alt="Screenshot 2023-05-01 at 7 37 05 PM" src="https://user-images.githubusercontent.com/49250788/235567661-b096864d-21cc-4174-958d-5fbe296446f1.png">


While much of the ranges of the two species overlapped, their distributions differed in significant ways in both the statewide analysis and the analysis across Tucson. Northern cardinals had a higher probability of occurrence in the city of Tucson than pyrrhuloxia, and had a higher probability of occurrence in the regions north of Tucson in the statewide analysis. 

In models of the distributions of each species across the city of Tucson, five environmental variables contributed to the models of both species. These were the second and third principal components of the climate variables, elevation, NLCD 42 (distance to evergreen forests), and NLCD 82 (distance to cultivated crops). None contributed only to the model of northern cardinals, and four contributed only to the model of pyrrhuloxia. They were surface imperviousness, NLCD 31 (distance to barren land), NLCD 41 (distance to deciduous forest), and NLCD 71 (distance to grassland/herbaceous).

Across the Tucson region, observations of northern cardinals were made at significantly greater impervious surface and greater canopy cover compared to observations of pyrrhuloxia (). Northern cardinals were also closer to open water, barren land, evergreen forest, and woody wetlands (NLCD 11, 31, 42, 90), further from deciduous forest and shrub/scrub (NLCD 41, 52), and at higher Mean Temperature of Coldest Quarter (Clim 11) compared to pyrrhuloxia.

In models of the distributions of each species across the state of Arizona, only two variables had significant permutation importance scores for both species: the second principal component of the climate variables, and elevation. For the model of northern cardinals, median household income, NLCD 11 (distance to open water), NLCD 82 (distance to cultivated crops), and NLCD 90 (distance to woody wetlands) were also significant. For the model of pyrrhuloxia, the third principal component of the climate variables and NLCD 41 (distance to deciduous forest) were significant.

Across the state of Arizona, observations of northern cardinals were made at significantly greater imperviousness and canopy cover, lower elevation, closer to deciduous forest, pasture/hay, and woody wetlands (NLCD 41, 81, 90), further from evergreen forest and shrub/scrub (NLCD 42, 52), at higher Mean Diurnal Range, Temperature Seasonality, Max Temperature of Warmest Month, Temperature Annual Range, Mean Temperature of Warmest Quarter, Precipitation of Driest Quarter, Precipitation of Coldest Quarter (Clim 2, 4, 5, 7, 10, 17, 19), and at lower Isothermality, Mean Temperature of Wettest Quarter, Precipitation of Wettest Month, Precipitation Seasonality, and Precipitation of Warmest Quarter (Clim 3, 8, 13, 15, 18).

# Discussion

We demonstrated that two closely-related species with similar niches have different spatial distributions following the urbanization of a region, and that northern cardinals are more prevalent in urban areas than pyrrhuloxia. Surface imperviousness, a measurement of urbanization, was important for the citywide analysis of pyrrhuloxia but not of northern cardinals, and significantly differed between the two species across both the Tucson region and the statewide region. We also demonstrated that, despite being found in similar ecosystems when the co-occur, northern cardinals occupy a much broader range of territory across the Arizona region. There were no regions where the probability of occurrence of pyrrhuloxia was greater than that of northern cardinals, and therefore the range of pyrrhuloxia can be thought of as a subset of the total range of northern cardinals. The factors that allow northern cardinals to occupy such a broad range, especially the anthropogenic factors, should be further investigated.

We expected that urban areas might be excluding pyrrhuloxia from a region that they might otherwise inhabit, but that changes in water availability associated with European colonization and subsequent irrigation had permitted the expansion of northern cardinals across the state. We found evidence for both. In the model of the Tucson region, surface imperviousness has a significant permutation importance score for pyrrhuloxia but not for northern cardinals. This suggests that urbanization plays a role in determining the distribution of pyrrhuloxia. The only regions where northern cardinals were more likely to occur than pyrrhuloxia were in the areas with high surface imperviousness near the city center of Tucson. In the model of the entire Arizona region, both median household income and distance to open water influenced the distribution of northern cardinals, but neither had a significant permutation importance score for pyrrhuloxia. Median household income is a proximate variable for human determined resource distribution, as areas with more income have the financial resources to cultivate more expensive and water intensive landscaping practices. The distribution of open water in the state of Arizona is heavily engineered, with irrigation systems, dams, and man made lakes creating a novel pattern of water availability for the native wildlife. The importance of open water and wealth for northern cardinals suggests that anthropogenic resource distributions have permitted the expansion of this species.

Our findings provide some preliminary evidence of the non-anthropogenic differences in ecological niches that might allow for this differentiation in both statewide and citywide distributions. Climate plays an important role in differentiating the species at both scales. Across the city of Tucson, only one variable differed between the species, the mean temperature of the coldest quarter. This same variable did not differ between the two species at the statewide scale, although 12 other climate variables did. These differences demonstrate more contradictions than overall pattern. Northern cardinals were found in areas with higher fluctuations in temperature at the daily, seasonal, and annual scale and with greater precipitation in the driest and coldest parts of the year. However, pyrrhuloxia were found in areas with more precipitation in the wettest month and warmest quarter, which seems contradictory. Pyrrhuloxia are also found in higher mean temperatures in the wettest quarters, but northern cardinals were found in areas with higher mean temperature of the warmest quarter and maximum temperature of the warmest month. The overall combination of climatic factors, rather than individual climate metrics, may therefore be more deterministic for the occupancy of an area by either one of these species. However, these contradictions may indicate that anthropogenic factors play a larger role in determining the distributions of these species. If proximity to urban areas and particular human-produced resources like suburban yards or water sources determines the distribution of one or both species, rather than the underlying climate of a region, these unusual patterns might occur. Across the entire state, surface imperviousness did not have a significant permutation importance score for either species. It did have a significant score for the Tucson analysis of Pyrrhuloxia, but not of Northern Cardinals. 

