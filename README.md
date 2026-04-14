# REPO Name: Stalheim_Quant_BIO
## Author: Ben Stalheim
## Class: Quantative Methods in Ecology and Evolution

My dataset contains bird detections made using autonomous recording units (ARUs). The raw audio files were ran through BirdNET to annotate and identify the species within the audio. I originally uploaded data from 2023, 2024, and 2025 to BirdNET and then joined the output in a separate script, where I saved a final CSV containing all of the detections. A key note is that this data was collected from 2023:2025 in June and July in southeast Georgia. I will first need to manually validate a subset of recordings for each species. This is because BirdNET detection are not all true positives. Once I validate all of the species, I can set species-specific thresholds to retain as much data as possible and ensure that the data is accurate. I eventually want to calculate community level vital rates like turnover, colonization, and extinction rates within my three study locations and across years. My main hypothesis is that community turnover occurs more rapidly at the reclaimed surface mine compared to the other locations. I will either use dynamic occupancy models, nonparametric estimators, or just use naïve estimators.

## Week 1
- Course: We learned about R and Rstudio and other introductions to coding. I made a script that read in my data file, did some standard manipulations and exploring, and also created an RMarkdown file. 
- **Data:** bn_dat_filtered_95.rds
- **R Script:** week1_ben_stalheim_intro

## Week 2
- Course: We were snowed out. But, we also learned about Github and how to set up repositories, link them to the class organization, and store them on our machines. We vowed to pull, commit, and push all changes. 
- Challenges: My CSV file was > 1GB, which exceeds the 100MB limit that Github allows for pushing and pulling. I finally realized I could convert it to an R data file (.rds), and this smaller file can now be pushed. 
- **Data:** bn_dat_filtered_95.rds
- **R Script:** week2_ben_stalheim_tidyverse

## Week 3
- Course: Lessons in Data Visualization and keys to making quality graphics. We talked and explored ggplot() and all of the fun ways to make figures readable, fun, and informative. We practiced making some preliminary plots with our own data. I found it helpful to see examples of published figures and think about the ways they could have been improved. I am always very impressed with how some of these figures get made. It is so cool! 
- **Data:** bn_dat_filtered_95.rds AND bacs_master_temp.csv (I mainly used the thresholded bird detection data, but did also use my sound level - distance dataset for one plot).  
- **R Script:** week3_ben_stalheim_ggplot

## Week 4
- Course: Statistical philosphies. Importance of expanding beyond P-values. Class discussion on Thursday.
- **Data:** NA
- **R Script:** NA

## Week 5
- Course: We learned about different tests and how implement them in R. We practiced with permutation tests and writing for loops. We also practiced with classical tests like T-tests and correlation tests. 

- **Hypotheses:** 
    - Permutation: Mean species richness per monitoring point is greater at Sansavilla WMA (early successional longleaf pine forest) than at Mission Mine (reclaimed heavy mineral surface mine).

    - Classic: There is a negative correlation between the number of detections/day of Northern Bobwhite and years since disturbance (reclamation) at the Mission Mine.

- **Data:** bn_dat_filtered_95.rds
- **R Script:** week5_ben_stalheim_tests

## Week 6, 7, and 8
- Course: Over the last few weeks, we have been learning about models. Specifically, linear models and the different ways to use them. We learned about univariate models and also multivariate models (with additive or interactive effects). We practiced making models of our own in class and followed along with example data in class. We have also learned about distributions and thinking about what fits our data best. For most of the instances we have practiced linear models using the normal distribution. However, we also practiced with Poisson, Negative Binomial, Binomial, Gamma, Beta (?), and more... Most of my data is count data and uses Poisson distributions. I also have lots of repeated samples, so I have been including random effects in my models. This is structured the same, but has to be fit in a different package and gets the added name of mixed-effects model (fixed and random, I think?). 

*Note:* I added some stuff to the very end of my script that I am working on for my research project and not really sure if I am thinking about it the right way. I am trying to model the effects of disturbance on latent abundance of various bird species. I used Royle-Nichols models in the package unmarked with the function occuRN() to generate latent abundance estimates based on repeated survey data. I then extracted the empirical Bayes estimates for each site and used these "most likely" estimates (I think that's how they worded it in their documentation). The problem is that I have 3 different study locations, each with different monitoring points within each location, and the data is collected over 3 years. When I try and fit an interactive model where years_since_disturbance*location, I often get errors where the model cannot fit all these parameters, so I am left grouping and modeling Abundance ~ Disturbance Age only. But this feels terribly wrong, because my 3 locations are very different. Disturbance type is different (fire vs mining), intensity is different (growing vs nongrowing season burns), and the interactive effect makes the most sense in my head. It seems my best options might be to just model each location on its own and remove the interactive effect. But now I have the issue of multiple years, where each year is independent from one another supposedly, but many species do occupy the same territory over and over. Also, some years are just better for certain bird species, so the year 2025 could have elevated numbers all across one study location, and this throws off the relationship with the disturbance gradient. Anyways, that's where I'm at... not totally sure. I know that I need to use a mixed-effects model to account for the psuedoreplication at my monitoring points (in code, they are labeled "site"). But I also think I need to use a poisson distribution, because my latent abundance estimates, come as counts... 

-**Data:** bn_dat_filtered_95.rd, aru_effort_long.csv and relative_abundance_estimates.csv

-**R Script:** week7_ben_stalheim_linear_models

## Week 9 and 10
- Course: We started by learning about generalized linear models (i.e., modeling data that don't fit normal distribution). For my data, I will mostly be using poisson, negative binomial, and binomial distributions. It is really important to check for overdispersion with these kinds of models as standard qq plots are not applicable (outside of the dharma package). 
