# REPO Name: Stalheim_Quant_BIO
## Author: Ben Stalheim
## Class: Quantative Methods in Ecology and Evolution

My dataset contains bird detections made using autonomous recording units (ARUs). The raw audio files were ran through BirdNET to annotate and identify the species within the audio. I originally uploaded data from 2023, 2024, and 2025 to BirdNET and then joined the output in a separate script, where I saved a final CSV containing all of the detections. A key note is that this data was collected from 2023 – 2025 in June and July in southeast Georgia. I will first need to manually validate a subset of recordings for each species. This is because BirdNET detection are not all true positives. Once I validate all of the species, I can set species-specific thresholds to retain as much data as possible and ensure that the data is accurate. I eventually want to calculate community level vital rates like turnover, colonization, and extinction rates within my three study locations and across years. My main hypothesis is that community turnover occurs more rapidly at the reclaimed surface mine compared to the other locations. I will either use dynamic occupancy models, nonparametric estimators, or just use naïve estimators.

## Week 1
- Course: We learned about R and Rstudio and other introductions to coding. I made a script that read in my data file, did some standard manipulations and exploring, and also created an RMarkdown file. 
- Data: bn_dat_allyears.RDS

## Week 2
- Course: We were snowed out. But, we also learned about Github and how to set up repositories, link them to the class organization, and store them on our machines. We vowed to pull, commit, and push all changes. 
- Challenges: My CSV file was > 1GB, which exceeds the 100MB limit that Github allows for pushing and pulling. I finally realized I could convert it to an R data file (.RDS), and this smaller file can now be pushed. 