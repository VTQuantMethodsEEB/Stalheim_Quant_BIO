#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       First Week of Quant Methods 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load required libraries
library(tidyverse)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#      Load Data and Filter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Data Dictionary: This dataset contains detections of bird species using passive
#     acoustic monitoring and a machine learning model called BirdNET. Included 
#     in the data are information on date, time, site, location, and time within 
#     the raw audio file.


# Load the dataset (I already did quite a bit of cleaning to this dataset)
birdnet_data <- read_csv("Data/bn_dat_allyears.csv") |> 
  select(common_name, sp_code, confidence, date, time, year, site, location)

# View the data (this dataset is a little big, I apologize)
view(birdnet_data)

# View the column names
colnames(birdnet_data)
# View the structure and type of data each column contains
str(birdnet_data)
# Dimensions of the dataframe
dim(birdnet_data)

# Select a column using brackets
species <- birdnet_data[, "common_name"]
# Summary of Data
summary(birdnet_data)

# Sometimes people want to use a logit score instead of a confidence score, so
# this adds that based on the confidence column
birdnet_data$logit=log(birdnet_data$confidence/(1-birdnet_data$confidence))

# Or use the mutate function
birdnet_data <- birdnet_data |> 
  mutate(logit = log(confidence / (1 - confidence)))

# Playing around with the data

# Make a dataframe showing the species and raw detection numbers

raw_detections <- birdnet_data |> 
  count(common_name, name = "detections") |> 
  arrange(desc(detections))
print(raw_detections)

# Set a universal threshold on the confidence score to remove likely false positives
birdnet_data_filtered <- birdnet_data |> 
  filter(confidence >= 0.7)
