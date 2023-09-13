# Libraries and packages
library(stats)
library(ggplot2)
library(devtools)
install.packages("visdat")
library(visdat)
library(plyr)
library(dplyr)
install.packages("factoextra")
library(factoextra)
library(cluster)

# File import and inspection
medical <- read.csv(file.choose(), header = TRUE)
View(medical)
str(medical)
summary(medical)
# Data preparation and manipulation (preprocessing)
# missing values detection
vis_miss(medical)
colSums(is.na(medical))
# duplicated values detection
duplicated(medical)
sum(duplicated(medical))

# removing unnecessary variables
medical1 <- select(medical, select = 
                     - c("CaseOrder", "Customer_id", "Interaction", "UID",
                         "City", "State", "County", "Zip", "Lat", "Lng",
                         "Population", "Area", "TimeZone", "Job", "Children",
                         "Age", "Marital", "Gender", "ReAdmis", "VitD_levels",
                         "Doc_visits", "Full_meals_eaten", "vitD_supp", 
                         "Soft_drink", "Initial_admin", "HighBlood",
                         "Stroke", "Complication_risk", "Overweight", "Arthritis",
                         "Diabetes", "Hyperlipidemia","BackPain", "Anxiety",
                         "Allergic_rhinitis","Reflux_esophagitis", "Asthma",
                         "Services","TotalCharge", "Additional_charges",
                         "Item1", "Item2", "Item3", "Item4", "Item5", "Item6",
                         "Item7", "Item8"))

# extract cleaned data
write.csv(medical1,"C:\\Users\\garci\\OneDrive\\Desktop\\WGU\\D212 Data Mining II\\MedicalCleaned.csv")

# intermediate calculations
# normalize data
medical1_normalized <- scale(medical1)

# calculate distance
medical1 <- dist(medical1_normalized)

# determine how many K (clusters) are needed within Sum of Squared
fviz_nbclust(medical1_normalized, kmeans, method = "wss")+labs(subtitle = "The Elbow Method")

# K-Means clustering method
km.result <- kmeans(medical1_normalized, centers = 7, nstart = 50)
print(km.result)
