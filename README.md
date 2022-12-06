# STA521_Proj2
## Andrew Kenealy and Viola Rothschild 

This file contains the requisite information for reproducing our code and report for the Fall 2022 STA 521 project "Cloudy Maps, Clear Classifiers: Comparing Methods of Identifying Clouds Over the Arctic." In the project, we analyze spatial image data from Shi et al.'s (2008) paper "Daytime Arctic Cloud Detection Based on Multi-Angle Satellite Data With Case Studies" to create and compare classification models to predict whether certain data points contain clouds vs. no clouds. 

# Code (R)
## STA521_proj2_AK_VR_FINAL.R
This .R file contains the code for replicating the data analysis and images found in our paper "Cloudy Maps, Clear Classifiers: Comparing Methods of Identifying Clouds Over the Arctic." To apply this code, load the image data into R. This data contains the three pictures from the satelite, each with many observations and 11 features. The features include a y-coordinate, x-coordinate, expert label (+1 = cloud, -1 = not cloud, 0= unlabeled), three expert calculated features (NDAI, SD, and CORR), and 5 raw radiance angle features captured by different cameras (DF, CF, BF, AF, and AN). Once you have loaded these data, the code will run. 

## CVMaster_VR_AK.R
This .R file contains the code for a generic cross valdation function. Load in this file when you load in the data. 

# Latex files 
This is the raw latex file for reproducing the actual report. 
