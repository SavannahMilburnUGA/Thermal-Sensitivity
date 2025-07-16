# Thermal-Sensitivity Research Aims
* Analyze the relationship between air temperature & stream temperature (thermal sensitivity - slope) using time-series data in CRB. <br>
* Analyze spatial variation of thermal sensitivity. <br>
* How do landscape variables explain the spatial variation of thermal sensitivity. <br>
# Overview
1) **2021_STandAT.R:** calculates daily mean, min, max air & stream temperature values from 7/1/2021 to 8/31/2021 across 72 CRB sites & 1 AREMP site.
2) **2021_TS.R:** calculates thermal sensitivity values using daily mean air temperature ~ daily mean stream temperature across 73 sites.
3) **app.R:** R Shiny web application that allows user to click one of 73 sites to view air-stream temperature time-series from 7/1/2021 to 8/31/2021 and view thermal sensitivity regression.
4) **2021_EVsAndCorr.R:** loads 40 landscape EVs and creates Spearman's correlation matrices: full and broken down by scale.
5) **2021_MLR.R:** finalized MLR model: thermalSensitivity ~ SLOPE + h2oLakesPe + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA. 
