# Using same 5 significant MLR covariates to make SSN model - compare models  
# Uses old SSN object from Michael (deleted all sites not from 2021 & irrelevant variables - Michael created in ArcGIS old tool)

# Clean house & remove saved files (keeping it clean)
# Remove all objects in workspace 
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Load packages
library(readr)
library(SSN2)

# Read 2021 data - 72 sites from N to S w/ thermal sensitivity + x, y, GRIDCODEs, 40 landscape covariates
SortedTSAndEVs2021 <- read_csv("results/2021/SortedTSAndEVs2021.csv")
# Check
head(SortedTSAndEVs2021)
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Import CRB SSN object Michael created
CRB_SSN <- ssn_import("EVs/SSN/ClackHist.ssn", predpts = "preds", overwrite = TRUE)

# Check
names(CRB_SSN) # SSN object has edges, obs, preds (null), path
# CRB_SSN$edges has 891 features & 49 fields w/ LINESTRING geometry in NAD 1983 Albers projection of streams in CRB w each COMID/REACHCODE having:
## FLOWDIR, FTYPE, FCODE, AreaSqKM, TotDASqKM, GRIDCODE< h2oKm2, BFI, h2oHiCascP, SLOPE, etc. 

# CRB_SSN$obs has Name, Year_, Max7DADM, Lat, Long_, NEAR_FID, NEAR_DIST, NEAR_X, NEAR_Y, NEAR_ANGLE, rid, ratio, GRIDCODE, BFI, SLOPE, Solar, Elev
## h2oKm2, upDist, afvArea, locID, netID, pid, geometry, netgeom

# CRB_SSN$preds has prediction points at 17, 274 locations w/ OBJECTID, Id, NEAR_FID, NEAR_DIST, NEAR_X, NEAR_Y, NEAR_ANGLE, rid, ratio, upDist, 
## afvArea, h2oKm2, GRIDCODE, BFI, SLOPE, Solar, HiCasc, Elev, locID, netID, pid, geometry, netgeom

summary(CRB_SSN) # observations on 25 varbs across 79 sites w/in x: -2056405 to -1993556 & y: 2700036 to 2782753
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Loading package
library(tidyverse)

# Select subset of MLR covariates
MLR_covariates <- SortedTSAndEVs2021 %>%
  select(index, GRIDCODE, x, y, thermalSensitivity, SLOPE, h2oHiCascP, h2oWetland, Shrub21, BurnRCA)

# Check
# View(MLR_covariates)

# Join MLR covariates with sf object of observation sites in CRB_SSN object by GRIDCODE
CRB_SSN$obs <- CRB_SSN$obs %>%
  left_join(MLR_covariates, by = "GRIDCODE", relationship = "many-to-many")
#-----------------------------------------------------------------------------------------------------------------------------------------------
## Sites in TS data are located on SAME stream segment - SAME GRIDCODE - causing SSN structural issues ?
## Investigating many-to-many relationship between 'x' & 'y' warning message - sites are located on same stream - have same GRIDCODE
MLR_covariates %>% 
  count(GRIDCODE) %>% 
  filter(n > 1)
# View(MLR_covariates)
# MLR_covariates has repeated GRIDCODES across 72 sites but makes sense since some sites are on same stream segment:
## GRIDCODE - # of sites w/ SAME - Sites
# 479419 - 2 - Site 44, Site 45
# 479722 - 2 - Site 49, Site 50
# 479779 - 4 - Site 65, Site 66, Site 68, Site 69
# 479834 - 2 - Site 15, Site 20
# 479836 - 2 - Site 41, Site 42
# 480426 - 2 - Site 24, Site 25

# Check how many unique GRIDCODEs you have in each dataset
length(unique(CRB_SSN$obs$GRIDCODE))  # SSN has 74 unique GRIDCODES - 5 have duplicate GRIDCODES
length(unique(MLR_covariates$GRIDCODE))  # TS has 64 unique GRIDCODES - 8 have duplicate GRIDCODES

# See which GRIDCODEs are in SSN but NOT in TS data
SSN_GRID <- unique(CRB_SSN$obs$GRIDCODE)
TS_GRID <- unique(MLR_covariates$GRIDCODE)
missing_from_TS <- setdiff(SSN_GRID, TS_GRID)
missing_from_SSN <- setdiff(TS_GRID, SSN_GRID)

print(paste("GRIDCODEs in SSN but not in thermal data:", length(missing_from_TS))) # 11
print(paste("GRIDCODEs in thermal data but not in SSN:", length(missing_from_SSN))) # 1
# View(CRB_SSN$obs)

# Summary
## SSN object has 74 unique GRIDCODES across 79 sites - 5 have duplicates
## TS has 64 unique GRIDCODES across 72 sites - 8 have duplicates
## 11 GRIDCODES exist in the SSN object but not in TS data
## 1 GRIDCODE exists in TS data but not in SSN - GRIDCODE: 479088
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Check 
summary(CRB_SSN) # obs changes from 25 varbs across 79 sites to 31 varbs across 93 sites
# Variables did increase by 6 so good
#-----------------------------------------------------------------------------------------------------------------------------------------------
## Likely won't use this since we are not predicting to unseen sites just comparing MLR to SSN
# Join MLR covariates w/ prediction sites in CRB_SSN object by GRIDCODE - access the sf inside the list
CRB_SSN$preds$preds <- CRB_SSN$preds$preds %>%
  left_join(MLR_covariates, by = "GRIDCODE", relationship = "many-to-many")

# Check
# View(CRB_SSN$preds$preds)
#-----------------------------------------------------------------------------------------------------------------------------------------------
library(sf)
# Check how many sites have duplicated thermal sensitivity values - checking if true duplicate or sites are just similar
CRB_SSN$obs %>%
  st_drop_geometry() %>%
  count(thermalSensitivity) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# Checking duplicated TS values actually match the same GRIDCODES - there are multiple sites on same stream reach so having duplicated TS values is accurate
duplicated_values <- c(0.162, 0.120, 0.126, 0.147, 0.201, 0.354, 0.360, 0.363, 0.397, 0.434, 0.436, 0.452, 0.479, 0.517)

# View observation sites by index
CRB_SSN$obs %>%
  arrange(index) %>%
  filter(thermalSensitivity %in% duplicated_values) %>%
  select(index, Name, thermalSensitivity, GRIDCODE) %>%
  View()
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Filter NAs out of observation sites in CRB SSN object
View(CRB_SSN$obs) # before
CRB_SSN$obs <- CRB_SSN$obs %>%
  filter(!is.na(thermalSensitivity) & !is.na(SLOPE.y) & !is.na(h2oHiCascP) & !is.na(h2oWetland) & !is.na(Shrub21) & !is.na(BurnRCA))
# Check
View(CRB_SSN$obs) # No NAs
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Run simple model using SSN lm but w/ no spatial components 
SSN_simple <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN)
varcomp(SSN_simple) # Nugget component explains 38.5% more variability
glance(SSN_simple)
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Fixing subscript error when trying to add spatial components to SSN object - could be because: 
# Dupe sites: causing indexing issues
# Missing distance matrices - reran distance matrices but got error
# Error w/ distance matrices: 
# A - some sites got duplicated during join
# B - multiple TS match to same SSN sites
# C - locID or pid got duplicated during join
#-----------------
# Check for dupes (locID repeated, pid repeated - is index different or repeated?)
# locID not unique 
CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  count(locID) %>% 
  filter(n > 1)
# pid not unique
CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  count(pid) %>% 
  filter(n > 1)
# Look at specific rows of pid
CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  filter(pid %in% c('77', '167', '383', '400')) %>%
  View()
#-----------------
# Determining true duplicates - look at index, same index means true duplicate, otherwise just same stream segment
# Removing observations that are true duplicates (same index but at different pids):
CRB_SSN$obs <- CRB_SSN$obs %>%
  filter(!(pid == 53 & index == 24)) %>% # Remove obs w/ pid 53 & index 24
  filter(!(pid == 56 & index == 25)) %>% # Remove obs w/ pid 56 & index 25
  filter(!(pid == 167 & index == 66)) %>% # Remove obs w/ pid 167 & index 66
  filter(!(pid == 383 & index == 65)) %>% # Remove obs w/ pid 383 & index 65
  filter(!(pid == 167 & index == 69)) %>% # Remove obs w/ pid 167 & index 69
  filter(!(pid == 383 & index == 68)) %>% # Remove obs w/ pid 383 & index 68
  filter(!(pid == 242 & index == 20)) %>% # Remove obs w/ pid 242 & index 20
  filter(!(pid == 332 & index == 15)) %>% # Remove obs w/ pid 332 & index 15
  filter(!(pid == 644 & index == 45)) %>% # Remove obs w/ pid 644 & index 45
  filter(!(pid == 294 & index == 44)) # Remove obs w/ pid 294 & index 44
#-----------------
# Check for dupes again
# locID not unique 
CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  count(locID) %>% 
  filter(n > 1)
# pid not unique
CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  count(pid) %>% 
  filter(n > 1)
# Look at specific rows of pid
CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  filter(pid %in% c('77', '167', '383', '400')) %>%
  View()
#-----------------
# Assigning new pids to observation sites w/ same pid but are DIFFERENT sites - NOT TRUE DUPLICATES
# Find the maximum pid 
max_pid <- max(CRB_SSN$obs$pid, na.rm = TRUE) # 644
# Allow site w/ first index to keep original pid:
## obs w/ pid 77 & index 41
## obs w/ pid 167 & index 65
## obs w/ pid 383 & index 66
## obs w/ pid 400 & index 49
# Assign new pid to site w/ second index based on max pid of 644 value
## obs w/ pid 77 & index 42
## obs w/ pid 167 & index 68
## obs w/ pid 383 & index 69
## obs w/ pid 400 & index 50
CRB_SSN$obs <- CRB_SSN$obs %>%
  mutate(pid = case_when(pid == 77 & index == 42 ~ max_pid + 1, pid == 167 & index == 68 ~ max_pid + 2, pid == 383 & index == 69 ~ max_pid + 3, pid == 400 & index == 50 ~ max_pid + 4, TRUE ~ pid))
#-----------------
# Check for dupes again
# locID not unique
CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  count(locID) %>% 
  filter(n > 1)
# pid good
CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  count(pid) %>% 
  filter(n > 1)
# Look at specific rows of locID
CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  filter(locID %in% c('68', '74', '141', '158')) %>%
  View()
#-----------------
# Assigning new locIDs to observation sites w/ same locID but are DIFFERENT sites - NOT TRUE DUPLICATES
# Find the maximum locID 
max_locID <- max(CRB_SSN$obs$locID, na.rm = TRUE) # 180 
# Allow site w/ first index to keep original locID:
## obs w/ locID 158 & index 41
## obs w/ locID 141 & index 65
## obs w/ locID 74 & index 66
## obs w/ locID 68 & index 49
# Assign new locID to site w/ second index based on max locID of 180 value
## obs w/ locID 158 & index 42
## obs w/ locID 141 & index 68
## obs w/ locID 74 & index 69
## obs w/ locID 68 & index 50
CRB_SSN$obs <- CRB_SSN$obs %>%
  mutate(locID = case_when(locID == 158 & index == 42 ~ max_locID + 1, locID == 141 & index == 68 ~ max_locID + 2, locID == 74 & index == 69 ~ max_locID + 3, locID == 68 & index == 50 ~ max_locID + 4, TRUE ~ locID))
#-----------------
# Final check for dupes
# locID good
CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  count(locID) %>% 
  filter(n > 1)
# pid good
CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  count(pid) %>% 
  filter(n > 1)
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Ensured no true duplicated sites & all locID/pid unique & checked TS valid
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Creating new distance matrix files since added new sites:
# Supplement CRB SSN object w/ hydrologic distance matrices preserving directionality
ssn_create_distmat(ssn.object = CRB_SSN, predpts = "preds", overwrite = TRUE)
# Stream distance matrices saved as local files in .ssn folder
# Matrices sotred as .Rdata files in sub-folder of obs and preds for observation sites & prediction sites
# Got error
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Trying to fix error: 
# Reset row names to be sequential
row.names(CRB_SSN$obs) <- 1:nrow(CRB_SSN$obs)

# Try to create distance matrices again 
ssn_create_distmat(ssn.object = CRB_SSN, overwrite = TRUE)

## Keep getting this error:
# Error in `.rowNamesDF<-`(x, value = value) : 
#   duplicate 'row.names' are not allowed
# In addition: Warning message:
# non-unique values when setting 'row.names': '167', '383', '400', '77' 
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Where are 167, 383, 400, 77 in error coming from ?

# Check row names are in the netgeom strings - YES
any(grepl("167|383|400|77", CRB_SSN$obs$netgeom))

# Check SSN edges, obs, preds, path
any(row.names(CRB_SSN$edges) %in% c("167", "383", "400", "77")) # YES
any(row.names(CRB_SSN$obs) %in% c("167", "383", "400", "77")) # NO
any(row.names(CRB_SSN$preds) %in% c("167", "383", "400", "77")) # NO

# Error in CRB_SSN edges OR netgeom strings in CRB_SSN obs 
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Edge row names are non-sequential ? - NOPE edge row names are already sequential - 1 to 500 so include 167, 383, 400, 77
# View(CRB_SSN$edges) - NOT CRB_SSN edges so look at netgeom strings: 
#-----------------------------------------------------------------------------------------------------------------------------------------------
# ssn_create_distmat() reads pid values from netgeom strings but there are duplicates
# Creates an error as it tries to use pids as row names for matrices but dupes (have old pids)

# Find which netgeom strings need to be fixed
# netgeom strings reference old pid values of 167, 383, 400, 77
dupe_netgeom <- CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  filter(grepl("167|383|400|77", netgeom)) %>%
  select(pid, rid, index, netgeom)
# netgeom format for sites: "SNETWORK (netID rid upDist ratio pid locID)"
print(dupe_netgeom)

# Likely error: reading pid value from netgeom - has old pids from dupes - multiple netgeom strings contain same pids
## Rows w/ pid = 645 & index 42, have netgeom containing 77 - change pid to 645
## Rows w/ pid = 646 & index 68, have netgeom containing 167 - change pid to 646
## Rows w/ pid = 647 & index 69, have netgeom containing 383 - change pid to 647
## Rows w/ pid = 648 & index 50, have netgeom containing 400 - change pid to 648
# Update netgeom strings to use changed pids instead of old pids causing duplicates
CRB_SSN$obs <- CRB_SSN$obs %>%
  mutate(netgeom = case_when(pid == 645 ~ gsub(" 77 ", " 645 ", netgeom), pid == 646 ~ gsub(" 167 ", " 646 ", netgeom), pid == 647 ~ gsub(" 383 ", " 647 ", netgeom), pid == 648 ~ gsub(" 400 ", " 648 ", netgeom), TRUE ~ netgeom))

# Check
CRB_SSN$obs %>% 
  st_drop_geometry() %>%
  filter(grepl("167|383|400|77", netgeom)) %>%
  select(pid, rid, index, netgeom)

# Create distance matrices 
ssn_create_distmat(ssn.object = CRB_SSN, overwrite = TRUE)
#-----------------------------------------------------------------------------------------------------------------------------------------------
# View CRB stream network w/ obs sites as brown circles & prediction as blue
ggplot() +
  geom_sf(data = CRB_SSN$edges) +
  geom_sf(data = CRB_SSN$preds$preds, pch = 17, color = "blue") +
  geom_sf(data = CRB_SSN$obs, color = "brown", size = 4) +
  theme_bw()
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Visualize distribution of thermal sensitivity on CRB stream network
ggplot() +
  geom_sf(data = CRB_SSN$edges) +
  geom_sf(data = CRB_SSN$obs, aes(color = thermalSensitivity), size = 4) +
  scale_color_viridis_c(limits = c(0, 0.6), option = "H") +
  theme_bw()
#-----------------------------------------------------------------------------------------------------------------------------------------------
# ssn_lm() function w/ commonly used arguments:
## Default for tailup/taildown/euclidean is none - MUST be specified if relevant covariances are desired
#-----------------
# tailup_type = tail-up covariance = linear/spherical/exponential/mariah/epa/none
#-----------------
# taildown_type = tail-down covariance = linear/spherical/exponential/mariah/epa/none
#-----------------
# euclid_type = Euclidean covariance = spherical/exponential/gaussian/cosine/cubic/pentaspherical/wave/jbessel/gravity/rquad/magnetic/none
#-----------------
# nugget_type = nugget/none
## Default for nugget is nugget = specifies nugget effect since many ecological processes have localized variability that is important to capture
#-----------------------------------------------------------------------------------------------------------------------------------------------
# How to decide between covariance functions?
# Fit several models & compare fits using AIC or CV error
# Or: visualize Torgegram() = semivariogram that describes variability in streams data based on flow-connected, flow-unconnected, & Euclidean spatial relationships
## Torgegram describes HOW semivariance (halved average squared difference) b/w observations changes w/ hydrologic or Euclidean distances 
### Strong dependence b/w flow-connected or flow-unconnected relationships : semivariance will increase w/ respective distance
### Not strong dependence: semivariance will be relatively flat
#-----------------------------------------------------------------------------------------------------------------------------------------------
# PLotting torgegram to help w/ deciding b/w covariance functions
tg <- Torgegram(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, type = c("flowcon", "flowuncon", "euclid"))
plot(tg)
# Flow-connected seems to increase but has some low semivariance ~40,000
# Flow-unconnected seems to decrease but also increases b/w 40,000 & 50,000
# Euclidean seems to decrease

# Suggest would benefit from a combination of tail-up, tail-down, & Euclidean components
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Re-run the model using SSN2 & compare among different covariance functions
## Trying different tail-up arguments: linear, spherical, exponential, mariah, epa OR none
## additive argument MUST be specified 

# LARGER de parameter
# Lower AIC/AICc

# Run simple model using SSN lm but w/ no spatial components - 
SSN_simple <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN)
summary(SSN_simple)
tidy(SSN_simple)
varcomp(SSN_simple) # Nugget component explains 38.5% more variability
glance(SSN_simple)

SSN_tailUp_linear <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", additive = "afvArea")
summary(SSN_tailUp_linear)
varcomp(SSN_tailUp_linear)
glances(SSN_simple, SSN_tailUp_linear)

SSN_tailUp_spherical <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "spherical", additive = "afvArea")
summary(SSN_tailUp_spherical)
varcomp(SSN_tailUp_spherical)
glances(SSN_tailUp_linear, SSN_tailUp_spherical)

SSN_tailUp_exponential <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "exponential", additive = "afvArea")
summary(SSN_tailUp_exponential)
varcomp(SSN_tailUp_exponential)
glances(SSN_tailUp_linear, SSN_tailUp_exponential)

SSN_tailUp_mariah <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", additive = "afvArea")
summary(SSN_tailUp_mariah)
varcomp(SSN_tailUp_mariah)
glances(SSN_tailUp_linear, SSN_tailUp_mariah)

SSN_tailUp_epa <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", additive = "afvArea")
summary(SSN_tailUp_epa)
varcomp(SSN_tailUp_epa)
glances(SSN_tailUp_mariah, SSN_tailUp_epa)
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Re-run the model using SSN2 & compare among different covariance functions
## Trying different tail-down arguments: linear, spherical, exponential, mariah, epa OR none

SSN_tailDown_linear <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, taildown_type = "linear")
summary(SSN_tailDown_linear)
varcomp(SSN_tailDown_linear)
glances(SSN_tailUp_mariah, SSN_tailDown_linear)

SSN_tailDown_spherical <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, taildown_type = "spherical")
summary(SSN_tailDown_spherical)
varcomp(SSN_tailDown_spherical)
glances(SSN_tailDown_linear, SSN_tailDown_spherical)

SSN_tailDown_exponential <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, taildown_type = "exponential")
summary(SSN_tailDown_exponential)
varcomp(SSN_tailDown_exponential)
glances(SSN_tailDown_spherical, SSN_tailDown_exponential)

SSN_tailDown_mariah <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, taildown_type = "mariah")
summary(SSN_tailDown_mariah)
varcomp(SSN_tailDown_mariah)
glances(SSN_tailDown_exponential, SSN_tailDown_mariah)

SSN_tailDown_epa <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, taildown_type = "epa")
summary(SSN_tailDown_epa)
varcomp(SSN_tailDown_epa)
glances(SSN_tailDown_mariah, SSN_tailDown_epa)
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Re-run the model using SSN2 & compare among different covariance functions
## Trying different Euclidean arguments: spherical, exponential, gaussian, cosine, cubic, pentaspherical, wave, jbessel, gravity, rquad, magnetic
SSN_e_spherical <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, euclid_type = "spherical")
summary(SSN_e_spherical)
varcomp(SSN_e_spherical)
glances(SSN_tailDown_mariah, SSN_e_spherical)

SSN_e_exponential <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, euclid_type = "exponential")
summary(SSN_e_exponential)
varcomp(SSN_e_exponential)
glances(SSN_e_spherical, SSN_e_exponential)

SSN_e_gaussian <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, euclid_type = "gaussian")
summary(SSN_e_gaussian)
varcomp(SSN_e_gaussian)
glances(SSN_e_exponential, SSN_e_gaussian)

SSN_e_cosine <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, euclid_type = "cosine")
summary(SSN_e_cosine)
varcomp(SSN_e_cosine)
glances(SSN_e_gaussian, SSN_e_cosine)

SSN_e_cubic <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, euclid_type = "cubic")
summary(SSN_e_cubic)
varcomp(SSN_e_cubic)
glances(SSN_e_gaussian, SSN_e_cubic)

SSN_e_pentaspherical <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, euclid_type = "pentaspherical")
summary(SSN_e_pentaspherical)
varcomp(SSN_e_pentaspherical)
glances(SSN_e_cubic, SSN_e_pentaspherical)

SSN_e_wave <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, euclid_type = "wave")
summary(SSN_e_wave)
varcomp(SSN_e_wave)
glances(SSN_e_cubic, SSN_e_wave)

SSN_e_jbessel <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, euclid_type = "jbessel")
summary(SSN_e_jbessel)
varcomp(SSN_e_jbessel)
glances(SSN_e_cubic, SSN_e_jbessel)

SSN_e_gravity <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, euclid_type = "gravity")
summary(SSN_e_gravity)
varcomp(SSN_e_gravity)
glances(SSN_e_jbessel, SSN_e_gravity)

SSN_e_rquad <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, euclid_type = "rquad")
summary(SSN_e_rquad)
varcomp(SSN_e_rquad)
glances(SSN_e_jbessel, SSN_e_rquad)

SSN_e_magnetic <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, euclid_type = "magnetic")
summary(SSN_e_magnetic)
varcomp(SSN_e_magnetic)
glances(SSN_e_jbessel, SSN_e_magnetic)

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Create empty dataframe with column names
model_comparison <- data.frame(model = character(),AIC = numeric(),AICc = numeric(),pseudo_r2 = numeric(),tailup_prop = numeric(), taildown_prop = numeric(), euclid_prop = numeric(), nugget_prop = numeric(), stringsAsFactors = FALSE)

# NO SPATIAL - SSN_simple
row_SSN_simple <- data.frame(model = "Simple - NO Spatial", AIC = glance(SSN_simple)$AIC, AICc = glance(SSN_simple)$AICc, pseudo_r2 = glance(SSN_simple)$pseudo.r.squared, tailup_prop = varcomp(SSN_simple)$proportion[2], taildown_prop = varcomp(SSN_simple)$proportion[3], euclid_prop = varcomp(SSN_simple)$proportion[4], nugget_prop = varcomp(SSN_simple)$proportion[5])
model_comparison <- rbind(model_comparison, row_SSN_simple)
# TAIL UP - Mariah, Linear, epa:
## M w/ TD (1, 2, 3, 0) & M w/ E (1, 2, 3, 0)

## L w/ TD (1, 2, 3, 0) & L w/ E (1, 2, 3, 0)

## E w/ TD (1, 2, 3, 0) & E w/ E (1, 2, 3, 0)

## 0 w/ TD (1, 2, 3) & E / (1, 2, 3)

### Iterate through best options from each spatial component to find best model
# Tail-Up: Mariah
m_m_j <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", taildown_type = "mariah", euclid_type = "jbessel", additive = "afvArea")
m_m_c <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", taildown_type = "mariah", euclid_type = "cubic", additive = "afvArea")
m_m_g <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", taildown_type = "mariah", euclid_type = "gaussian", additive = "afvArea")
m_m_0 <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", taildown_type = "mariah", additive = "afvArea")
#--
# Create new row 1 
row_m_m_j <- data.frame(model = "MMJ", AIC = glance(m_m_j)$AIC, AICc = glance(m_m_j)$AICc, pseudo_r2 = glance(m_m_j)$pseudo.r.squared, tailup_prop = varcomp(m_m_j)$proportion[2], taildown_prop = varcomp(m_m_j)$proportion[3], euclid_prop = varcomp(m_m_j)$proportion[4], nugget_prop = varcomp(m_m_j)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_m_j)
cat("Completed model: ", "MMJ", "\n")
# Create new row 2 
row_m_m_c <- data.frame(model = "MMC", AIC = glance(m_m_c)$AIC, AICc = glance(m_m_c)$AICc, pseudo_r2 = glance(m_m_c)$pseudo.r.squared, tailup_prop = varcomp(m_m_c)$proportion[2], taildown_prop = varcomp(m_m_c)$proportion[3], euclid_prop = varcomp(m_m_c)$proportion[4], nugget_prop = varcomp(m_m_c)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_m_c)
cat("Completed model: ", "MMC", "\n")
# Create new row 3
row_m_m_g <- data.frame(model = "MMG", AIC = glance(m_m_g)$AIC, AICc = glance(m_m_g)$AICc, pseudo_r2 = glance(m_m_g)$pseudo.r.squared, tailup_prop = varcomp(m_m_g)$proportion[2], taildown_prop = varcomp(m_m_g)$proportion[3], euclid_prop = varcomp(m_m_g)$proportion[4], nugget_prop = varcomp(m_m_g)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_m_g)
cat("Completed model: ", "MMG", "\n")
# Create new row 4 
row_m_m_0 <- data.frame(model = "MM0", AIC = glance(m_m_0)$AIC, AICc = glance(m_m_0)$AICc, pseudo_r2 = glance(m_m_0)$pseudo.r.squared, tailup_prop = varcomp(m_m_0)$proportion[2], taildown_prop = varcomp(m_m_0)$proportion[3], euclid_prop = varcomp(m_m_0)$proportion[4], nugget_prop = varcomp(m_m_0)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_m_0)
cat("Completed model: ", "MM0", "\n")
#---
m_e_j <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", taildown_type = "exponential", euclid_type = "jbessel", additive = "afvArea")
m_e_c <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", taildown_type = "exponential", euclid_type = "cubic", additive = "afvArea")
m_e_g <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", taildown_type = "exponential", euclid_type = "gaussian", additive = "afvArea")
m_e_0 <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", taildown_type = "exponential", additive = "afvArea")
#---
# Create new row 1 
row_m_e_j <- data.frame(model = "MEJ", AIC = glance(m_e_j)$AIC, AICc = glance(m_e_j)$AICc, pseudo_r2 = glance(m_e_j)$pseudo.r.squared, tailup_prop = varcomp(m_e_j)$proportion[2], taildown_prop = varcomp(m_e_j)$proportion[3], euclid_prop = varcomp(m_e_j)$proportion[4], nugget_prop = varcomp(m_e_j)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_e_j)
cat("Completed model: ", "MEJ", "\n")
# Create new row 2 
row_m_e_c <- data.frame(model = "MEC", AIC = glance(m_e_c)$AIC, AICc = glance(m_e_c)$AICc, pseudo_r2 = glance(m_e_c)$pseudo.r.squared, tailup_prop = varcomp(m_e_c)$proportion[2], taildown_prop = varcomp(m_e_c)$proportion[3], euclid_prop = varcomp(m_e_c)$proportion[4], nugget_prop = varcomp(m_e_c)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_e_c)
cat("Completed model: ", "MEC", "\n")
# Create new row 3
row_m_e_g <- data.frame(model = "MEG", AIC = glance(m_e_g)$AIC, AICc = glance(m_e_g)$AICc, pseudo_r2 = glance(m_e_g)$pseudo.r.squared, tailup_prop = varcomp(m_e_g)$proportion[2], taildown_prop = varcomp(m_e_g)$proportion[3], euclid_prop = varcomp(m_e_g)$proportion[4], nugget_prop = varcomp(m_e_g)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_e_g)
cat("Completed model: ", "MEG", "\n")
# Create new row 4 
row_m_e_0 <- data.frame(model = "ME0", AIC = glance(m_e_0)$AIC, AICc = glance(m_e_0)$AICc, pseudo_r2 = glance(m_e_0)$pseudo.r.squared, tailup_prop = varcomp(m_e_0)$proportion[2], taildown_prop = varcomp(m_e_0)$proportion[3], euclid_prop = varcomp(m_e_0)$proportion[4], nugget_prop = varcomp(m_e_0)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_e_0)
cat("Completed model: ", "ME0", "\n")
#---
m_l_j <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", taildown_type = "linear", euclid_type = "jbessel", additive = "afvArea")
m_l_c <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", taildown_type = "linear", euclid_type = "cubic", additive = "afvArea")
m_l_g <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", taildown_type = "linear", euclid_type = "gaussian", additive = "afvArea")
m_l_0 <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", taildown_type = "linear", additive = "afvArea")
#---
# Create new row 1 
row_m_l_j <- data.frame(model = "MLJ", AIC = glance(m_l_j)$AIC, AICc = glance(m_l_j)$AICc, pseudo_r2 = glance(m_l_j)$pseudo.r.squared, tailup_prop = varcomp(m_l_j)$proportion[2], taildown_prop = varcomp(m_l_j)$proportion[3], euclid_prop = varcomp(m_l_j)$proportion[4], nugget_prop = varcomp(m_l_j)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_l_j)
cat("Completed model: ", "MLJ", "\n")
# Create new row 2 
row_m_l_c <- data.frame(model = "MLC", AIC = glance(m_l_c)$AIC, AICc = glance(m_l_c)$AICc, pseudo_r2 = glance(m_l_c)$pseudo.r.squared, tailup_prop = varcomp(m_l_c)$proportion[2], taildown_prop = varcomp(m_l_c)$proportion[3], euclid_prop = varcomp(m_l_c)$proportion[4], nugget_prop = varcomp(m_l_c)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_l_c)
cat("Completed model: ", "MLC", "\n")
# Create new row 3
row_m_l_g <- data.frame(model = "MLG", AIC = glance(m_l_g)$AIC, AICc = glance(m_l_g)$AICc, pseudo_r2 = glance(m_l_g)$pseudo.r.squared, tailup_prop = varcomp(m_l_g)$proportion[2], taildown_prop = varcomp(m_l_g)$proportion[3], euclid_prop = varcomp(m_l_g)$proportion[4], nugget_prop = varcomp(m_l_g)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_l_g)
cat("Completed model: ", "MLG", "\n")
# Create new row 4 
row_m_l_0 <- data.frame(model = "ML0", AIC = glance(m_l_0)$AIC, AICc = glance(m_l_0)$AICc, pseudo_r2 = glance(m_l_0)$pseudo.r.squared, tailup_prop = varcomp(m_l_0)$proportion[2], taildown_prop = varcomp(m_l_0)$proportion[3], euclid_prop = varcomp(m_l_0)$proportion[4], nugget_prop = varcomp(m_l_0)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_l_0)
cat("Completed model: ", "ML0", "\n")
#---
m_0_0 <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", additive = "afvArea")
m_0_j <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", euclid_type = "jbessel", additive = "afvArea")
m_0_c <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah", euclid_type = "cubic", additive = "afvArea")
m_0_g <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "mariah",euclid_type = "gaussian", additive = "afvArea")
#--- 
# Create new row 1 
row_m_0_j <- data.frame(model = "M0J", AIC = glance(m_0_j)$AIC, AICc = glance(m_0_j)$AICc, pseudo_r2 = glance(m_0_j)$pseudo.r.squared, tailup_prop = varcomp(m_0_j)$proportion[2], taildown_prop = varcomp(m_0_j)$proportion[3], euclid_prop = varcomp(m_0_j)$proportion[4], nugget_prop = varcomp(m_0_j)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_0_j)
cat("Completed model: ", "M0J", "\n")
# Create new row 2 
row_m_0_c <- data.frame(model = "M0C", AIC = glance(m_0_c)$AIC, AICc = glance(m_0_c)$AICc, pseudo_r2 = glance(m_0_c)$pseudo.r.squared, tailup_prop = varcomp(m_0_c)$proportion[2], taildown_prop = varcomp(m_0_c)$proportion[3], euclid_prop = varcomp(m_0_c)$proportion[4], nugget_prop = varcomp(m_0_c)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_0_c)
cat("Completed model: ", "M0C", "\n")
# Create new row 3
row_m_0_g <- data.frame(model = "M0G", AIC = glance(m_0_g)$AIC, AICc = glance(m_0_g)$AICc, pseudo_r2 = glance(m_0_g)$pseudo.r.squared, tailup_prop = varcomp(m_0_g)$proportion[2], taildown_prop = varcomp(m_0_g)$proportion[3], euclid_prop = varcomp(m_0_g)$proportion[4], nugget_prop = varcomp(m_0_g)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_0_g)
cat("Completed model: ", "M0G", "\n")
# Create new row 4 
row_m_0_0 <- data.frame(model = "M00", AIC = glance(m_0_0)$AIC, AICc = glance(m_0_0)$AICc, pseudo_r2 = glance(m_0_0)$pseudo.r.squared, tailup_prop = varcomp(m_0_0)$proportion[2], taildown_prop = varcomp(m_0_0)$proportion[3], euclid_prop = varcomp(m_0_0)$proportion[4], nugget_prop = varcomp(m_0_0)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_m_0_0)
cat("Completed model: ", "M00", "\n")
#------------------------------------------------------
# Tail-Up: Linear
l_m_j <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", taildown_type = "mariah", euclid_type = "jbessel", additive = "afvArea")
l_m_c <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", taildown_type = "mariah", euclid_type = "cubic", additive = "afvArea")
l_m_g <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", taildown_type = "mariah", euclid_type = "gaussian", additive = "afvArea")
l_m_0 <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", taildown_type = "mariah", additive = "afvArea")
#---
# Create new row 1 
row_l_m_j <- data.frame(model = "LMJ", AIC = glance(l_m_j)$AIC, AICc = glance(l_m_j)$AICc, pseudo_r2 = glance(l_m_j)$pseudo.r.squared, tailup_prop = varcomp(l_m_j)$proportion[2], taildown_prop = varcomp(l_m_j)$proportion[3], euclid_prop = varcomp(l_m_j)$proportion[4], nugget_prop = varcomp(l_m_j)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_m_j)
cat("Completed model: ", "LMJ", "\n")
# Create new row 2 
row_l_m_c <- data.frame(model = "LMC", AIC = glance(l_m_c)$AIC, AICc = glance(l_m_c)$AICc, pseudo_r2 = glance(l_m_c)$pseudo.r.squared, tailup_prop = varcomp(l_m_c)$proportion[2], taildown_prop = varcomp(l_m_c)$proportion[3], euclid_prop = varcomp(l_m_c)$proportion[4], nugget_prop = varcomp(l_m_c)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_m_c)
cat("Completed model: ", "LMC", "\n")
# Create new row 3
row_l_m_g <- data.frame(model = "LMG", AIC = glance(l_m_g)$AIC, AICc = glance(l_m_g)$AICc, pseudo_r2 = glance(l_m_g)$pseudo.r.squared, tailup_prop = varcomp(l_m_g)$proportion[2], taildown_prop = varcomp(l_m_g)$proportion[3], euclid_prop = varcomp(l_m_g)$proportion[4], nugget_prop = varcomp(l_m_g)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_m_g)
cat("Completed model: ", "LMG", "\n")
# Create new row 4 
row_l_m_0 <- data.frame(model = "LM0", AIC = glance(l_m_0)$AIC, AICc = glance(l_m_0)$AICc, pseudo_r2 = glance(l_m_0)$pseudo.r.squared, tailup_prop = varcomp(l_m_0)$proportion[2], taildown_prop = varcomp(l_m_0)$proportion[3], euclid_prop = varcomp(l_m_0)$proportion[4], nugget_prop = varcomp(l_m_0)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_m_0)
cat("Completed model: ", "LM0", "\n")
#---
l_e_j <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", taildown_type = "exponential", euclid_type = "jbessel", additive = "afvArea")
l_e_c <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", taildown_type = "exponential", euclid_type = "cubic", additive = "afvArea")
l_e_g <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", taildown_type = "exponential", euclid_type = "gaussian", additive = "afvArea")
l_e_0 <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", taildown_type = "exponential", additive = "afvArea")
#---
# Create new row 1 
row_l_e_j <- data.frame(model = "LEJ", AIC = glance(l_e_j)$AIC, AICc = glance(l_e_j)$AICc, pseudo_r2 = glance(l_e_j)$pseudo.r.squared, tailup_prop = varcomp(l_e_j)$proportion[2], taildown_prop = varcomp(l_e_j)$proportion[3], euclid_prop = varcomp(l_e_j)$proportion[4], nugget_prop = varcomp(l_e_j)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_e_j)
cat("Completed model: ", "LEJ", "\n")
# Create new row 2 
row_l_e_c <- data.frame(model = "LEC", AIC = glance(l_e_c)$AIC, AICc = glance(l_e_c)$AICc, pseudo_r2 = glance(l_e_c)$pseudo.r.squared, tailup_prop = varcomp(l_e_c)$proportion[2], taildown_prop = varcomp(l_e_c)$proportion[3], euclid_prop = varcomp(l_e_c)$proportion[4], nugget_prop = varcomp(l_e_c)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_e_c)
cat("Completed model: ", "LEC", "\n")
# Create new row 3
row_l_e_g <- data.frame(model = "LEG", AIC = glance(l_e_g)$AIC, AICc = glance(l_e_g)$AICc, pseudo_r2 = glance(l_e_g)$pseudo.r.squared, tailup_prop = varcomp(l_e_g)$proportion[2], taildown_prop = varcomp(l_e_g)$proportion[3], euclid_prop = varcomp(l_e_g)$proportion[4], nugget_prop = varcomp(l_e_g)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_e_g)
cat("Completed model: ", "LEG", "\n")
# Create new row 4 
row_l_e_0 <- data.frame(model = "LE0", AIC = glance(l_e_0)$AIC, AICc = glance(l_e_0)$AICc, pseudo_r2 = glance(l_e_0)$pseudo.r.squared, tailup_prop = varcomp(l_e_0)$proportion[2], taildown_prop = varcomp(l_e_0)$proportion[3], euclid_prop = varcomp(l_e_0)$proportion[4], nugget_prop = varcomp(l_e_0)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_e_0)
cat("Completed model: ", "LE0", "\n")
#---
l_l_j <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", taildown_type = "linear", euclid_type = "jbessel", additive = "afvArea")
l_l_c <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", taildown_type = "linear", euclid_type = "cubic", additive = "afvArea")
l_l_g <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", taildown_type = "linear", euclid_type = "gaussian", additive = "afvArea")
l_l_0 <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", taildown_type = "linear", additive = "afvArea")
#---
# Create new row 1 
row_l_l_j <- data.frame(model = "LLJ", AIC = glance(l_l_j)$AIC, AICc = glance(l_l_j)$AICc, pseudo_r2 = glance(l_l_j)$pseudo.r.squared, tailup_prop = varcomp(l_l_j)$proportion[2], taildown_prop = varcomp(l_l_j)$proportion[3], euclid_prop = varcomp(l_l_j)$proportion[4], nugget_prop = varcomp(l_l_j)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_l_j)
cat("Completed model: ", "LLJ", "\n")
# Create new row 2 
row_l_l_c <- data.frame(model = "LLC", AIC = glance(l_l_c)$AIC, AICc = glance(l_l_c)$AICc, pseudo_r2 = glance(l_l_c)$pseudo.r.squared, tailup_prop = varcomp(l_l_c)$proportion[2], taildown_prop = varcomp(l_l_c)$proportion[3], euclid_prop = varcomp(l_l_c)$proportion[4], nugget_prop = varcomp(l_l_c)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_l_c)
cat("Completed model: ", "LLC", "\n")
# Create new row 3
row_l_l_g <- data.frame(model = "LLG", AIC = glance(l_l_g)$AIC, AICc = glance(l_l_g)$AICc, pseudo_r2 = glance(l_l_g)$pseudo.r.squared, tailup_prop = varcomp(l_l_g)$proportion[2], taildown_prop = varcomp(l_l_g)$proportion[3], euclid_prop = varcomp(l_l_g)$proportion[4], nugget_prop = varcomp(l_l_g)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_l_g)
cat("Completed model: ", "LLG", "\n")
# Create new row 4 
row_l_l_0 <- data.frame(model = "LL0", AIC = glance(l_l_0)$AIC, AICc = glance(l_l_0)$AICc, pseudo_r2 = glance(l_l_0)$pseudo.r.squared, tailup_prop = varcomp(l_l_0)$proportion[2], taildown_prop = varcomp(l_l_0)$proportion[3], euclid_prop = varcomp(l_l_0)$proportion[4], nugget_prop = varcomp(l_l_0)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_l_0)
cat("Completed model: ", "LL0", "\n")
#---
l_0_0 <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", additive = "afvArea")
l_0_j <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", euclid_type = "jbessel", additive = "afvArea")
l_0_c <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", euclid_type = "cubic", additive = "afvArea")
l_0_g <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "linear", euclid_type = "gaussian", additive = "afvArea")
#---
# Create new row 1 
row_l_0_j <- data.frame(model = "L0J", AIC = glance(l_0_j)$AIC, AICc = glance(l_0_j)$AICc, pseudo_r2 = glance(l_0_j)$pseudo.r.squared, tailup_prop = varcomp(l_0_j)$proportion[2], taildown_prop = varcomp(l_0_j)$proportion[3], euclid_prop = varcomp(l_0_j)$proportion[4], nugget_prop = varcomp(l_0_j)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_0_j)
cat("Completed model: ", "L0J", "\n")
# Create new row 2 
row_l_0_c <- data.frame(model = "L0C", AIC = glance(l_0_c)$AIC, AICc = glance(l_0_c)$AICc, pseudo_r2 = glance(l_0_c)$pseudo.r.squared, tailup_prop = varcomp(l_0_c)$proportion[2], taildown_prop = varcomp(l_0_c)$proportion[3], euclid_prop = varcomp(l_0_c)$proportion[4], nugget_prop = varcomp(l_0_c)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_0_c)
cat("Completed model: ", "L0C", "\n")
# Create new row 3
row_l_0_g <- data.frame(model = "L0G", AIC = glance(l_0_g)$AIC, AICc = glance(l_0_g)$AICc, pseudo_r2 = glance(l_0_g)$pseudo.r.squared, tailup_prop = varcomp(l_0_g)$proportion[2], taildown_prop = varcomp(l_0_g)$proportion[3], euclid_prop = varcomp(l_0_g)$proportion[4], nugget_prop = varcomp(l_0_g)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_0_g)
cat("Completed model: ", "L0G", "\n")
# Create new row 4 
row_l_0_0 <- data.frame(model = "L00", AIC = glance(l_0_0)$AIC, AICc = glance(l_0_0)$AICc, pseudo_r2 = glance(l_0_0)$pseudo.r.squared, tailup_prop = varcomp(l_0_0)$proportion[2], taildown_prop = varcomp(l_0_0)$proportion[3], euclid_prop = varcomp(l_0_0)$proportion[4], nugget_prop = varcomp(l_0_0)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_l_0_0)
cat("Completed model: ", "L00", "\n")
#------------------------------------------------------
# Tail-Up: Epa
e_m_j <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", taildown_type = "mariah", euclid_type = "jbessel", additive = "afvArea")
e_m_c <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", taildown_type = "mariah", euclid_type = "cubic", additive = "afvArea")
e_m_g <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", taildown_type = "mariah", euclid_type = "gaussian", additive = "afvArea")
e_m_0 <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", taildown_type = "mariah", additive = "afvArea")
#---
# Create new row 1 
row_e_m_j <- data.frame(model = "EMJ", AIC = glance(e_m_j)$AIC, AICc = glance(e_m_j)$AICc, pseudo_r2 = glance(e_m_j)$pseudo.r.squared, tailup_prop = varcomp(e_m_j)$proportion[2], taildown_prop = varcomp(e_m_j)$proportion[3], euclid_prop = varcomp(e_m_j)$proportion[4], nugget_prop = varcomp(e_m_j)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_m_j)
cat("Completed model: ", "EMJ", "\n")
# Create new row 2 
row_e_m_c <- data.frame(model = "EMC", AIC = glance(e_m_c)$AIC, AICc = glance(e_m_c)$AICc, pseudo_r2 = glance(e_m_c)$pseudo.r.squared, tailup_prop = varcomp(e_m_c)$proportion[2], taildown_prop = varcomp(e_m_c)$proportion[3], euclid_prop = varcomp(e_m_c)$proportion[4], nugget_prop = varcomp(e_m_c)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_m_c)
cat("Completed model: ", "EMC", "\n")
# Create new row 3
row_e_m_g <- data.frame(model = "EMG", AIC = glance(e_m_g)$AIC, AICc = glance(e_m_g)$AICc, pseudo_r2 = glance(e_m_g)$pseudo.r.squared, tailup_prop = varcomp(e_m_g)$proportion[2], taildown_prop = varcomp(e_m_g)$proportion[3], euclid_prop = varcomp(e_m_g)$proportion[4], nugget_prop = varcomp(e_m_g)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_m_g)
cat("Completed model: ", "EMG", "\n")
# Create new row 4 
row_e_m_0 <- data.frame(model = "EM0", AIC = glance(e_m_0)$AIC, AICc = glance(e_m_0)$AICc, pseudo_r2 = glance(e_m_0)$pseudo.r.squared, tailup_prop = varcomp(e_m_0)$proportion[2], taildown_prop = varcomp(e_m_0)$proportion[3], euclid_prop = varcomp(e_m_0)$proportion[4], nugget_prop = varcomp(e_m_0)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_m_0)
cat("Completed model: ", "EM0", "\n")
#---
e_e_j <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", taildown_type = "exponential", euclid_type = "jbessel", additive = "afvArea")
e_e_c <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", taildown_type = "exponential", euclid_type = "cubic", additive = "afvArea")
e_e_g <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", taildown_type = "exponential", euclid_type = "gaussian", additive = "afvArea")
e_e_0 <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", taildown_type = "exponential", additive = "afvArea")
#---
# Create new row 1 
row_e_e_j <- data.frame(model = "EEJ", AIC = glance(e_e_j)$AIC, AICc = glance(e_e_j)$AICc, pseudo_r2 = glance(e_e_j)$pseudo.r.squared, tailup_prop = varcomp(e_e_j)$proportion[2], taildown_prop = varcomp(e_e_j)$proportion[3], euclid_prop = varcomp(e_e_j)$proportion[4], nugget_prop = varcomp(e_e_j)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_e_j)
cat("Completed model: ", "EEJ", "\n")
# Create new row 2 
row_e_e_c <- data.frame(model = "EEC", AIC = glance(e_e_c)$AIC, AICc = glance(e_e_c)$AICc, pseudo_r2 = glance(e_e_c)$pseudo.r.squared, tailup_prop = varcomp(e_e_c)$proportion[2], taildown_prop = varcomp(e_e_c)$proportion[3], euclid_prop = varcomp(e_e_c)$proportion[4], nugget_prop = varcomp(e_e_c)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_e_c)
cat("Completed model: ", "EEC", "\n")
# Create new row 3
row_e_e_g <- data.frame(model = "EEG", AIC = glance(e_e_g)$AIC, AICc = glance(e_e_g)$AICc, pseudo_r2 = glance(e_e_g)$pseudo.r.squared, tailup_prop = varcomp(e_e_g)$proportion[2], taildown_prop = varcomp(e_e_g)$proportion[3], euclid_prop = varcomp(e_e_g)$proportion[4], nugget_prop = varcomp(e_e_g)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_e_g)
cat("Completed model: ", "EEG", "\n")
# Create new row 4 
row_e_e_0 <- data.frame(model = "EE0", AIC = glance(e_e_0)$AIC, AICc = glance(e_e_0)$AICc, pseudo_r2 = glance(e_e_0)$pseudo.r.squared, tailup_prop = varcomp(e_e_0)$proportion[2], taildown_prop = varcomp(e_e_0)$proportion[3], euclid_prop = varcomp(e_e_0)$proportion[4], nugget_prop = varcomp(e_e_0)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_e_0)
cat("Completed model: ", "EE0", "\n")
#---
e_l_j <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", taildown_type = "linear", euclid_type = "jbessel", additive = "afvArea")
e_l_c <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", taildown_type = "linear", euclid_type = "cubic", additive = "afvArea")
e_l_g <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", taildown_type = "linear", euclid_type = "gaussian", additive = "afvArea")
e_l_0 <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", taildown_type = "linear", additive = "afvArea")
#---
# Create new row 1 
row_e_l_j <- data.frame(model = "ELJ", AIC = glance(e_l_j)$AIC, AICc = glance(e_l_j)$AICc, pseudo_r2 = glance(e_l_j)$pseudo.r.squared, tailup_prop = varcomp(e_l_j)$proportion[2], taildown_prop = varcomp(e_l_j)$proportion[3], euclid_prop = varcomp(e_l_j)$proportion[4], nugget_prop = varcomp(e_l_j)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_l_j)
cat("Completed model: ", "ELJ", "\n")
# Create new row 2 
row_e_l_c <- data.frame(model = "ELC", AIC = glance(e_l_c)$AIC, AICc = glance(e_l_c)$AICc, pseudo_r2 = glance(e_l_c)$pseudo.r.squared, tailup_prop = varcomp(e_l_c)$proportion[2], taildown_prop = varcomp(e_l_c)$proportion[3], euclid_prop = varcomp(e_l_c)$proportion[4], nugget_prop = varcomp(e_l_c)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_l_c)
cat("Completed model: ", "ELC", "\n")
# Create new row 3
row_e_l_g <- data.frame(model = "ELG", AIC = glance(e_l_g)$AIC, AICc = glance(e_l_g)$AICc, pseudo_r2 = glance(e_l_g)$pseudo.r.squared, tailup_prop = varcomp(e_l_g)$proportion[2], taildown_prop = varcomp(e_l_g)$proportion[3], euclid_prop = varcomp(e_l_g)$proportion[4], nugget_prop = varcomp(e_l_g)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_l_g)
cat("Completed model: ", "ELG", "\n")
# Create new row 4 
row_e_l_0 <- data.frame(model = "EL0", AIC = glance(e_l_0)$AIC, AICc = glance(e_l_0)$AICc, pseudo_r2 = glance(e_l_0)$pseudo.r.squared, tailup_prop = varcomp(e_l_0)$proportion[2], taildown_prop = varcomp(e_l_0)$proportion[3], euclid_prop = varcomp(e_l_0)$proportion[4], nugget_prop = varcomp(e_l_0)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_l_0)
cat("Completed model: ", "EL0", "\n")
#---
e_0_0 <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", additive = "afvArea")
e_0_j <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", euclid_type = "jbessel", additive = "afvArea")
e_0_c <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", euclid_type = "cubic", additive = "afvArea")
e_0_g <- ssn_lm(formula = thermalSensitivity ~ SLOPE.y + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA, ssn.object = CRB_SSN, tailup_type = "epa", euclid_type = "gaussian", additive = "afvArea")
#---
# Create new row 1 
row_e_0_j <- data.frame(model = "E0J", AIC = glance(e_0_j)$AIC, AICc = glance(e_0_j)$AICc, pseudo_r2 = glance(e_0_j)$pseudo.r.squared, tailup_prop = varcomp(e_0_j)$proportion[2], taildown_prop = varcomp(e_0_j)$proportion[3], euclid_prop = varcomp(e_0_j)$proportion[4], nugget_prop = varcomp(e_0_j)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_0_j)
cat("Completed model: ", "E0J", "\n")
# Create new row 2 
row_e_0_c <- data.frame(model = "E0C", AIC = glance(e_0_c)$AIC, AICc = glance(e_0_c)$AICc, pseudo_r2 = glance(e_0_c)$pseudo.r.squared, tailup_prop = varcomp(e_0_c)$proportion[2], taildown_prop = varcomp(e_0_c)$proportion[3], euclid_prop = varcomp(e_0_c)$proportion[4], nugget_prop = varcomp(e_0_c)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_0_c)
cat("Completed model: ", "E0C", "\n")
# Create new row 3
row_e_0_g <- data.frame(model = "E0G", AIC = glance(e_0_g)$AIC, AICc = glance(e_0_g)$AICc, pseudo_r2 = glance(e_0_g)$pseudo.r.squared, tailup_prop = varcomp(e_0_g)$proportion[2], taildown_prop = varcomp(e_0_g)$proportion[3], euclid_prop = varcomp(e_0_g)$proportion[4], nugget_prop = varcomp(e_0_g)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_0_g)
cat("Completed model: ", "E0G", "\n")
# Create new row 4 
row_e_0_0 <- data.frame(model = "E00", AIC = glance(e_0_0)$AIC, AICc = glance(e_0_0)$AICc, pseudo_r2 = glance(e_0_0)$pseudo.r.squared, tailup_prop = varcomp(e_0_0)$proportion[2], taildown_prop = varcomp(e_0_0)$proportion[3], euclid_prop = varcomp(e_0_0)$proportion[4], nugget_prop = varcomp(e_0_0)$proportion[5])
# Add to comparison summary
model_comparison <- rbind(model_comparison, row_e_0_0)
cat("Completed model: ", "E00", "\n")
#---
model_comparison$model
# Order by lowest AIC then highest pseudo R squared
model_comparison <- model_comparison[order(model_comparison$AIC, -model_comparison$pseudo_r2), ]
# Reset row numbers
rownames(model_comparison) <- NULL
View(model_comparison)

# Save model_comparison
write.csv(model_comparison, "results/2021/SSN/model_comparison.csv")
saveRDS(model_comparison, "results/2021/RDS/model_comparison.RDS")
#-----------------------------------------------------------------------------------------------------------------------------------------------
# Tail-up: linear, spherical, exponential, mariah, epa OR none
# Tail-down: linear, spherical, exponential, mariah, epa OR none
# Euclidean: spherical, exponential, gaussian, cosine, cubic, pentaspherical, wave, jbessel, gravity, rquad, magnetic OR none
# Nugget: nugget OR none


# summary(model)
# Pseudo R squared returned specifying proportion of variability explained by fixed effects
# Larger the de parameter = the MORE variability in the process is attributed to relevant effect
# Larger the range parameter = the MORE autocorrelated nearby observations are w/ respect to relevant effect

# varcomp(model)
# Directly compare sources of variability in model 
## Covariates (PR-sq) = fixed effects of covariates on RV

# tidy(ssnModel, conf.int = TRUE) // Tidy & add confidence intervals
# glance(ssnModel) // Glance at model-fit statistics
# glances(ssnModel1, ssnModel2) // glance at multiple models
# Lower AIC/AICc = BETTER fit to data
## Able to compare AIC/AICc since ONLY changing covariance structure
# additive ONLY required when tail-up covariance is specified

# loocv(ssnModel) // compare model fits using leave-one-out CV