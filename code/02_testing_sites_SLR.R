## test sites in San Luis Rey

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidylog)

## workflow

## match comids of test sites to delta h
## predict prob csci at test comids
## compare with ffm ranges and csci

## wd of curve data
## /Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Flow_curves_new

# Upload data -------------------------------------------------------------

## raw data

csci <- read.csv("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Flow_curves_new/output_data/00_csci_delta_formatted_median_updated_RF2023.csv")
head(csci)

csciscores <- csci %>%
  select(masterid, csci, comid, latitude, longitude) %>%
  distinct()

## define metrics used in RB9
csci_metrics <- c("DS_Mag_50", "Peak_10", "FA_Mag", "Wet_BFL_Mag_50") #edited by Rachel 9/7 - chosen for Katies portion of the deliverable

## csci models

bio_h_summary <- read.csv("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Flow_curves_new/output_data/01_csci_hydro_endpoints_order_July2023.csv") %>% 
  mutate(hydro.endpoints = case_when(hydro.endpoints == "d_ds_mag_50" ~ "DS_Mag_50",                 #renamed all mag variables here and below
                                     hydro.endpoints == "d_fa_mag" ~ "FA_Mag",
                                     hydro.endpoints == "d_peak_10" ~ "Peak_10",
                                     hydro.endpoints == "d_peak_2" ~ "Peak_2",
                                     hydro.endpoints == "d_peak_5" ~ "Peak_5",
                                     hydro.endpoints == "d_sp_mag" ~ "SP_Mag",
                                     hydro.endpoints == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     hydro.endpoints == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50", 
                                     hydro.endpoints == "delta_q99" ~ "Q99"))
bio_h_summary

## find index to remove
ind1 <- which(bio_h_summary$biol.endpoints == "CSCI" & bio_h_summary$thresholds == 0.79
              & bio_h_summary$hydro.endpoints %in% csci_metrics)

ind1 ## use only these 

## remove from grid
bio_h_summary <- bio_h_summary[ind1,]

bio_h_summary <- bio_h_summary %>%
  mutate(comb_code = paste0(hydro.endpoints, "_", thresholds))

## upload GLMs and subset
load(file = "/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Flow_curves_new/models/01_CSCI_negative_GLM_all_delta_mets_July2023.RData")
neg.glm <- neg.glm[ind1]

# neg.glm 
load(file = "/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Flow_curves_new/models/01_CSCI_positive_GLM_all_delta_mets_July2023.RData")
pos.glm <- pos.glm[ind1]

## csci/ffm data
all_csci <- read.csv("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Flow_curves_new/output_data/01_CSCI_neg_pos_logR_metrics_figures_July2023.csv") %>% #edited 8/29
  mutate(PredictedProbability = case_when(hydro.endpoints == "d_peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values 
                                          hydro.endpoints == "d_peak_5" & hydro > 0 ~ NA_real_,
                                          hydro.endpoints == "d_peak_2" & hydro > 0 ~ NA_real_,
                                          TRUE ~ as.numeric(PredictedProbability)))

## FIX NAMES TO MATCH LABELS AND LIMITS 
all_csci <- all_csci %>% 
  mutate(hydro.endpoints = case_when(hydro.endpoints == "d_ds_mag_50" ~ "DS_Mag_50",                 #renamed all mag variables here and below
                                     hydro.endpoints == "d_fa_mag" ~ "FA_Mag",
                                     hydro.endpoints == "d_peak_10" ~ "Peak_10",
                                     hydro.endpoints == "d_peak_2" ~ "Peak_2",
                                     hydro.endpoints == "d_peak_5" ~ "Peak_5",
                                     hydro.endpoints == "d_sp_mag" ~ "SP_Mag",
                                     hydro.endpoints == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     hydro.endpoints == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50", 
                                     hydro.endpoints == "delta_q99" ~ "Q99"))

## scale probability
all_csci <- all_csci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep=""))

all_csci <- left_join(all_csci, labels, by ="hydro.endpoints")

head(all_csci)

## subset to only important metrics
all_csci_sub <- subset(all_csci, hydro.endpoints %in% csci_metrics) #edited by Rachel 8/29

## test sites

testSites <- read.csv("input_data/test_sites.csv") %>%
  rename(masterid = test_site)
testSites

## delta
delta <- read.csv("/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/SD Hydro Vulnerability Assessment - General/Data/RawData/Data_for_SDSU/R/data/Output_09/2022-11-29_predicted_abs_FFMq99_deltaFFMq99_SD_COMIDS_medianDelta_test2q99_test12FFM_allgages.csv")
head(delta)

delta <- delta %>%
  mutate(comid = as.character(comid))

## eng data
engCSCI <- read.csv("/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/San Luis Rey Causal and Flow - General/Data/Working/Flow Module Development/data_exploration/Eng_Metric_Final_csci.csv")
head(engCSCI)

## take out duplicates and probs data (not needed at this point)
engCSCI <- engCSCI %>%
  select(masterid, max_csci, comid) %>%
  distinct()

# Match data --------------------------------------------------------------

### get csci score for test sites

## from model data
testCSCIScores <- right_join(csciscores, testSites, by = "masterid")
## 4 sites matched????

## from san luis rey - Eng match
EngCSCIScores <- inner_join(engCSCI, testSites, by = "masterid") 
EngCSCIScores ## 39 

## match with rb9 delta

deltaCSCI <- left_join(EngCSCIScores, delta, by = "comid", relationship = "many-to-many") %>%
  ## FIX NAMES TO MATCH LABELS AND LIMITS 
  mutate(Hydro_endpoint = case_when(metric == "ds_mag_50" ~ "DS_Mag_50",                 #renamed all mag variables here and below
                                     metric == "fa_mag" ~ "FA_Mag",
                                     metric == "peak_10" ~ "Peak_10",
                                     metric == "peak_2" ~ "Peak_2",
                                     metric == "peak_5" ~ "Peak_5",
                                     metric == "sp_mag" ~ "SP_Mag",
                                     metric == "wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     metric == "wet_bfl_mag_50" ~ "Wet_BFL_Mag_50", 
                                     metric == "q99" ~ "Q99")) %>%
  select(-metric)
  
head(deltaCSCI)


# get alteration ----------------------------------------------------------

limits <- read.csv("output_data/01_CSCI_delta_thresholds_scaled_updated.csv")
limits

## join limits with delta data - format with neg and pos as column headings
delta_df <- left_join(deltaCSCI, limits, by = "Hydro_endpoint") %>%
  drop_na(delta_FFM_median_cfs, Hydro_endpoint) %>%
  select(-X, -n, - abs_FFM_median_cfs, - comid.2, -comid_wy, -wayr, -WYT, metric, -metric, -Biol, -Bio_endpoint) %>%
  pivot_longer(Threshold40:Threshold60, names_to = "ProbThreshold", values_to = "Value") %>%
  pivot_wider(names_from = "Type", values_from = "Value")

head(delta_df)

## define alteration per subbasin, per year - within limits
delta_dfx <- delta_df %>%
  # group_by(comid, Hydro.endpoint, Bio_endpoint, Bio_threshold, Threshold) %>%
  mutate(Result = ifelse(delta_FFM_median_cfs <= Positive & delta_FFM_median_cfs >= Negative, "WithinLimits", "OutsideLimits")) %>%  #edited by rachel from "DeltaH" to delta_FFM_median_cfs
  filter(!ProbThreshold == "Threshold50") %>%
  mutate(Result = ifelse(is.na(Result), "WithinLimits", Result))


# Plot --------------------------------------------------------------------


