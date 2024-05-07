## Delta H limits

getwd()
library(ggplot2)
library(dplyr)
library(tidyverse)

load("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Flow_curves_new/Code/functions/root_interpolation_function.Rdata")
## function to find value in curve

## full names for labels
labels <- read.csv("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Flow_curves_new/Data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(hydro.endpoints = Flow.Metric.Code)
labels[25, 1] <- "Peak Flow Magnitude (Q99, cfs)"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow Magnitude"
labels

# ASCI --------------------------------------------------------------------

## upload data
all_asci <- read.csv("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Flow_curves_new/output_data/01_h_asci_neg_pos_logR_metrics_figures_July2023.csv") %>% ## change data here!!! #edited 8/29
  mutate(PredictedProbability = case_when(hydro.endpoints == "d_peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values 
                                          hydro.endpoints == "d_peak_5" & hydro > 0 ~ NA_real_,
                                          hydro.endpoints == "d_peak_2" & hydro > 0 ~ NA_real_,
                                          TRUE ~ as.numeric(PredictedProbability)))


## FIX NAMES TO MATCH LABELS AND LIMITS - Rachel 9/6
all_asci <- all_asci %>% 
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
all_asci <- all_asci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep=""))

all_asci <- left_join(all_asci, labels, by ="hydro.endpoints")

head(all_asci)

# asci_metrics <- c("Q99", "SP_Dur", "DS_Dur_WS")
## added by rachel 8/29
mag_metrics <-c("DS_Mag_50", "FA_Mag", "Peak_10", "Peak_2", "Peak_5", "SP_Mag", 
                "Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "Q99")


## subset to only important metrics
all_asci_sub <- subset(all_asci, hydro.endpoints %in% mag_metrics) # edited 8/29 by rachel 

unique(all_asci_sub$hydro.endpoints)


# CSCI --------------------------------------------------------------------

## upload data
all_csci <- read.csv("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Flow_curves_new/output_data/01_CSCI_neg_pos_logR_metrics_figures_July2023.csv") %>% #edited 8/29
  mutate(PredictedProbability = case_when(hydro.endpoints == "d_peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values 
                                          hydro.endpoints == "d_peak_5" & hydro > 0 ~ NA_real_,
                                          hydro.endpoints == "d_peak_2" & hydro > 0 ~ NA_real_,
                                          TRUE ~ as.numeric(PredictedProbability)))

## FIX NAMES TO MATCH LABELS AND LIMITS - Rachel 9/6
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

# csci_metrics <-c("Q99", "SP_Tim","DS_Dur_WS")
## added by rachel 8/29
# mag_metrics <-c("Q99", "DS_Mag_90", "DS_Mag_50", "SP_Mag", "Wet_BFL_Mag_50", "Wet_BFL_Mag_10", "FA_Mag", "DS_Dur_WS", "SP_Dur", "SP_Tim")
mag_metrics <-c("DS_Mag_50", "FA_Mag", "Peak_10", "Peak_2", "Peak_5", "SP_Mag", 
                "Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "Q99")


## subset to only important metrics
all_csci_sub <- subset(all_csci, hydro.endpoints %in% mag_metrics) #edited by Rachel 8/29



# find roots of curve -----------------------------------------------------

## ASCI

## create df
df <- as.data.frame(matrix(ncol=10))
colnames(df) <- c("metric", "Threshold40", "Threshold50", "Threshold60", "n", "Type", "Biol", "Bio_endpoint", "Bio_threshold", "Hydro_endpoint")


## define metrics
metrics <- unique(all_asci_sub$comb_code_type)
metrics <- metrics[grep("0.86", metrics)]  # edited by Rachel 8/29
metrics

# i = 1
## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i]
  
  hydroxx <- all_asci_sub %>%
    filter(comb_code_type == met)
  
  ## get curves values at different probabilities
  thresh40 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.4) 
  thresh40 <- ifelse(length(thresh40) == 0, NA, thresh40)
  
  thresh50 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.5)
  thresh50 <- ifelse(length(thresh50) == 0, NA, thresh50)
  
  thresh60 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.6) 
  thresh60 <- ifelse(length(thresh60) == 0, NA, thresh60)
  
  ## add info to df
  df[i, 1] <- met
  df[i, 2] <- thresh40
  df[i, 3] <- thresh50
  df[i, 4] <- thresh60
  df[i, 5] <- length(hydroxx$PredictedProbabilityScaled)
  df[i ,6] <- hydroxx$Type[1]
  df[i ,7] <- "ASCI"
  df[i, 8] <- hydroxx$biol.endpoints[1]
  df[i, 9] <- hydroxx$thresholds[1]
  df[i, 10] <- hydroxx$hydro.endpoints[1]
  
  
}


df
write.csv(df, "output_data/01_ASCI_delta_thresholds_scaled_updated.csv")

# CSCI --------------------------------------------------------------------

## create df
df <- as.data.frame(matrix(ncol=10))
colnames(df) <- c("metric", "Threshold40", "Threshold50", "Threshold60", "n", "Type", "Biol", "Bio_endpoint", "Bio_threshold", "Hydro_endpoint")


## define metrics
metrics <- unique(all_csci_sub$comb_code_type)
metrics <- metrics[grep("0.79", metrics)]  # edited by Rachel 8/29
metrics

## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i]
  
  hydroxx <- all_csci_sub %>%
    filter(comb_code_type == met)
  
  ## get curves values at different probabilities
  thresh40 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.4) ### 8/29 changed for new probability thresholds - used to be 0.5
  thresh40 <- ifelse(length(thresh40) == 0, NA, thresh40)
  
  thresh50 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.5) ### 8/29 changed for new probability thresholds - used to be 0.25
  thresh50 <- ifelse(length(thresh50) == 0, NA, thresh50)
  
  thresh60 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.6) ### 8/29 changed for new probability thresholds - used to be 0.75
  thresh60 <- ifelse(length(thresh60) == 0, NA, thresh60)
  
  ## add info to df
  df[i, 1] <- met
  df[i, 2] <- thresh40
  df[i, 3] <- thresh50
  df[i, 4] <- thresh60
  df[i, 5] <- length(hydroxx$PredictedProbabilityScaled)
  df[i ,6] <- hydroxx$Type[1]
  df[i ,7] <- "CSCI"
  df[i, 8] <- hydroxx$biol.endpoints[1]
  df[i, 9] <- hydroxx$thresholds[1]
  df[i, 10] <- hydroxx$hydro.endpoints[1]
  
  
}


df
write.csv(df, "output_data/01_CSCI_delta_thresholds_scaled_updated.csv")


# Combine data ------------------------------------------------------------

asci <- read.csv("output_data/01_ASCI_delta_thresholds_scaled_updated.csv")
csci <- read.csv("output_data/01_CSCI_delta_thresholds_scaled_updated.csv")

delta <- rbind(asci, csci)

write.csv(delta, "output_data/01_ALL_delta_thresholds_scaled_updated.csv")


# Figures CSCI-----------------------------------------------------------------

head(all_csci_sub)

## filter to only 0.79
all_csci <- all_csci_sub %>%
  mutate(Thresholds = as.character(thresholds)) %>%
  filter(thresholds == 0.79) 

## define FFM to loop through
HydroEnds <- unique(all_csci$hydro.endpoints)

HydroEnds
m=2

for(m in 1:length(HydroEnds)) {
  
  ## title of FFM
  main.title <- all_csci %>%
    ungroup() %>%
    filter(hydro.endpoints == paste(HydroEnds[m])) %>%
    select(Flow.Metric.Name) %>%
    distinct(Flow.Metric.Name)
  
  ## subset data and put in order for geom.path
  all_cscix <- subset(all_csci,hydro.endpoints == paste(HydroEnds[m]))
  all_cscix <- all_cscix[order(all_cscix$PredictedProbabilityScaled, all_cscix$hydro),]
  
  # Rachel edited 9/20
  if(paste(HydroEnds[m]) %in% c("d_peak_10", "d_peak_2", "d_peak_5")){
    all_cscix <- all_cscix %>% 
      filter(Type == "Negative")
  }
  
  q3 <- ggplot(all_cscix, aes(x=hydro, y=PredictedProbabilityScaled))+
    geom_path()+
    facet_wrap(~Type, scales = "free_x") +
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) +
    scale_y_continuous(limits=c(0,1))+
    geom_hline(yintercept = 0.4,  linetype="dashed", linewidth=0.5, color = "grey50") +
    geom_hline(yintercept = 0.6,  linetype="dashed", linewidth=0.5, color = "grey50") +
    theme_minimal()+
    theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
    labs(title = paste(main.title),
         x = "Delta H",
         y = "Probability of Good CSCI") #+ theme_bw(base_size = 15)
  q3
  
  out.filename <- paste0("figures/01_csci_", paste(HydroEnds[m]), "_0.79_updated.jpg")
  ggsave(q3, file = out.filename, dpi=300, height=4, width=6, bg = "white")
  
  
}



# Figures ASCI ------------------------------------------------------------

## filter to 0.86
all_asci <- all_asci_sub %>%
  mutate(Thresholds = as.character(thresholds)) %>%
  filter(thresholds == 0.86)

## define FFM to loop through
HydroEnds <- unique(all_asci$hydro.endpoints)
# m = 3

for(m in 1:length(HydroEnds)) {
  
  ## title of FFM
  main.title <- all_asci %>%
    ungroup() %>%
    filter(hydro.endpoints == paste(HydroEnds[m])) %>%
    select(Flow.Metric.Name) %>%
    distinct(Flow.Metric.Name)
  
  ## subset data and put in order for geom.path
  all_ascix <- subset(all_asci,hydro.endpoints == paste(HydroEnds[m]))
  all_ascix <- all_ascix[order(all_ascix$PredictedProbabilityScaled, all_ascix$hydro),]
  
  # Rachel edited 9/20
  if(paste(HydroEnds[m]) %in% c("d_peak_10", "d_peak_2", "d_peak_5")){
    all_ascix <- all_ascix %>% 
      filter(Type == "Negative")
  }
  
  
  q3 <- ggplot(all_ascix, aes(x=hydro, y=PredictedProbabilityScaled))+
    geom_path()+
    facet_wrap(~Type, scales = "free_x") +
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) +
    scale_y_continuous(limits=c(0,1))+
    theme_minimal()+
    theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
    labs(title = paste(main.title),
         x = "Delta H",
         y = "Probability of Good ASCI") #+ theme_bw(base_size = 15)
  q3
  
  out.filename <- paste0("figures/01_asci_", paste(HydroEnds[m]), "_0.86_updated_0920.jpg")
  ggsave(q3, file = out.filename, dpi=300, height=4, width=6, bg = "white")
  
  
}


