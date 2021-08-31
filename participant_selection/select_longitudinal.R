" Apply longitudinal exclusion criteria to wave 3 TILDA data
[Run following 'select_cross_sectional.R']
Selects participants with cognitive measures available across several waves

Research Question: What factors of cognitive function influence SIFI performance?

For internal TILDA researchers: please ensure that you are first connected to the TILDA drives and
ensure that the paths to requested datasets are correct for your configuration and operating system. 

For external researchers, prior to use please request access to the TILDA dataset here 
https://tilda.tcd.ie/data/accessing-data/ note that the SIFI data has not yet been integrated into
the public release dataset so you may need to contact a TILDA data administrator for full data access.

Script info: This script applies exclusion criteria to the cross-sectional data gathered at the 
same TILDA wave at the SIFI (Wave 3) and characterises the cognitive data available. 

Author: Rebecca J Hirst (Hirstr@tcd.ie)
Last Edited: 04/05/2021

"
# Start new set of lists to keep track of how many participants are available with longitudinal data 
my_incl_counts_long<-c()
my_incl_labels_long<-c()
my_excl_labels_long<-c()

#### import packages ####
library(tidyverse)
require(foreign) # for data imports (e.g. 'read.dta')
library(PRISMAstatement) # to automatically populate exclusion chart
library(dplyr) # to select relevant columns for data frame
library(tidyr) # to tidy up data frame (e.g. 'drop_na')
library(gridExtra) # to create multipanel figures in a 'grid'
require(ggplot2) # for plots
library(stringr)# to manipulate string variables
library(lme4)# for fitting mixed effects models
library(ggeffects) # for plotting mixed effects results
library(readstata13) # to read in state13 .dta file (wave5)
library(plyr) # to for summary function

#### Handy functions ####

# To dodge individual point positions along the x axis
position_dodge2(
  width = NULL,
  preserve = c("total", "single"),
  padding = 0.1,
  reverse = FALSE
)

#### Label wave 3 variables ####
tilda_dataW3<-tilda_data
colnames(tilda_dataW3) <- paste(colnames(tilda_dataW3), "W3", sep = "_")
tilda_dataW3<-tilda_dataW3%>% 
  dplyr::rename(
    tilda_serial = tilda_serial_W3
  )

#### import other waves ####

wave1 <- read.dta("/Volumes/releases/Wave 1/tildav1-8-0.dta")
wave2 <- read.dta("/Volumes/releases/Wave 2/tildav2-5-0.dta")
wave4 <- read.dta("/Volumes/releases/Wave 4/Datasets/tildav4-6-0.dta")
wave5 <- read.dta13("/Volumes/releases/Wave 5/Datasets/tildav5-6-1.dta")


# Retain original dataframes for reference
wave1_orig<-wave1
wave2_orig<-wave2
wave4_orig<-wave4
wave5_orig<-wave5

#### Select variables from each wave ####
# Note: cannot use loop because different waves have different names

# ph125 is called animal naming in other waves
wave1<-wave1%>% 
  select(tilda_serial, age,age3, sex,
         MOCASum, mmseTest, 
         edu3,ph102,ph108, ph125,
         CRTmeancog, CRTmeanmot, CRTsdcog, CRTsdmot,
         COGsartCookedMean,COGsartOmmissions, COGsartErrors3,
         COGtrail1time, COGtrail2time,
         COGimmediaterecall1, COGimmediaterecall2, COGdelayedrecall,
         ph114, COGprosmem1, COGprosmem2, ph107_1, ph107_2, ph107_3)

# Create immediate recall score
wave1$immediaterecall_total<-wave1$COGimmediaterecall1+wave1$COGimmediaterecall2

# Append column headers with W1 and rename variable names to be consistent with other waves
wave1W1<-wave1
colnames(wave1W1) <- paste(colnames(wave1W1), "W1", sep = "_")

wave1W1<-wave1W1%>% 
  dplyr::rename(
    tilda_serial = tilda_serial_W1,
    COGmoca_W1 = MOCASum_W1,
    COGmmse_W1 = mmseTest_W1,
    COGanimal_naming_W1 = ph125_W1
  )

# Select variable names from wave 2
wave2<-wave2%>% 
  select(tilda_serial, age,age3, sex,
         COGmmse, ph125,
         edu3,ph102,ph108,
         COGimmediaterecall1, COGimmediaterecall2, COGdelayedrecall,
         ph114, COGprosmem1, COGprosmem2, ph107_1, ph107_2, ph107_3)

# Create immediate recall score
wave2$immediaterecall_total<-wave2$COGimmediaterecall1+wave2$COGimmediaterecall2

# Append column headers with W2 and rename variable names to be consistent with other waves
wave2W2<-wave2
colnames(wave2W2) <- paste(colnames(wave2W2), "W2", sep = "_")
wave2W2<-wave2W2%>% 
  dplyr::rename(
    tilda_serial = tilda_serial_W2,
    COGanimal_naming_W2 = ph125_W2
  )

# Select variable names from wave 4
wave4<-wave4%>% 
  select(tilda_serial, age,age3, sex,
         COGmmse, COGanimal_naming,
         edu3,ph102,ph108,
         COGimmediaterecall1, COGimmediaterecall2, COGdelayedrecall,
         ph114, COGprosmem1, COGprosmem2, ph107_01, ph107_02, ph107_03)

# Create immediate recall score
wave4$immediaterecall_total<-wave4$COGimmediaterecall1+wave4$COGimmediaterecall2

# Append column headers with W4
wave4W4<-wave4
colnames(wave4W4) <- paste(colnames(wave4W4), "W4", sep = "_")
wave4W4<-wave4W4%>% 
  dplyr::rename(
    tilda_serial = tilda_serial_W4
  )

# Select variable names from wave 5
wave5<-wave5%>% 
  select(tilda_serial, age,age3, sex,
         COGmmse, COGanimal_naming,
         edu3,ph102,ph108a,
         COGimmediaterecall1, COGimmediaterecall2, COGdelayedrecall,
         ph114, COGprosmem1, COGprosmem2, ph107_01, ph107_02)

# Create immediate recall score
wave5$immediaterecall_total<-wave5$COGimmediaterecall1+wave5$COGimmediaterecall2

# Append column headers with W5
wave5W5<-wave5
colnames(wave5W5) <- paste(colnames(wave5W5), "W5", sep = "_")
wave5W5<-wave5W5%>% 
  dplyr::rename(
    tilda_serial = tilda_serial_W5
  )

#### Merge datasets ####

# Start N # start label will be "total available from wave 3"
my_incl_counts_long<-c(my_incl_counts_long, nrow(tilda_dataW3))

# Merge
tilda_dataW3W1<-merge(tilda_dataW3, wave1W1, by='tilda_serial')

# Track counts for exclusion plot
my_incl_counts_long<-c(my_incl_counts_long, nrow(tilda_dataW3W1))
my_incl_labels_long<-c(my_incl_labels_long, 'available with wave 1 and wave 3')
my_excl_labels_long<-c(my_excl_labels_long, 'no wave 1')

# Merge
tilda_dataW3W1W2<-merge(tilda_dataW3W1, wave2W2, by='tilda_serial')

# Track counts for exclusion plot
my_incl_counts_long<-c(my_incl_counts_long, nrow(tilda_dataW3W1W2))
my_incl_labels_long<-c(my_incl_labels_long, 'available with wave 1, 2 and wave 3')
my_excl_labels_long<-c(my_excl_labels_long, 'no wave 2')

# Merge
tilda_dataW3W1W2W4<-merge(tilda_dataW3W1W2, wave4W4, by='tilda_serial')

# Track counts for exclusion plot
my_incl_counts_long<-c(my_incl_counts_long, nrow(tilda_dataW3W1W2W4))
my_incl_labels_long<-c(my_incl_labels_long, 'available with wave 1, 2, 4 and wave 3')
my_excl_labels_long<-c(my_excl_labels_long, 'no wave 4')

# Merge
tilda_dataW3W1W2W4W5<-merge(tilda_dataW3W1W2W4, wave5W5, by='tilda_serial')

# Track counts for exclusion plot
my_incl_counts_long<-c(my_incl_counts_long, nrow(tilda_dataW3W1W2W4W5))
my_incl_labels_long<-c(my_incl_labels_long, 'available with wave 1, 2, 4, 5 and wave 3')
my_excl_labels_long<-c(my_excl_labels_long, 'no wave 5')

# Retain original dataframe as variabls
tilda_dataW3W1W2W4W5_orig<-tilda_dataW3W1W2W4W5

# Plot exclusion criteria
myChart<- flow_exclusions(
  incl_counts = my_incl_counts_long,
  total_label = "Available at wave 3 following exclusion criteria",
  incl_labels = my_incl_labels_long,
  excl_labels = my_excl_labels_long,
  percent_of_prev = TRUE
)

# Missing/na for immediate recall at W1 - W5 (i.e. field contains 'na')
tilda_dataW3W1W2W4W5<-tilda_dataW3W1W2W4W5%>% 
  drop_na(immediaterecall_total_W1, immediaterecall_total_W2, immediaterecall_total_W3, immediaterecall_total_W4, immediaterecall_total_W5)
my_incl_counts_long<-c(my_incl_counts_long, nrow(tilda_dataW3W1W2W4W5))
my_incl_labels_long<-c(my_incl_labels_long, 'has immediate recall at all waves')
my_excl_labels_long<-c(my_excl_labels_long, 'missing immediate recall at a wave')

# Missing/na for delayed recall at W1 - W5 (i.e. field contains 'na')
tilda_dataW3W1W2W4W5<-tilda_dataW3W1W2W4W5%>% 
  drop_na(COGdelayedrecall_W1, COGdelayedrecall_W2, COGdelayedrecall_W3, COGdelayedrecall_W4, COGdelayedrecall_W5)
my_incl_counts_long<-c(my_incl_counts_long, nrow(tilda_dataW3W1W2W4W5))
my_incl_labels_long<-c(my_incl_labels_long, 'has delayed recall at all waves')
my_excl_labels_long<-c(my_excl_labels_long, 'missing delayed recall at a wave')

# Missing/na for animal naming at W1 - W5 (i.e. field contains 'na')
tilda_dataW3W1W2W4W5<-tilda_dataW3W1W2W4W5%>% 
  drop_na(COGanimal_naming_W1,COGanimal_naming_W2, COGanimal_naming_W3, COGanimal_naming_W4, COGanimal_naming_W5)
my_incl_counts_long<-c(my_incl_counts_long, nrow(tilda_dataW3W1W2W4W5))
my_incl_labels_long<-c(my_incl_labels_long, 'has animal naming at all waves')
my_excl_labels_long<-c(my_excl_labels_long, 'missing animal naming at a wave')

# Plot exclusion criteria
myChart<- flow_exclusions(
  incl_counts = my_incl_counts_long,
  total_label = "Available at wave 3 following exclusion criteria",
  incl_labels = my_incl_labels_long,
  excl_labels = my_excl_labels_long,
  percent_of_prev = TRUE
)
#### Dataframes used only for k means script ####
# These dataframes have only the columns used for longitudinal clustering

immediate_recall_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial, immediaterecall_total_W1,
         immediaterecall_total_W2, immediaterecall_total_W3, immediaterecall_total_W4,
         immediaterecall_total_W5)

animal_naming_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial,
         COGanimal_naming_W1, COGanimal_naming_W2, COGanimal_naming_W3,
         COGanimal_naming_W4, COGanimal_naming_W5)

delayed_recall_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial,
         COGdelayedrecall_W1, COGdelayedrecall_W2, COGdelayedrecall_W3,
         COGdelayedrecall_W4, COGdelayedrecall_W5)
