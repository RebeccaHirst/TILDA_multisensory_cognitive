"Run following 'select_cross_sectional.R'

Selects participants with cognitive measures available across several waves
"
# clear workspace variables
#rm(list = ls())

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

#### handy functions ####

# to dodge individual point positions along the x axis

position_dodge2(
  width = NULL,
  preserve = c("total", "single"),
  padding = 0.1,
  reverse = FALSE
)

#### label wave 3 ####
tilda_dataW3<-tilda_data
colnames(tilda_dataW3) <- paste(colnames(tilda_dataW3), "W3", sep = "_")
tilda_dataW3<-tilda_dataW3%>% 
  dplyr::rename(
    tilda_serial = tilda_serial_W3
  )

#### import other waves ####

# Capi or ha? (looks like only a single file?)
wave1 <- read.dta("/Volumes/releases/Wave 1/tildav1-8-0.dta")

wave2 <- read.dta("/Volumes/releases/Wave 2/tildav2-5-0.dta")

wave4 <- read.dta("/Volumes/releases/Wave 4/Datasets/tildav4-6-0.dta")

wave5 <- read.dta13("/Volumes/releases/Wave 5/Datasets/tildav5-6-1.dta")

wave1_orig<-wave1
wave2_orig<-wave2
wave4_orig<-wave4
wave5_orig<-wave5
#### select variables from each wave ####

# cannot use loop because different waves have different names
# ph125 is the name of animal naming in other waves
wave1<-wave1%>% 
  select(tilda_serial, age,age3, sex,
         MOCASum, mmseTest, 
         edu3,ph102,ph108, ph125,
         CRTmeancog, CRTmeanmot, CRTsdcog, CRTsdmot,
         COGsartCookedMean,COGsartOmmissions, COGsartErrors3,
         COGtrail1time, COGtrail2time,
         COGimmediaterecall1, COGimmediaterecall2, COGdelayedrecall,
         ph114, COGprosmem1, COGprosmem2, ph107_1, ph107_2, ph107_3)

# create immediate recall score
wave1$immediaterecall_total<-wave1$COGimmediaterecall1+wave1$COGimmediaterecall2

# append column headers with W1 and rename variable names to be consistend with other waves

wave1W1<-wave1
colnames(wave1W1) <- paste(colnames(wave1W1), "W1", sep = "_")

wave1W1<-wave1W1%>% 
  dplyr::rename(
    tilda_serial = tilda_serial_W1,
    COGmoca_W1 = MOCASum_W1,
    COGmmse_W1 = mmseTest_W1,
    COGanimal_naming_W1 = ph125_W1
  )

# select variable names from wave 2

wave2<-wave2%>% 
  select(tilda_serial, age,age3, sex,
         COGmmse, ph125,
         edu3,ph102,ph108,
         COGimmediaterecall1, COGimmediaterecall2, COGdelayedrecall,
         ph114, COGprosmem1, COGprosmem2, ph107_1, ph107_2, ph107_3)

# create immediate recall score
wave2$immediaterecall_total<-wave2$COGimmediaterecall1+wave2$COGimmediaterecall2

# append column headers with W2 and rename variable names to be consistend with other waves
wave2W2<-wave2
colnames(wave2W2) <- paste(colnames(wave2W2), "W2", sep = "_")
wave2W2<-wave2W2%>% 
  dplyr::rename(
    tilda_serial = tilda_serial_W2,
    COGanimal_naming_W2 = ph125_W2
  )

# select variable names from wave 4
wave4<-wave4%>% 
  select(tilda_serial, age,age3, sex,
         COGmmse, COGanimal_naming,
         edu3,ph102,ph108,
         COGimmediaterecall1, COGimmediaterecall2, COGdelayedrecall,
         ph114, COGprosmem1, COGprosmem2, ph107_01, ph107_02, ph107_03)

# create immediate recall score
wave4$immediaterecall_total<-wave4$COGimmediaterecall1+wave4$COGimmediaterecall2

# append column headers with W4
wave4W4<-wave4
colnames(wave4W4) <- paste(colnames(wave4W4), "W4", sep = "_")
wave4W4<-wave4W4%>% 
  dplyr::rename(
    tilda_serial = tilda_serial_W4
  )

# select variable names from wave 5
wave5<-wave5%>% 
  select(tilda_serial, age,age3, sex,
         COGmmse, COGanimal_naming,
         edu3,ph102,ph108a,
         COGimmediaterecall1, COGimmediaterecall2, COGdelayedrecall,
         ph114, COGprosmem1, COGprosmem2, ph107_01, ph107_02)

# create immediate recall score
wave5$immediaterecall_total<-wave5$COGimmediaterecall1+wave5$COGimmediaterecall2

# append column headers with W5
wave5W5<-wave5
colnames(wave5W5) <- paste(colnames(wave5W5), "W5", sep = "_")
wave5W5<-wave5W5%>% 
  dplyr::rename(
    tilda_serial = tilda_serial_W5
  )



#### merge datasets ####
tilda_dataW3W1<-merge(tilda_dataW3, wave1W1, by='tilda_serial')
tilda_dataW3W1W2<-merge(tilda_dataW3W1, wave2W2, by='tilda_serial')
tilda_dataW3W1W2W4<-merge(tilda_dataW3W1W2, wave4W4, by='tilda_serial')
tilda_dataW3W1W2W4W5<-merge(tilda_dataW3W1W2W4, wave5W5, by='tilda_serial')
tilda_dataW3W1W2W4W5_orig<-tilda_dataW3W1W2W4W5
#### data cleaning of HA variables at W1 ####

#### data drames used only for k means script ####
# These dataframes have only the columns used for clustering
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

