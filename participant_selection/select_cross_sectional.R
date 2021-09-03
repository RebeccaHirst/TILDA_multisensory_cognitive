" Apply cross-sectional exclusion criteria to wave 3 TILDA data
Selects participants with SIFI, cognitive and demographic variables available at wave 3 of TILDA

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
# clear workspace variables

#rm(list = ls())

# Do you want to store plots of the cognitive variables? If so where (note if TRUE plots take a while to generate)
store_cog_plots <- FALSE

# Path to store distribution plots of cognitive scores
pre_outlier_path <- '/Users/rebeccahirst/Documents/TILDA_post_doc/Cognitive_function_paper/updated_analysis/Figures/cognitive_variables/pre_outlier/'
post_outlier_path <- '/Users/rebeccahirst/Documents/TILDA_post_doc/Cognitive_function_paper/updated_analysis/Figures/cognitive_variables/post_outlier/'

#### import packages ####

require(foreign) # for data imports (e.g. 'read.dta')
library(PRISMAstatement) # to automatically populate exclusion chart
library(dplyr) # to select relevant columns for data frame
library(tidyr) # to tidy up data frame (e.g. 'drop_na')
library(gridExtra) # to create multipanel figures in a 'grid'
require(ggplot2) # for plots
library(stringr)# to manipulate string variables
library(lme4)# for fitting mixed effects models
library(ggeffects) # for plotting mixed effects results


#### import data ####

# Computer Assisted Personal Interview (CAPI) - WAVE 3
capi <- read.dta("/Volumes/releases/Wave 3/CAPI/tildav3-5-0.dta")

# Healthcare assessment (ha) - WAVE 3
ha <- read.dta("/Volumes/releases/Wave 3/tilda-HAC-v3.2.2.dta")

# Merge the capi (Computer assisted personal interview) and ha dataframes
tilda_data <-merge(capi, ha, by="tilda_serial")

# Retain original data frame
tilda_data_orig<-tilda_data

# Select variables used for analysis
# Note: COGmoca allowed us to keep to additional participants that were missing 'moca'
tilda_data<-tilda_data_orig%>% 
  select(tilda_serial, age,age3, sex, in_ha, 
         COGmoca, COGmmse, 
         `_has_sifi_data`,edu3,ph102,ph108, ph107_98, visualAcuityLeft, visualAcuityRight,
         CRTmeancog, CRTmeanmot, CRTsdcog, CRTsdmot,
         COGsartCookedMean,COGsartOmmissions, COGsartErrors3,
         COGtrail1time, COGtrail2time,COGtraildifftime, COGtraildeltatime,
         COGimmediaterecall1, COGimmediaterecall2, COGdelayedrecall,
         COGanimal_naming,COGtrail1errors, COGtrail2errors, COGtrail1nearmisses,
         COGtrail2nearmisses, COGtrail1prompts, COGtrail2prompts,COGtrail2colorerrors,
         ph114, ph142, COGprosmem1, COGprosmem2, ph107_01, ph107_02, ph107_03,
         Shams_2B0F_70, Shams_2B0F_150, Shams_2B0F_230, Shams_0B2F,
         Shams_1B1F, Shams_2B1F_70, Shams_2B1F_150, Shams_2B1F_230,
         Shams_2B1F_m70, Shams_2B1F_m150, Shams_2B1F_m230, COGnartRawScore)

#### Handy functions ####
# See corresponding function tests in 'function_tests'

# Exclude outliers based on +/- 3 standard deviations
exclude_outliers<- function(df, col) {
  upperlimit<-mean(df[, col])+(3*sd(df[, col]))
  lowerlimit<-mean(df[, col])-(3*sd(df[, col]))
  df <- df[!(df[, col]>upperlimit),]
  var_outliers_removed <- df[!(df[, col]<lowerlimit),]
  return(var_outliers_removed)
}

# Extract the number from a string e.g. numextract('hithere203') returns '203'
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# Plot distributions (function called before and after outlier exclusion)
plot_cog_distributions <- function(varList, tilda_data, outpath){
  # Input:
  #     varList: a list of variable names to plot
  #     tilda_data: the current tilda dataframe
  #     outpath: the path to where the figures will be saved
  
  # Note: no specified binwidth because appropriate bin width differs for each variable
  for (var in varList){
    p1<-ggplot(tilda_data, aes(tilda_data[, var])) + 
      geom_histogram(alpha = 0.7,  position = 'identity')+xlim(0, max(tilda_data[, var]))+xlab(var)
    p2<-ggplot(tilda_data[tilda_data$age3=='50-64',], aes(tilda_data[tilda_data$age3=='50-64',][, var], fill = sex)) + 
      geom_histogram(alpha = 0.7,  position = 'identity')+xlim(0, max(tilda_data[, var]))+xlab(var)+ggtitle('50-64 year olds')
    p3<-ggplot(tilda_data[tilda_data$age3=='65-74',], aes(tilda_data[tilda_data$age3=='65-74',][, var], fill = sex)) + 
      geom_histogram(alpha = 0.7,  position = 'identity')+xlim(0, max(tilda_data[, var]))+xlab(var)+ggtitle('65-74year olds')
    p4<-ggplot(tilda_data[tilda_data$age3=='>=75',], aes(tilda_data[tilda_data$age3=='>=75',][, var], fill = sex)) + 
      geom_histogram(alpha = 0.7,  position = 'identity')+xlim(0, max(tilda_data[, var]))+xlab(var)+ggtitle('>75 year olds')
    pp<-grid.arrange(p1, p2,p3,p4, nrow = 2, ncol=2)
    
    # change path of output depending on if outliers have been excluded yet
    ggsave(
      paste(outpath, var, '.pdf'),
      plot = pp,
      device = NULL,
      path = NULL,
      scale = 1,
      width = 5,
      height = 6,
      units = c("in"),
      dpi = 300,
      limitsize = TRUE,
    )}
}
#### Apply exclusion criteria ####

# Track the number of participants dropped at each stage to feed into the exclusion figure
my_incl_counts<-c()
my_incl_labels<-c()
my_excl_labels<-c()

# Not in health assessement (therefore no SIFI data)
tilda_data<-tilda_data[(tilda_data$in_ha=="Centre assessment in Dublin"),]
tilda_data<-tilda_data[(tilda_data$`_has_sifi_data`==1),]
my_incl_counts<-c(my_incl_counts, nrow(tilda_data))

# Age cut off
tilda_data<-tilda_data[!(tilda_data$age<50),]
n_minus_age<- nrow(tilda_data)
my_incl_counts<-c(my_incl_counts, nrow(tilda_data))
my_incl_labels<-c(my_incl_labels, 'Selected age range')
my_excl_labels<-c(my_excl_labels, 'Age <50 years')

# Missing demographic variables (i.e. field contains 'na')
tilda_data<-tilda_data%>% 
  drop_na(age, sex, edu3)
my_incl_counts<-c(my_incl_counts, nrow(tilda_data))
my_incl_labels<-c(my_incl_labels, 'has key demographics')
my_excl_labels<-c(my_excl_labels, 'missing demographics')

# Registered legally blind
tilda_data<-tilda_data[!(tilda_data$ph102==6),]
my_incl_counts<-c(my_incl_counts, nrow(tilda_data))
my_incl_labels<-c(my_incl_labels, 'not legally blind')
my_excl_labels<-c(my_excl_labels, 'registered legally blind')

# Responded 'dont know' (DK) to self reported vision or hearing
tilda_data<-tilda_data[!(tilda_data$ph102==98),]
my_incl_counts<-c(my_incl_counts, nrow(tilda_data))
my_incl_labels<-c(my_incl_labels, 'has self reported vision')
my_excl_labels<-c(my_excl_labels, 'DK to self-reported vision')

tilda_data<-tilda_data[!(tilda_data$ph108==98),]
my_incl_counts<-c(my_incl_counts, nrow(tilda_data))
my_incl_labels<-c(my_incl_labels, 'has self reported hearing')
my_excl_labels<-c(my_excl_labels, 'DK to self-reported hearing')

# Responses 'dont know' (DK) to hearing aid use
tilda_data<-tilda_data[!(tilda_data$ph107_98=="Yes"),]
my_incl_counts<-c(my_incl_counts, nrow(tilda_data))
my_incl_labels<-c(my_incl_labels, 'has self reported hearing aid')
my_excl_labels<-c(my_excl_labels, 'DK to self-reported hearing aid')

# Create variable to indicate best visual acuity 
tilda_data$best_acuity <- with(tilda_data, pmin(visualAcuityLeft, visualAcuityRight, na.rm=TRUE))
tilda_data$worst_acuity <- with(tilda_data, pmax(visualAcuityLeft, visualAcuityRight, na.rm=TRUE))

# Exclude if no VAS score could be created (e.g. visual acuity not assessed for either eye)
tilda_data<-tilda_data%>% 
  drop_na(best_acuity)
tilda_data$VAS<-100 - 50 * tilda_data$best_acuity
my_incl_counts<-c(my_incl_counts, nrow(tilda_data))
my_incl_labels<-c(my_incl_labels, 'has visual acuity score')
my_excl_labels<-c(my_excl_labels, 'no visual acuity score')

# Start new set of lists to have separate figure panel for exclusions based on cognitive variables
my_incl_counts_cog<-c()
my_incl_labels_cog<-c()
my_excl_labels_cog<-c()

# Missing cognitive measures
# Start N
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))

# Missing MoCA
tilda_data<-tilda_data%>% 
  drop_na(COGmoca)
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has MoCa')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missing MoCa')

# Missing MMSE
# tilda_data<-tilda_data%>% 
#   drop_na(COGmmse)
# my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
# my_incl_labels_cog<-c(my_incl_labels_cog, 'has MMSE')
# my_excl_labels_cog<-c(my_excl_labels_cog, 'missing MMSE')

# Missing CRT
tilda_data<-tilda_data%>% 
  drop_na(CRTmeancog, CRTmeanmot)
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has CRT')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missing CRT')

# Missing SART
tilda_data<-tilda_data%>% 
  drop_na(COGsartCookedMean, COGsartOmmissions, COGsartErrors3)
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has SART')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missing SART')

# Missing NART
# tilda_data<-tilda_data%>% 
#   drop_na(COGnartRawScore)
# my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
# my_incl_labels_cog<-c(my_incl_labels_cog, 'has NART')
# my_excl_labels_cog<-c(my_excl_labels_cog, 'missing NART')

# Missing CTT
tilda_data<-tilda_data%>% 
  drop_na(COGtrail1time, COGtrail2time)
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has CTT')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missing CTT')

# Missing Animal naming
tilda_data<-tilda_data%>% 
  drop_na(COGanimal_naming)
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has Animal naming')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missing Animal naming')

# Missing Immediate recall
tilda_data<-tilda_data%>% 
  drop_na(COGimmediaterecall1, COGimmediaterecall2)
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has Immediate recall')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missing Immediate recall')

# Missing delayed recall
tilda_data<-tilda_data%>% 
  drop_na(COGdelayedrecall)
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has Delayed recall')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missing Delayed recall')

# Missing prospective memory
# tilda_data<-tilda_data%>% 
#   drop_na(COGprosmem1, COGprosmem2)
# my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
# my_incl_labels_cog<-c(my_incl_labels_cog, 'has prospective memory')
# my_excl_labels_cog<-c(my_excl_labels_cog, 'missing prospective memory')

# Missing or responsed 'don't know' (DK) to self-reported day-to-day memory
# tilda_data<-tilda_data[!(tilda_data$ph114==98),]
# my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
# my_incl_labels_cog<-c(my_incl_labels_cog, 'has self reported day-to-day memory')
# my_excl_labels_cog<-c(my_excl_labels_cog, 'DK self reported day-to-day memory')
# 
# # Missing or responsed 'don't know' (DK) to self-reported memory decline
# tilda_data<-tilda_data[!(tilda_data$ph142==98),]
# tilda_data<-tilda_data[!(tilda_data$ph142==-1),]
# my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
# my_incl_labels_cog<-c(my_incl_labels_cog, 'has self memory decline since last interview')
# my_excl_labels_cog<-c(my_excl_labels_cog, 'DK self memory decline or no previous interview')

#### Make exclusion figures ####

# Figure 1a shows up until the point of exclusion due to cognitive measures
flow_exclusions(
  incl_counts = my_incl_counts,
  total_label = "Total in wave 3 (w3) Health Assessment",
  incl_labels = my_incl_labels,
  excl_labels = my_excl_labels,
  percent_of_prev = TRUE
)

# Figure 1b shows exclusions due to cognitive measures
flow_exclusions(
  incl_counts = my_incl_counts_cog,
  total_label = "Has wave 3 SIFI",
  incl_labels = my_incl_labels_cog,
  excl_labels = my_excl_labels_cog,
  percent_of_prev = TRUE
)


# Generate total immediate recall score 
# https://tilda.tcd.ie/publications/reports/pdf/w4-key-findings-report/Chapter%208.pdf

tilda_data$immediaterecall_total<-rowSums(cbind(tilda_data$COGimmediaterecall1, tilda_data$COGimmediaterecall2))

#### Characterize cognitive measures ####
# Before exclusions based on Outliers

varList<-c("COGsartCookedMean", "COGsartOmmissions", "COGsartErrors3", 
           "COGtrail1time", "COGtrail1time", "COGtrail2time", "COGtrail2time",
           "CRTmeancog", "CRTmeanmot", "CRTsdcog", "CRTsdmot", "COGprosmem1",
           "COGprosmem2", "ph114", "ph142", "COGimmediaterecall1", "COGimmediaterecall2", "immediaterecall_total",
           "COGdelayedrecall", "COGtraildifftime", "COGtraildeltatime",
           "COGanimal_naming","COGtrail1errors", "COGtrail2errors", "COGtrail1nearmisses",
           "COGtrail2nearmisses", "COGtrail1prompts", "COGtrail2prompts","COGtrail2colorerrors", "COGnartRawScore", "immediaterecall_total")

if(store_cog_plots){
  # Plot and save the cognitive distributions (pre outlier removal)
  plot_cog_distributions(varList, tilda_data, pre_outlier_path) 
}

# Plot proportion with self reported fair/poor memory (as in https://tilda.tcd.ie/publications/reports/pdf/w4-key-findings-report/Chapter%208.pdf)
# Make binary fair/poor memory variable
# tilda_data$fair_or_poor_memory<-ifelse(tilda_data$ph114>=4, 1, 0)
# ageByMem<-table(tilda_data$fair_or_poor_memory, tilda_data$age3)
# propotionMem<-ageByMem[2,]/ageByMem[1,]

#### Exclude based on outlying cognitive measures ####

# Start new set of lists to have separate figure panel for exclusions based on cognitive variables
my_incl_counts_cog2<-c()
my_incl_labels_cog2<-c()
my_excl_labels_cog2<-c()

# Start N
my_incl_counts_cog2<-c(my_incl_counts_cog2, nrow(tilda_data))

# Exclude CRT outliers based on 3* SD from mean
tilda_data<-exclude_outliers(tilda_data, 'CRTmeancog')
my_incl_counts_cog2<-c(my_incl_counts_cog2, nrow(tilda_data))
my_incl_labels_cog2<-c(my_incl_labels_cog2, 'non outlying CRT cog')
my_excl_labels_cog2<-c(my_excl_labels_cog2, 'outlying CRT cog')


tilda_data<-exclude_outliers(tilda_data, 'CRTmeanmot')
my_incl_counts_cog2<-c(my_incl_counts_cog2, nrow(tilda_data))
my_incl_labels_cog2<-c(my_incl_labels_cog2, 'non outlying CRT mot')
my_excl_labels_cog2<-c(my_excl_labels_cog2, 'outlying CRT mot')


# Exclude outlying CTT times
tilda_data<-exclude_outliers(tilda_data, 'COGtrail1time')
my_incl_counts_cog2<-c(my_incl_counts_cog2, nrow(tilda_data))
my_incl_labels_cog2<-c(my_incl_labels_cog2, 'non outlying CTT1')
my_excl_labels_cog2<-c(my_excl_labels_cog2, 'outlying CRT CTT1')

tilda_data<-exclude_outliers(tilda_data, 'COGtrail2time')
my_incl_counts_cog2<-c(my_incl_counts_cog2, nrow(tilda_data))
my_incl_labels_cog2<-c(my_incl_labels_cog2, 'non outlying CTT2')
my_excl_labels_cog2<-c(my_excl_labels_cog2, 'outlying CRT CTT2')

# CTT times less than 5 seconds or over 550 seconds (possible data input error)
tilda_data<-tilda_data[!(tilda_data$COGtrail1time<5),]
tilda_data<-tilda_data[!(tilda_data$COGtrail1time>550),]
tilda_data<-tilda_data[!(tilda_data$COGtrail2time<5),]
my_incl_counts_cog2<-c(my_incl_counts_cog2, nrow(tilda_data))
my_incl_labels_cog2<-c(my_incl_labels_cog2, 'realistic CTT time')
my_excl_labels_cog2<-c(my_excl_labels_cog2, 'CTT1 time <5 seconds or >550 seconds/CCT time <5 seconds')

# Unrealistic SART errors - suggesting participant did not understand task instructions
tilda_data<-tilda_data[!(tilda_data$COGsartOmmissions>92),] # (omission errors>=50%of go trials) missed over 50% of the possible go trials 207 total trials 23 ommissions
tilda_data<-tilda_data[!(tilda_data$COGsartErrors3>18),] # (commission errors>=80% of stom trials) pressed on over 80% of the 3s
my_incl_counts_cog2<-c(my_incl_counts_cog2, nrow(tilda_data))
my_incl_labels_cog2<-c(my_incl_labels_cog2, 'has SART')
my_excl_labels_cog2<-c(my_excl_labels_cog2, 'missed >50% press trials OR pressed >80% no press trials')

# Figure 1c shows exclusions due to outlying cognitive measures
flow_exclusions(
  incl_counts = my_incl_counts_cog2,
  total_label = "Available with SIFI and cognitive measures",
  incl_labels = my_incl_labels_cog2,
  excl_labels = my_excl_labels_cog2,
  percent_of_prev = TRUE
)

if(store_cog_plots){
  # Plot and save the cognitive distributions (post outlier removal)
  plot_cog_distributions(varList, tilda_data, post_outlier_path)
}
