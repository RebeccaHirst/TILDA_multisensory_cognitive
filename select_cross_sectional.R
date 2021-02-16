"Analyse Sound-Induced Flash Illusion (SIFI) and Cognitive TILDA data

Research Question: What factors of cognitive function influence SIFI performance?

Context: Our original analysis consisted of 1) EFA on all cognitive measures
        to extract factors of interes 2) use these parameters to predict _Efficiency_
        (Efficient = more accurate at longer SOAs relative to short; 
        Inefficienct = less accurate, or similar accuracy, at longer SOAs relative to short)
        Reviewer feedback suggested that the use of these groupings was unclear therefore we 
        opted to adjust our approach. 

Script sections:
        Select participants (for cross -sectional analysis)
        Analyse cognitive data 
        Logistic mixed effects model: SIFI as outcome variable cog as predictors

Author: Rebecca J Hirst (Hirstr@tcd.ie)
Last Edited: 181220

To do:
  - Check cases where variable starts with COG and pick one 
"
# clear workspace variables
#rm(list = ls())

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

# Healthcare assesement - WAVE 3
ha <- read.dta("/Volumes/releases/Wave 3/tilda-HAC-v3.2.2.dta")

# merge the capi and ha dataframes
tilda_data <-merge(capi, ha, by="tilda_serial")

# retain original data frame
tilda_data_orig<-tilda_data

# select variables we will use for analysis
# note: COGmoca allowed us to keep to additional participants that were missing 'moca'
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

#### handy functions ####
# each function has a corresponding test in 'function_tests'
exclude_outliers<- function(df, col) {
  upperlimit<-mean(df[, col])+(3*sd(df[, col]))
  lowerlimit<-mean(df[, col])-(3*sd(df[, col]))
  df <- df[!(df[, col]>upperlimit),]
  var_outliers_removed <- df[!(df[, col]<lowerlimit),]
  return(var_outliers_removed)
}

# Will extract the number from a string e.g. numextract('hithere203') returns '203'
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

#### apply exclusion criteria ####

# track the number of participants at each stage to feed into the exclusion figure
my_incl_counts<-c()
my_incl_labels<-c()
my_excl_labels<-c()

# not in health assessement (therefore no SIFI data)
tilda_data<-tilda_data[(tilda_data$in_ha=="Centre assessment in Dublin"),]
tilda_data<-tilda_data[(tilda_data$`_has_sifi_data`==1),]
my_incl_counts<-c(my_incl_counts, nrow(tilda_data))

#Age cut off
tilda_data<-tilda_data[!(tilda_data$age<50),]
n_minus_age<- nrow(tilda_data)
my_incl_counts<-c(my_incl_counts, nrow(tilda_data))
my_incl_labels<-c(my_incl_labels, 'Selected age range')
my_excl_labels<-c(my_excl_labels, 'Age <50 years')

# missing demographic variables (i.e. field contains 'na')
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

# Exclude if no VAS score could be created (visual acuity not assessed for either eye)
tilda_data<-tilda_data%>% 
  drop_na(best_acuity)
tilda_data$VAS<-100 - 50 * tilda_data$best_acuity
my_incl_counts<-c(my_incl_counts, nrow(tilda_data))
my_incl_labels<-c(my_incl_labels, 'has visual acuity score')
my_excl_labels<-c(my_excl_labels, 'no visual acuity score')


# Start new set of lists to have seperate figure panel for exclusions based on cognitive variables
my_incl_counts_cog<-c()
my_incl_labels_cog<-c()
my_excl_labels_cog<-c()

# Missing cognitive measures

# Missing MoCA
tilda_data<-tilda_data%>% 
  drop_na(COGmoca)
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))

# Missing MMSE
tilda_data<-tilda_data%>% 
  drop_na(COGmmse)
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has mmse')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missing mmse')

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
tilda_data<-tilda_data%>% 
  drop_na(COGnartRawScore)
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has NART')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missing NART')

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
my_incl_labels_cog<-c(my_incl_labels_cog, 'has immediate recall')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missing immediate recall')

# Missing delayed recall
tilda_data<-tilda_data%>% 
  drop_na(COGdelayedrecall)
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has delayed recall')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missing delayed recall')

# Missing prospective memory
tilda_data<-tilda_data%>% 
  drop_na(COGprosmem1, COGprosmem2)
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has prospective memory')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missing prospective memory')

# Missing or responsed 'don't know' (DK) to self-reported day-to-day memory
tilda_data<-tilda_data[!(tilda_data$ph114==98),]
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has self reported day-to-day memory')
my_excl_labels_cog<-c(my_excl_labels_cog, 'DK self reported day-to-day memory')

# Missing or responsed 'don't know' (DK) to self-reported memory decline
tilda_data<-tilda_data[!(tilda_data$ph142==98),]
tilda_data<-tilda_data[!(tilda_data$ph142==-1),]
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has self memory decline since last interview')
my_excl_labels_cog<-c(my_excl_labels_cog, 'DK self memory decline or no previous interview')

#Update note - think we might want to use tilda_data$COGtraildifftime instead of CTT1 and CTT2 individually?

#### make exclusion figures ####

#Figure 1a shows up untill the point of exclusion due to cognitive measures
flow_exclusions(
  incl_counts = my_incl_counts,
  total_label = "Total in wave 3 (w3) Health Assessment",
  incl_labels = my_incl_labels,
  excl_labels = my_excl_labels,
  percent_of_prev = TRUE
)

#Figure 1b shows exclusions due to cognitive measures
flow_exclusions(
  incl_counts = my_incl_counts_cog,
  total_label = "Total in wave 3 (w3) Health Assessment",
  incl_labels = my_incl_labels_cog,
  excl_labels = my_excl_labels_cog,
  percent_of_prev = TRUE
)


# generate total immediate recall score 
# https://tilda.tcd.ie/publications/reports/pdf/w4-key-findings-report/Chapter%208.pdf

tilda_data$immediaterecall_total<-rowSums(cbind(tilda_data$COGimmediaterecall1, tilda_data$COGimmediaterecall2))

#### characterize cognitive measures ####
# Before exclusions based on outliers

varList<-c("COGsartCookedMean", "COGsartOmmissions", "COGsartErrors3", 
           "COGtrail1time", "COGtrail1time", "COGtrail2time", "COGtrail2time",
           "CRTmeancog", "CRTmeanmot", "CRTsdcog", "CRTsdmot", "COGprosmem1",
           "COGprosmem2", "ph114", "ph142", "COGimmediaterecall1", "COGimmediaterecall2", "immediaterecall_total",
           "COGdelayedrecall", "COGtraildifftime", "COGtraildeltatime",
           "COGanimal_naming","COGtrail1errors", "COGtrail2errors", "COGtrail1nearmisses",
           "COGtrail2nearmisses", "COGtrail1prompts", "COGtrail2prompts","COGtrail2colorerrors", "COGnartRawScore")


varList<-c("immediaterecall_total")

# not specified binwidth because appropriate bin width differs for each variable

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
    paste('/Users/rebeccahirst/Documents/TILDA_post_doc/Cognitive_function_paper/updated_analysis/Figures/cognitive_variables/post_outlier/', var, '.pdf'),
    plot = pp,
    device = NULL,
    path = NULL,
    scale = 1,
    width = NA,
    height = NA,
    units = c("in", "cm", "mm"),
    dpi = 300,
    limitsize = TRUE,
  )}

#plot proportion with self reported fair/poor memory (as in https://tilda.tcd.ie/publications/reports/pdf/w4-key-findings-report/Chapter%208.pdf)
# make binary fair/poor memory variable
tilda_data$fair_or_poor_memory<-ifelse(tilda_data$ph114>=4, 1, 0)
ageByMem<-table(tilda_data$fair_or_poor_memory, tilda_data$age3)
propotionMem<-ageByMem[2,]/ageByMem[1,]

#### Exclude based on outlying cognitive measures ####

#exclude CRT outliers based on 3* sd from mean
tilda_data<-exclude_outliers(tilda_data, 'CRTmeancog')
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'non outlying CRT cog')
my_excl_labels_cog<-c(my_excl_labels_cog, 'outlying CRT cog')


tilda_data<-exclude_outliers(tilda_data, 'CRTmeanmot')
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'non outlying CRT mot')
my_excl_labels_cog<-c(my_excl_labels_cog, 'outlying CRT mot')


#exclude outlying CTT times
tilda_data<-exclude_outliers(tilda_data, 'COGtrail1time')
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'non outlying CTT1')
my_excl_labels_cog<-c(my_excl_labels_cog, 'outlying CRT CTT1')

tilda_data<-exclude_outliers(tilda_data, 'COGtrail2time')
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'non outlying CTT2')
my_excl_labels_cog<-c(my_excl_labels_cog, 'outlying CRT CTT2')

# CTT times less than 5 seconds or over 550 seconds (possible data input error)
#this was our initial cut off - perhaps we only need sd +/-3 now?
tilda_data<-tilda_data[!(tilda_data$COGtrail1time<5),]
tilda_data<-tilda_data[!(tilda_data$COGtrail1time>550),]#
tilda_data<-tilda_data[!(tilda_data$COGtrail2time<5),]
my_incl_counts_cog<-nrow(tilda_data)
my_incl_labels_cog<-c(my_incl_labels_cog, 'realistic CTT time')
my_excl_labels_cog<-c(my_excl_labels_cog, 'CTT1 time <5 seconds or >550 seconds/CCT time <5 seconds')

# unrealistic SART errors 
tilda_data<-tilda_data[!(tilda_data$COGsartOmmissions>92),]#missed over 50% of the possible go trials 207 total trials 23 ommissions
tilda_data<-tilda_data[!(tilda_data$COGsartErrors3>18),]#pressed on over 80% of the 3s
my_incl_counts_cog<-c(my_incl_counts_cog, nrow(tilda_data))
my_incl_labels_cog<-c(my_incl_labels_cog, 'has SART')
my_excl_labels_cog<-c(my_excl_labels_cog, 'missed >50% press trials OR pressed >80% no press trials')


