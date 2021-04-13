" Run logistic mixed effects models to predict accuracy on SIFI from 
the cross-sectional cognitive measures available at the wave 3 healthcase assessment.

Author(s): Hirst R J 

This script should be run following select_cross_sectional.R and select_longitudinal.R (which select participants included in analysis)

The analysis comprises of three parts, with similar structure for each:
  1. Cross-sectional analysis of Choice Reaction Time (CRT)
  2. Cross-sectional analysis of Sustained Attention to Response Task (SART)
  3. Cross-sectional analysis of Colour Trials Test (CTT)

All models are run fully adjusted for covariates. 
"
#### import libraries ####

library(lme4) # lme4 library for the mixed effects models
library(dotwhisker) # for dot whisker plots of results 

#### Functions ####
# A set of custom functions used throughout analysis

# Extract number from a string
library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

#### Cross-sectional analysis of Choice Reaction Time (CRT) task ####
# This set of models explore the effect of cognitive and motor elements of the CRT in relation to SIFI

# Make dataframes to be used in models (including co-variates to be included later)
# ph108_W3 - self reported hearing at wave 3
# ph102 - self reported vision at wave 3
analysis_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial, Shams_2B1F_m230_W3, Shams_2B1F_m150_W3, Shams_2B1F_m70_W3, Shams_2B1F_70_W3, 
         Shams_2B1F_150_W3, Shams_2B1F_230_W3, 
         age_W3, sex_W3, edu3_W3, VAS_W3, ph108_W3, ph102_W3, Shams_1B1F_W3, Shams_2B0F_70_W3, 
         Shams_0B2F_W3, CRTmeancog_W3, CRTmeanmot_W3)

#### Prep dataframes for models ####

# Reshape to long format for mixed model #

# Rename the SIFI columns with standard formatting so that they can be reshaped
names(analysis_df)[names(analysis_df) ==  "Shams_2B1F_m230_W3"]<-"Pre_230"
names(analysis_df)[names(analysis_df) ==  "Shams_2B1F_m150_W3"]<-"Pre_150"
names(analysis_df)[names(analysis_df) ==  "Shams_2B1F_m70_W3"]<-"Pre_70"
names(analysis_df)[names(analysis_df) == "Shams_2B1F_70_W3"]<-"Post_70"
names(analysis_df)[names(analysis_df) == "Shams_2B1F_150_W3"]<-"Post_150"
names(analysis_df)[names(analysis_df) == "Shams_2B1F_230_W3"]<-"Post_230"

# View the dataframe
View(analysis_df)

# Reshape based on SOA - gives 6 rows per participant (-230, -150,-70, 70, 150, 230)
analysis_df_long<- reshape(analysis_df, idvar="tilda_serial",
                           varying = c("Pre_230","Pre_150", "Pre_70", "Post_70", "Post_150", "Post_230"),
                           v.name=c("Accuracy"),
                           times=c("Pre_230","Pre_150", "Pre_70", "Post_70", "Post_150", "Post_230"),
                           direction="long")

# Make column for the Pre/Post (i.e. negative and positive SOAs) factor that is created from the time column
analysis_df_long$Pre_Post <- as.numeric(grepl('Pre', analysis_df_long$time, ignore.case=T))

# Make an SOA column from the "time" column (using custom numextract function)
analysis_df_long$SOA<-numextract(analysis_df_long$time)

# View the data frame
View(analysis_df_long)

# Set the new variables (Pre/Post and SOA) as factors
analysis_df_long$SOA=factor(analysis_df_long$SOA, levels = c("70", "150", "230"))
analysis_df_long$Pre_Post=as.factor(analysis_df_long$Pre_Post)

# Set self reported sensory function as factors
analysis_df_long$ph102_W3=factor(analysis_df_long$ph102_W3, levels = c(5, 4, 3, 2, 1))# in previous analyses we recoded to reverse so higher was better across measures - do we need to here?
analysis_df_long$ph108_W3=factor(analysis_df_long$ph108_W3, levels = c(5, 4, 3, 2, 1))# in previous analyses we recoded to reverse so higher was better across measures - do we need to here?

# Set performance on control tasks as factors
analysis_df_long$Shams_0B2F_W3=factor(analysis_df_long$Shams_0B2F_W3, levels = c(0, 0.5, 1))
analysis_df_long$Shams_2B0F_70_W3=factor(analysis_df_long$Shams_2B0F_70_W3, levels = c(0, 0.5, 1))
analysis_df_long$Shams_1B1F_W3=factor(analysis_df_long$Shams_1B1F_W3, levels = c(0, 0.5, 1))

# Scale continuous/numeric predictors in model # 
analysis_df_long_scaled <- analysis_df_long
analysis_df_long_scaled$age_W3 <-scale(analysis_df_long_scaled$age_W3)
analysis_df_long_scaled$VAS_W3 <-scale(analysis_df_long_scaled$VAS_W3)
analysis_df_long_scaled$CRTmeancog_W3 <-scale(analysis_df_long_scaled$CRTmeancog_W3)
analysis_df_long_scaled$CRTmeanmot_W3 <-scale(analysis_df_long_scaled$CRTmeanmot_W3)

# take into account how many trials per condition for a logistic model
analysis_df_long_scaled$nTrials<-2

#### Fit mixed models ####
# Fully adjusted models

# An additive null model (no cognitive measures) - Adjusted Null model
SOA_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive <-glmer(
  Accuracy ~  age_W3 + sex_W3 + edu3_W3 + SOA + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
  Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# An additive model with MRT and CRT aspects of the CRT - Adjusted CRT + MRT model
SOA_CRTboth_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive <-glmer(
  Accuracy ~  age_W3 + CRTmeancog_W3 + CRTmeanmot_W3 + sex_W3 + edu3_W3 + SOA + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# An additive model with MRT aspect of the CRT - Adjusted MRT model
SOA_CRTmot_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive <-glmer(
  Accuracy ~  age_W3 + CRTmeanmot_W3 + sex_W3 + edu3_W3 + SOA + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# An additive model with CRT aspect of the CRT - Adjusted CRT model
SOA_CRTcog_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive <-glmer(
  Accuracy ~  age_W3 + CRTmeancog_W3 + sex_W3 + edu3_W3 + SOA + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# Test significance of adjusted additive effects 

# Likelihood ratio test: Adjusted Null vs. Adjusted CRT model
anova(SOA_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive, SOA_CRTcog_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive)

# Likelihood ratio test: Adjusted Null vs. Adjusted MRT model
anova(SOA_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive, SOA_CRTmot_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive)

# Adjusted interaction model: MRT * SOA
SOA_CRTmot_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_interaction <-glmer(
  Accuracy ~  age_W3 + CRTmeanmot_W3*SOA + sex_W3 + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# Likelihood ratio test: Adjusted MRT + SOA model vs Adjusted MRT * SOA model
anova(SOA_CRTmot_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive, SOA_CRTmot_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_interaction)

# Dot whisker plot of full best model
dwplot(SOA_CRTmot_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_interaction,dodge_size = 1, vline=geom_vline(xintercept=0, colour="grey60", linetype=2),dot_args = list(aes(shape = model)), show_intercept = TRUE)


#### Cross-sectional analysis of Sustained Attention to Response Time (SART) task ####
# These models explore the error and omission elements of the SART in relation to SIFI 

# Make dataframe to be used in models
analysis_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial, Shams_2B1F_m230_W3, Shams_2B1F_m150_W3, Shams_2B1F_m70_W3, Shams_2B1F_70_W3, 
         Shams_2B1F_150_W3, Shams_2B1F_230_W3, 
         age_W3, sex_W3, edu3_W3, VAS_W3, ph108_W3, ph102_W3, Shams_1B1F_W3, Shams_2B0F_70_W3, 
         Shams_0B2F_W3, COGsartOmmissions_W3, COGsartErrors3_W3)

#### Prep dataframes for models ####

# Reshape to long format for mixed model 

# Rename the SIFI columns with standard formatting so that they can be reshaped
names(analysis_df)[names(analysis_df) ==  "Shams_2B1F_m230_W3"]<-"Pre_230"
names(analysis_df)[names(analysis_df) ==  "Shams_2B1F_m150_W3"]<-"Pre_150"
names(analysis_df)[names(analysis_df) ==  "Shams_2B1F_m70_W3"]<-"Pre_70"
names(analysis_df)[names(analysis_df) == "Shams_2B1F_70_W3"]<-"Post_70"
names(analysis_df)[names(analysis_df) == "Shams_2B1F_150_W3"]<-"Post_150"
names(analysis_df)[names(analysis_df) == "Shams_2B1F_230_W3"]<-"Post_230"


# View the dataframe
View(analysis_df)

# Reshape based on SOA - gives 6 rows per participant (-230, -150,-70, 70, 150, 230)
analysis_df_long<- reshape(analysis_df, idvar="tilda_serial",
                           varying = c("Pre_230","Pre_150", "Pre_70", "Post_70", "Post_150", "Post_230"),
                           v.name=c("Accuracy"),
                           times=c("Pre_230","Pre_150", "Pre_70", "Post_70", "Post_150", "Post_230"),
                           direction="long")

# Make a column for the Pre/Post factor (i.e. negative or positive SOAs) that is created from the time column
analysis_df_long$Pre_Post <- as.numeric(grepl('Pre', analysis_df_long$time, ignore.case=T))

# Make an SOA column from the time column SOA (use custom numextract function)
analysis_df_long$SOA<-numextract(analysis_df_long$time)

# View the data frame
View(analysis_df_long)

# Set the new variables (Pre/Post and SOA) as factors
analysis_df_long$SOA=factor(analysis_df_long$SOA, levels = c("70", "150", "230"))
analysis_df_long$Pre_Post=as.factor(analysis_df_long$Pre_Post)

# Set self reported sensory function as factors
analysis_df_long$ph102_W3=factor(analysis_df_long$ph102_W3, levels = c(5, 4, 3, 2, 1))# in previous analyses we recoded to reverse so higher was better across measures - do we need to here?
analysis_df_long$ph108_W3=factor(analysis_df_long$ph108_W3, levels = c(5, 4, 3, 2, 1))# in previous analyses we recoded to reverse so higher was better across measures - do we need to here?

# Set performance on control tasks as factors
analysis_df_long$Shams_0B2F_W3=factor(analysis_df_long$Shams_0B2F_W3, levels = c(0, 0.5, 1))
analysis_df_long$Shams_2B0F_70_W3=factor(analysis_df_long$Shams_2B0F_70_W3, levels = c(0, 0.5, 1))
analysis_df_long$Shams_1B1F_W3=factor(analysis_df_long$Shams_1B1F_W3, levels = c(0, 0.5, 1))

# Scale continuous/numeric predictors in model
analysis_df_long_scaled <- analysis_df_long
analysis_df_long_scaled$age_W3 <-scale(analysis_df_long_scaled$age_W3)
analysis_df_long_scaled$VAS_W3 <-scale(analysis_df_long_scaled$VAS_W3)
analysis_df_long_scaled$CRTmeancog_W3 <-scale(analysis_df_long_scaled$COGsartErrors3_W3)
analysis_df_long_scaled$CRTmeanmot_W3 <-scale(analysis_df_long_scaled$COGsartOmmissions_W3)

# Take into account how many trials per condition for a logistic model
analysis_df_long_scaled$nTrials<-2


#### Fit mixed models ####
# Fully Adjusted Models

# An additive model with omission aspect of SART  - Adjusted omission model
SOA_SARTo_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive <-glmer(
  Accuracy ~  age_W3 + COGsartOmmissions_W3 + sex_W3 + edu3_W3 + SOA + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# An additive model with commission aspect of SART  - Adjusted commission model
SOA_SARTe_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive <-glmer(
  Accuracy ~  age_W3 + COGsartErrors3_W3 + sex_W3 + edu3_W3 + SOA + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# Test significance of additive effects in adjusted models

# Likelihood ratio test: Adjusted Null vs. Adjusted omission model
anova(SOA_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive, SOA_SARTo_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive)

# Likelihood ratio test: Adjusted Null vs. Adjusted commission model
anova(SOA_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive, SOA_SARTe_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive)

#### Cross-sectional analysis of Colour Trails Task (CTT) task ####
# In this model we will explore the effect of various measures of the Color Trails Test (CTT) in relation to SIFI 
# We explore CTT1, CTT2 and the ratio score between the two 

# Make dataframes to be used in models
analysis_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial, Shams_2B1F_m230_W3, Shams_2B1F_m150_W3, Shams_2B1F_m70_W3, Shams_2B1F_70_W3, 
         Shams_2B1F_150_W3, Shams_2B1F_230_W3, 
         age_W3, sex_W3, edu3_W3, VAS_W3, ph108_W3, ph102_W3, Shams_1B1F_W3, Shams_2B0F_70_W3, 
         Shams_0B2F_W3,  COGtrail2time_W3, COGtrail1time_W3, COGtraildeltatime_W3)#, COGtraildifftime_W3)#delta is just the absolute value of the diff

#### Prep datafames for models ####

# Make a ratio score of CTT1 and CTT2 # a score higher than 1 indicates the participant was slower in CTT2 relative to CTT1
analysis_df$COGtrailratiotime_W3 <- analysis_df$COGtrail2time_W3/analysis_df$COGtrail1time_W3
View(cbind(analysis_df$COGtrail1time_W3, analysis_df$COGtrail2time_W3, analysis_df$COGtrailratiotime_W3))

# Reshape to long format for mixed model

# Rename the SIFI columns with standard formatting so that they can be reshaped
names(analysis_df)[names(analysis_df) ==  "Shams_2B1F_m230_W3"]<-"Pre_230"
names(analysis_df)[names(analysis_df) ==  "Shams_2B1F_m150_W3"]<-"Pre_150"
names(analysis_df)[names(analysis_df) ==  "Shams_2B1F_m70_W3"]<-"Pre_70"
names(analysis_df)[names(analysis_df) == "Shams_2B1F_70_W3"]<-"Post_70"
names(analysis_df)[names(analysis_df) == "Shams_2B1F_150_W3"]<-"Post_150"
names(analysis_df)[names(analysis_df) == "Shams_2B1F_230_W3"]<-"Post_230"

# View the dataframe
View(analysis_df)

# Reshape based on SOA - gives 6 rows per participant (-230, -150,-70, 70, 150, 230)
analysis_df_long<- reshape(analysis_df, idvar="tilda_serial",
                           varying = c("Pre_230","Pre_150", "Pre_70", "Post_70", "Post_150", "Post_230"),
                           v.name=c("Accuracy"),
                           times=c("Pre_230","Pre_150", "Pre_70", "Post_70", "Post_150", "Post_230"),
                           direction="long")

# Make a column for the Pre/Post factor (i.e. negative or positive SOAs)
analysis_df_long$Pre_Post <- as.numeric(grepl('Pre', analysis_df_long$time, ignore.case=T))

# Make an SOA column from the time column SOA (using custom numextract function)
analysis_df_long$SOA<-numextract(analysis_df_long$time)

# View the data frame
View(analysis_df_long)

# Set the new variables (Pre/Post and SOA) as factors
analysis_df_long$SOA=factor(analysis_df_long$SOA, levels = c("70", "150", "230"))
analysis_df_long$Pre_Post=as.factor(analysis_df_long$Pre_Post)

# Set self reported sensory function as factors
analysis_df_long$ph102_W3=factor(analysis_df_long$ph102_W3, levels = c(5, 4, 3, 2, 1))# in previous analyses we recoded to reverse so higher was better across measures - do we need to here?
analysis_df_long$ph108_W3=factor(analysis_df_long$ph108_W3, levels = c(5, 4, 3, 2, 1))# in previous analyses we recoded to reverse so higher was better across measures - do we need to here?

# Set performance on control tasks as factors
analysis_df_long$Shams_0B2F_W3=factor(analysis_df_long$Shams_0B2F_W3, levels = c(0, 0.5, 1))
analysis_df_long$Shams_2B0F_70_W3=factor(analysis_df_long$Shams_2B0F_70_W3, levels = c(0, 0.5, 1))
analysis_df_long$Shams_1B1F_W3=factor(analysis_df_long$Shams_1B1F_W3, levels = c(0, 0.5, 1))

# Scale continuous/numeric predictors in model
analysis_df_long_scaled <- analysis_df_long
analysis_df_long_scaled$age_W3 <-scale(analysis_df_long_scaled$age_W3)
analysis_df_long_scaled$VAS_W3 <-scale(analysis_df_long_scaled$VAS_W3)
analysis_df_long_scaled$COGtrail2time_W3 <-scale(analysis_df_long_scaled$COGtrail2time_W3)
analysis_df_long_scaled$COGtrail1time_W3 <-scale(analysis_df_long_scaled$COGtrail1time_W3)
analysis_df_long_scaled$COGtraildeltatime_W3 <-scale(analysis_df_long_scaled$COGtraildeltatime_W3)
analysis_df_long_scaled$COGtrailratiotime_W3 <-scale(analysis_df_long_scaled$COGtrailratiotime_W3)

# Take into account how many trials per condition for a logistic model
analysis_df_long_scaled$nTrials<-2


#### Fit mixed models ####
# Fully Adjusted Models

# An additive model with CTT1 aspect of CTT  - Adjusted CTT1 model
SOA_CTT1_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive <-glmer(
  Accuracy ~  age_W3 + COGtrail1time_W3 + sex_W3 + edu3_W3 + SOA + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# An additive model with CTT2 aspect of CTT  - Adjusted CTT2 model
SOA_CTT2_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive <-glmer(
  Accuracy ~  age_W3 + COGtrail2time_W3 + sex_W3 + edu3_W3 + SOA + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# An additive model with both CTT1 and CTT2 aspect of CTT  - Adjusted CTT1 + CTT2 model
SOA_CTT2_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive <-glmer(
  Accuracy ~  age_W3 + COGtrail1time_W3 + COGtrail2time_W3 + sex_W3 + edu3_W3 + SOA + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# Likelihood ratio: Adjusted Null model vs. Adjusted CTT1 model
anova(SOA_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive, SOA_CTT1_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive)

# Likelihood ratio: Adjusted Null model vs. Adjusted CTT2 model
anova(SOA_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive, SOA_CTT2_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive)

# An interaction model with CTT2 aspect of CTT  - Adjusted CTT2 * SOA model
SOA_CTT2_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_interaction <-glmer(
  Accuracy ~  age_W3 + COGtrail2time_W3*SOA + sex_W3 + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# Likelihood ratio: Adjusted CTT2 + SOA model vs. Adjusted CTT2 * SOA model
anova(SOA_CTT2_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_additive, SOA_CTT2_PP_age_sex_edu_VAS_SRv_SRh_controlSIFI_interaction)







# Note: why the warning below when plotting? - because factor header needs to be coerced to character for label?
# Warning messages:
#   1: In bind_rows_(x, .id) :
#   binding factor and character vector, coercing into character vector
# 2: In bind_rows_(x, .id) :
#   binding character and factor vector, coercing into character vector

# Can rename the plot labels before plotting 
dwplot(SOA_CTT2_PP_age_interaction) %>%
  relabel_predictors(c("age_W3" = "age",                       
                       "sex_W3Female" = "sex:Female", 
                       "edu3_W3Secondary" = "edu:Secondary", 
                       "edu3_W3Third/higher" = "edu:Third/Higher", 
                       "COGtrail2time_W3" = "CTT2_time", 
                       "SOA150" = "SOA150",
                       "SOA230" = "SOA230",
                       "Pre_Post1" = "Pre_Post1",
                       "COGtrail2time_W3:SOA150" = "CCT2_time:SOA150",
                       "COGtrail2time_W3:SOA230" = "CCT2_time:SOA230",
                       "sd_(intercept).tilda_serial" = "intercept"))


#### Supplementary analyses of CTT ####

# Additive model with both CTT1 and CTT2 time - CTT1 + CTT2 model
SOA_CTT2_CTT1_PP_age_additive <-glmer(
  Accuracy ~  age_W3 + sex_W3 + edu3_W3 +  COGtrail2time_W3 +  COGtrail1time_W3 + SOA + Pre_Post + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# Likelihood ratio test: CTT1 vs. CTT1 + CTT2 model
anova(SOA_CTT1_PP_age_additive, SOA_CTT2_CTT1_PP_age_additive)

# Likelihood ratio test: CTT2 vs. CTT1 + CTT2 model
anova(SOA_CTT2_PP_age_additive, SOA_CTT2_CTT1_PP_age_additive)

# Additive model with the CTT2/CTT1 ratio time - CTT2/CTT1 model
SOA_CTTratio_PP_age_additive <-glmer(
  Accuracy ~  age_W3 + sex_W3 + edu3_W3 +  COGtrailratiotime_W3 + SOA + Pre_Post + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# compare the CTT2/CTT1 ratio time to the null model # non significant
# Likelihood ratio test: Null model vs. CTT2/CTT1 model
anova(SOA_PP_age_sex_edu_additive, SOA_CTTratio_PP_age_additive)


