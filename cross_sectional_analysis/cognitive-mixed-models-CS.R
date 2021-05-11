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

#If plotting only we will only run full models and plot them, likelihood ratio tests willnot be performed
plotting_only <- TRUE

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

# Parameters for saving plots, including outpath
saveplot <- function(var, plot){
  ggsave(
    paste('/Users/rebeccahirst/Documents/TILDA_post_doc/Cognitive_function_paper/updated_analysis/Figures/dotwhiskers/',var,  '.pdf'),
    plot = plot,
    device = NULL,
    path = NULL,
    scale = 1,
    width = NA,
    height = NA,
    units = c("in", "cm", "mm"),
    dpi = 300,
    limitsize = TRUE,
  )
}

# custom dwplot settings 
mydwplot <- function(model, oldvar1, oldvar2, newvar1, newvar2, title){
  dwplot(model, dodge_size = 1, vline=geom_vline(xintercept=0, colour="grey60", linetype=2),dot_args = list(aes(shape = model)), show_intercept = TRUE)%>%
    relabel_predictors(oldvar1 = newvar1, 
                       oldvar2 = newvar2,
                       paste("SOA150:", oldvar1) = paste(newvar1, " * SOA [150]"),
                       paste("SOA230:", oldvar1) = paste(newvar1, " * SOA [230]"),
                       paste("SOA150:", oldvar2) = paste(newvar2, " * SOA [150]"),
                       paste("SOA230:", oldvar2) = paste(newvar2, " * SOA [230]"),
                       "age_W3" = "Age",
                       "age_W3:SOA150" = "Age * SOA [150]",
                       "age_W3:SOA230" = "Age * SOA [230]",
                       "sex_W3Female" = "Sex [Female]",
                       "SOA150:sex_W3Female" = "Sex [ Female] * SOA [150]",
                       "SOA230:sex_W3Female" = "Sex [ Female] * SOA [230]",
                       "edu3_W3Third/higher" = "Edu. [Third/Higher]",
                       "edu3_W3Secondary" = "Edu. [Secondary]",
                       "SOA150" = "SOA [150]", 
                       "SOA230" = "SOA [230]", 
                       "Pre_Post1" = "Pre/Post [Pre]", 
                       "VAS_W3" = "VAS",
                       "ph108_W34" = "SR. hearing [Fair]",
                       "ph108_W33" = "SR. hearing [Good]",
                       "ph108_W32" = "SR. hearing [V. Good]",
                       "ph108_W31" = "SR. hearing [Excellent]",
                       "ph102_W34" = "SR. vision [Fair]",
                       "ph102_W33" = "SR. vision [Good]",
                       "ph102_W32" = "SR. vision [V. Good]",
                       "ph102_W31" = "SR. vision [Excellent]",
                       "Shams_1B1F_W30.5" = "1B1F [0.5]",
                       "Shams_1B1F_W31" = "1B1F [1]",
                       "Shams_2B0F_70_W31" = "2B0F [1]",
                       "Shams_2B0F_70_W30.5" = "2B0F [0.5]",
                       "Shams_0B2F_W31" = "0B2F [1]",
                       "Shams_0B2F_W30.5" = "0B2F [0.5]",
                       "sd_(Intercept).tilda_serial" = "(Intercept).participant [SD]") + xlab("Coefficient") + ggtitle("Predicting Accuracy in 1B2F\nChoice Response Time")
}

#### Prep dataframe ####

# Make dataframes to be used in models (including co-variates to be included later)
# ph108_W3 - self reported hearing at wave 3
# ph102 - self reported vision at wave 3
analysis_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial, # participant ID
         Shams_2B1F_m230_W3, # accuracy on illusory 2 beep 1 flash (2B1F) SIFI condition at -230 SOA
         Shams_2B1F_m150_W3, # accuracy on 2B1F SIFI condition at -150 SOA
         Shams_2B1F_m70_W3, # accuracy on 2B1F SIFI condition at -70 SOA
         Shams_2B1F_70_W3, # accuracy on 2B1F SIFI condition at +70 SOA
         Shams_2B1F_150_W3, # accuracy on 2B1F SIFI condition at +150 SOA
         Shams_2B1F_230_W3, # accuracy on 2B1F SIFI condition at +230 SOA
         age_W3, # age in years at wave 3
         sex_W3, # sex
         edu3_W3, # eductation level
         VAS_W3, # visual acuity score (100 = 20/20 vision)
         ph108_W3, # self reported hearing at wave 3
         ph102_W3, # self reported vision at wave 3
         Shams_1B1F_W3, # accuracy on non-illusory 1B1F SIFI condition
         Shams_2B0F_70_W3,  # accuracy on non-illusory 2B0F (70 ms) SIFI condition
         Shams_0B2F_W3, # accuracy on non-illusory 0B2F (70 ms) SIFI condition
         CRTmeancog_W3, # CRT cognitive RT at wave 3
         CRTmeanmot_W3, # CRT motor RT at wave 3
         COGsartOmmissions_W3, # SART omission errors at wave 3
         COGsartErrors3_W3, # SART commission errors at wave 3
         COGtrail2time_W3, # CTT2 time
         COGtrail1time_W3, # CTT1 time
         COGtraildeltatime_W3) # CTT delta time

# Make a ratio score of CTT1 and CTT2 # score > 1 indicates slower in CTT2 relative to CTT1
analysis_df$COGtrailratiotime_W3 <- analysis_df$COGtrail2time_W3/analysis_df$COGtrail1time_W3

# View the ratio score against raw scores to check
View(cbind(analysis_df$COGtrail1time_W3, analysis_df$COGtrail2time_W3, analysis_df$COGtrailratiotime_W3))
View(cbind(analysis_df$COGtrail1time_W3, analysis_df$COGtrail2time_W3, analysis_df$COGtrailratiotime_W3, analysis_df$COGtraildeltatime_W3))

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

# Scale continuous/numeric predictors in model  
analysis_df_long_scaled <- analysis_df_long
analysis_df_long_scaled$age_W3 <-scale(analysis_df_long_scaled$age_W3)
analysis_df_long_scaled$VAS_W3 <-scale(analysis_df_long_scaled$VAS_W3)
analysis_df_long_scaled$CRTmeancog_W3 <-scale(analysis_df_long_scaled$CRTmeancog_W3)
analysis_df_long_scaled$CRTmeanmot_W3 <-scale(analysis_df_long_scaled$CRTmeanmot_W3)
analysis_df_long_scaled$COGsartErrors3_W3<-scale(analysis_df_long_scaled$COGsartErrors3_W3)
analysis_df_long_scaled$COGsartOmmissions_W3 <-scale(analysis_df_long_scaled$COGsartOmmissions_W3)
analysis_df_long_scaled$COGtrail2time_W3 <-scale(analysis_df_long_scaled$COGtrail2time_W3)
analysis_df_long_scaled$COGtrail1time_W3 <-scale(analysis_df_long_scaled$COGtrail1time_W3)
analysis_df_long_scaled$COGtraildeltatime_W3 <-scale(analysis_df_long_scaled$COGtraildeltatime_W3)
analysis_df_long_scaled$COGtrailratiotime_W3 <-scale(analysis_df_long_scaled$COGtrailratiotime_W3)

# Take into account how many trials per condition for a logistic model
analysis_df_long_scaled$nTrials<-2

#### 1. Cross-sectional analysis of Choice Reaction Time (CRT) task ####
# This set of models explore the effect of cognitive and motor elements of the CRT in relation to SIFI

#### 1.1 Fit mixed models ####
# Fully adjusted models

if(!plotting_only){
  # If we are only plotting we don't need all of the models (save time, only get full model)
  
  # Adjusted baseline interaction model for CRT: MRT * SOA + CRT + SOA + age *SOA + sex * SOA
  SOA_CRTmot_model <-glmer(
    Accuracy ~  age_W3 * SOA + CRTmeancog_W3 + CRTmeanmot_W3 * SOA + sex_W3 * SOA+ edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
      Shams_0B2F_W3 + (1|tilda_serial), 
    data = analysis_df_long_scaled, 
    family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  
  # Adjusted baseline interaction model for MRT: MRT + SOA + CRT * SOA + age *SOA + sex * SOA
  SOA_CRTcog_model <-glmer(
    Accuracy ~  age_W3 * SOA + CRTmeancog_W3 * SOA + CRTmeanmot_W3 + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
      Shams_0B2F_W3 + (1|tilda_serial), 
    data = analysis_df_long_scaled, 
    family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  
}

# Adjusted full interaction model: MRT * SOA + CRT * SOA + age *SOA + sex * SOA Note: can take a long time to converge
SOA_CRTcog_CRTmot_model <-glmer(
  Accuracy ~  age_W3 * SOA + CRTmeancog_W3 * SOA + CRTmeanmot_W3 * SOA + sex_W3 * SOA+ edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

#### 1.2 Test significance with likelihood ratio tests ####

if(!plotting_only){
  # If we are only plotting we don't need all of the models (save time, only get full model)
  
  # Likelihood ratio test: Adjusted baseline interaction model for CRT vs.  Adjusted full interaction model 
  'X2(2)= 19.924 p  = 4.715e-05 *** (Baseline; AIC = 28183, BIC = 28431; Full model; AIC = 28167, BIC = 28431)
  CRT * SOA interaction  significant when also controlling for the age * SOA interaction and the sex * SOA interaction'
  anova(SOA_CRTmot_model, SOA_CRTcog_CRTmot_model)
  
  # Likelihood ratio test: Adjusted baseline interaction model for MRT vs.  Adjusted full interaction model  - 
  'X2(2)= 69.585 p = 7.759e-16 *** (Baseline; AIC = 28232, BIC = 28481; Full model; AIC = 28167, BIC = 28431)
  MRT * SOA interaction still highly significant when also controlling for the age * SOA interaction '
  anova(SOA_CRTcog_model, SOA_CRTcog_CRTmot_model)
  
  # Summarize the full best model
  summary(SOA_CRTcog_CRTmot_model)
}

#### 1.3 Visualize results ####

# Dot whisker plot of full best model
# The most complex model
crt_dot_whisker <- mydwplot(SOA_CRTcog_CRTmot_model, "CRTmeancog_W3", "CRTmeanmot_W3", "CRT [Cognitive]", "CRT [Motor]", "Predicting Accuracy in 1B2F\nChoice Response Time")
saveplot('CRT', crt_dot_whisker)

#### 2. Cross-sectional analysis of Sustained Attention to Response Time (SART) task ####
# These models explore the error and omission elements of the SART in relation to SIFI 

#### 2.1 Fit mixed models ####
# Fully Adjusted Models

if(!plotting_only){
  # If we are only plotting we don't need all of the models (save time, only get full model)
  
  # Adjusted baseline interaction model for omissions: omissions + SOA + commissions * SOA + Age * SOA + sex *SOAA
  SOA_SARTcom_model <-glmer(
    Accuracy ~  age_W3 * SOA + COGsartOmmissions_W3 + COGsartErrors3_W3 * SOA + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
      Shams_0B2F_W3 + (1|tilda_serial), 
    data = analysis_df_long_scaled, 
    family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  
  # Adjusted baseline interaction model for commissions: commissions + SOA + omissions * SOA + Age * SOA + sex * SOA
  SOA_SARTom_model <-glmer(
    Accuracy ~  age_W3 * SOA + COGsartOmmissions_W3 * SOA + COGsartErrors3_W3 + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
      Shams_0B2F_W3 + (1|tilda_serial), 
    data = analysis_df_long_scaled, 
    family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  
}

# Adjusted full interaction model: commissions * SOA + omissions * SOA + Age * SOA + sex * SOA
SOA_SARTcom_SARTom_model <-glmer(
  Accuracy ~  age_W3 * SOA + COGsartOmmissions_W3 * SOA + COGsartErrors3_W3 * SOA + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

#### 2.2 Test significance with likelihood ratio tests ####
if(!plotting_only){
  # If we are only plotting we don't need all of the models (save time, only get full model)
  
  # Likelihood ratio test for commission * SOA: Adjusted commission + omission * SOA model + Age * SOA vs. Adjusted commissions * SOA + omissions * SOA + Age * SOA 
  'X2(2)= 8.7395 p  = 0.01265 * ( Baseline model; AIC = 28235, BIC= 28484; full model; AIC = 28230, BIC = 28494)
  Errors of commission do significantly improve model fit whilst controlling for interaction with age
  but not at the corrected alpha criterion level used to adjust for multiple comparisons'
  anova(SOA_SARTom_model, SOA_SARTcom_SARTom_model)
  
  # Likelihood ratio test for omission * SOA: Adjusted commission + omission * SOA model + Age * SOA vs. Adjusted commissions * SOA + omissions * SOA + Age * SOA 
  'X2(2)= 22.855 p  = 1.089e-05 *** (Baseline; AIC = 28249 , BIC= 28498; full model; AIC = 28230, BIC = 28494)
  Errors of omission significantly improve model fit whilst controlling for interaction with age'
  anova(SOA_SARTcom_model, SOA_SARTcom_SARTom_model)
  
}

#### 2.3 Visualize results ####

# The most complex model
summary(SOA_SARTcom_SARTom_model)
sart_dot_whisker <- mydwplot(SOA_SARTcom_SARTom_model, "COGsartOmmissions_W3", "COGsartErrors3_W3", "SART [Ommission]", "SART [Commission]", "Predicting Accuracy in 1B2F\nSustained Attention to Response Task")
saveplot('SART', sart_dot_whisker)

#### 3. Cross-sectional analysis of Colour Trails Task (CTT) task ####
# In this model we will explore the effect of various measures of the Color Trails Test (CTT) in relation to SIFI 
# We explore CTT1, CTT2 and the ratio score between the two 

#### 3.1 Fit mixed models ####
# Fully Adjusted Models

' Initially fitted models in same way as other models, but noted models failed to converge: See below

# Adjusted baseline interaction model for CTT2: CTT1 * SOA + CTT2 + SOA + Age * SOA - FAILED CONVERGENCE
SOA_CTT1_model_original <-glmer(
  Accuracy ~  age_W3 * SOA + COGtrail1time_W3 * SOA + COGtrail2time_W3 + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# Adjusted baseline interaction model for CTT1: CTT1 + SOA + CTT2 * SOA + Age * SOA
SOA_CTT2_model_original <-glmer(
  Accuracy ~  age_W3 * SOA + COGtrail1time_W3 + COGtrail2time_W3 * SOA + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# Adjusted full interaction model: CTT1 * SOA + CTT2 * SOA + Age * SOA + SOA * sex - FAILED CONVERGENCE
SOA_CTT1_CTT2_model_original <-glmer(
  Accuracy ~  age_W3 * SOA + COGtrail1time_W3 * SOA + COGtrail2time_W3 * SOA + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
'

# Because models with CTT1 failed to converge we need to simplify the models
# Here, we do not include CTT2 and CTT1 in the same model use CTTdelta in place of CTT2 (which allows us to examine basic processing speed and slowing caused by CTT2 whilst controlling for the other factor)

if(!plotting_only){
  # If we are only plotting we don't need all of the models (save time, only get full model)
  
  # Baseline CTT1 whilst controlling for delta interaction model 
  SOA_CTT1_model_original <-glmer(
    Accuracy ~  age_W3 * SOA + COGtrail1time_W3 + COGtraildeltatime_W3 * SOA + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
      Shams_0B2F_W3 + (1|tilda_serial), 
    data = analysis_df_long_scaled, 
    family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  
  # Baseline CTT delta whilst controlling for CTT interaction model 
  SOA_CTTdelta_model_original <-glmer(
    Accuracy ~  age_W3 * SOA + COGtrail1time_W3*SOA + COGtraildeltatime_W3 + SOA + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
      Shams_0B2F_W3 + (1|tilda_serial), 
    data = analysis_df_long_scaled, 
    family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  
}

# Full CTT1*SOA and delta * SOA interaction model 
SOA_CTT1_CTTdelta_model_original <-glmer(
  Accuracy ~  age_W3 * SOA + COGtrail1time_W3*SOA + COGtraildeltatime_W3 * SOA + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))


#### 3.2 Test significance with likelihood ratio tests ####
if(!plotting_only){
  # If we are only plotting we don't need all of the models (save time, only get full model)
  # Likelihood ratio test for CTTdelta * SOA whilst controlling for CTT1 * SOA: Adjusted CTTdelta SOA + Age *SOA model + Sex * SOA vs. Adjusted CTTdelta * SOA + Age *SOA + Sex * SOA
  'X2(2) = 79.369  p  < 2.2e-16 *** (CTTdelta + SOA AIC = 28458, BIC = 28458  CTTdelta * SOA AIC = 28135, BIC = 28398)
CTT1, processing speed significantly improves model fit whilst controlling the delta * SOA term'
  anova(SOA_CTT1_model_original, SOA_CTT1_CTTdelta_model_original)
  
  # Likelihood ratio test for CTT1 * SOA whilst controlling for CTTdelta * SOA: Adjusted CTT1 SOA + Age *SOA model + Sex * SOA vs. Adjusted CTT1 * SOA + Age *SOA + Sex * SOA
  'X2(2) = 67.439  p  = 2.269e-15 ***(CTTdelta + SOA AIC = 28198, BIC = 28446  CTTdelta * SOA AIC = 28135, BIC = 28398)
Delta, which represents the slowing caused by distractor circles in CTT2 significantly improves model fit whilst controlling the CTT1*SOA term'
  anova(SOA_CTTdelta_model_original, SOA_CTT1_CTTdelta_model_original)
  
}

#### 3.3 Visualize results ####
# The most complex model

# Note: sd_(Intercept).tilda_serial indicates the standard deviation of the random effect, which shows us how likely 
# an individual is to deviate from the standard intercept.
# "The idea of a random intercept and a random slope indicate that any given subject
# will “wiggle” a bit around this mean regression line both up or down (random
# intercept) and clockwise or counterclockwise (random slope). The variances (and
# therefore standard deviations) of the random effects determine the sizes of typical
# deviations from the mean intercept and slope" https://www.stat.cmu.edu/~hseltman/309/Book/chapter15.pdf
# https://www.theanalysisfactor.com/understanding-random-effects-in-mixed-models/
ctt_dot_whisker <- mydwplot(SOA_CTT1_CTTdelta_model_original, "COGtrail1time_W3", "COGtraildeltatime_W3", "CTT1", "CTT delta", "Predicting Accuracy in 1B2F\nColor Trails Test")
saveplot('CTT', ctt_dot_whisker)
