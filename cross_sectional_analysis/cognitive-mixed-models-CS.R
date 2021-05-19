" Run logistic mixed effects models to predict accuracy on SIFI from 
the cross-sectional cognitive measures available at the wave 3 healthcase assessment.

Author(s): Hirst R J 

This script should be run following select_cross_sectional.R and select_longitudinal.R (which select participants included in analysis)

The analysis comprises of three parts, with similar structure for each:
  1. Cross-sectional analysis of Choice Reaction Time (CRT)
  2. Cross-sectional analysis of Sustained Attention to Response Task (SART)
  3. Cross-sectional analysis of Colour Trials Test (CTT)

All models are run fully adjusted for covariates. 

NOTE: make a folder in your working directory named 'figures' and 'tables' to automatically save results in pdf/word tables

"

#If plotting only we will only run full models and plot them, likelihood ratio tests willnot be performed
plotting_only <- FALSE

#### import libraries ####

library(lme4) # lme4 library for the mixed effects models
library(dotwhisker) # for dot whisker plots of results 
library(sjPlot) #For making html tables of results

#### Functions ####
# A set of custom functions used throughout analysis

# Extract number from a string
library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# Automatically save plots to a directory named figures/tables in the current working directory
current_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
path_breaks <- which(strsplit(current_directory, "")[[1]]=="/")
plot_outpath<- paste(substr(current_directory, start = 1, stop = path_breaks[length(path_breaks)]),'figures/', sep = '')
table_outpath<- paste(substr(current_directory, start = 1, stop = path_breaks[length(path_breaks)]),'tables/', sep = '')

saveplot <- function(var, plot){
  ggsave(
    paste(plot_outpath, var,  '.pdf', sep = ''),
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
# myplot
myplot <- function(model, var1, var2){
  plot_model(model, dot.size = 1,
             axis.labels = rev(c("Age", "SOA [150]", "SOA [230]", var1, var2, "Sex [Female]", "Education [Secondary]",
                                 "Education [Third/Higher]", "Pre/Post [Pre]", "VAS", "SR. hearing [Fair]", "SR. hearing [Good]",
                                 "SR. hearing [Very Good]", "SR. hearing [Excellent]", "SR. vision [Fair]", "SR. vision [Good]",
                                 "SR. vision [Very Good]", "SR. vision [Excellent]", "1B1F [0.5]", "1B1F [1]",
                                 "2B0F [0.5]", "2B0F [1]", "0B2F [0.5]", "0B2F [1]", "Age * SOA [150]",
                                 "Age * SOA [230]", paste("SOA [150] * ", var1), paste("SOA [230] * ", var1),
                                 paste("SOA [150] * ", var2), paste("SOA [230] * ", var2), "Sex [Female] * SOA [150]",
                                 "Sex [Female] * SOA [230]")))+ ggtitle(paste("Predicting Accuracy in 1B2F\n", var1,'and', var2))
}

# mytable
mytable <- function(model, var1, var2, plotname){
  tab_model(model, file = paste(table_outpath, plotname, '.doc', sep = ''),
             pred.labels = c("Intercept", "Age", "SOA [150]", "SOA [230]", var1, var2, "Sex [Female]", "Education [Secondary]",
                                 "Education [Third/Higher]", "Pre/Post [Pre]", "VAS", "SR. hearing [Fair]", "SR. hearing [Good]",
                                 "SR. hearing [Very Good]", "SR. hearing [Excellent]", "SR. vision [Fair]", "SR. vision [Good]",
                                 "SR. vision [Very Good]", "SR. vision [Excellent]", "1B1F [0.5]", "1B1F [1]",
                                 "2B0F [0.5]", "2B0F [1]", "0B2F [0.5]", "0B2F [1]", "Age * SOA [150]",
                                 "Age * SOA [230]", paste("SOA [150] * ", var1), paste("SOA [230] * ", var1),
                                 paste("SOA [150] * ", var2), paste("SOA [230] * ", var2), "Sex [Female] * SOA [150]",
                                 "Sex [Female] * SOA [230]"))
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
  'X2(2)= 18.306 p  = 0.0001059 (Baseline; AIC = 28550, BIC = 28798; Full model; AIC = 28536, BIC = 28800)
  CRT * SOA interaction  significant when also controlling for the age * SOA interaction and the sex * SOA interaction'
  anova(SOA_CRTmot_model, SOA_CRTcog_CRTmot_model)
  
  # Likelihood ratio test: Adjusted baseline interaction model for MRT vs.  Adjusted full interaction model  - 
  'X2(2)= 75.641 p < 2.2e-16 *** (Baseline; AIC = 28607, BIC = 28856; Full model; AIC = 28536, BIC = 28800)
  MRT * SOA interaction still highly significant when also controlling for the age * SOA interaction '
  anova(SOA_CRTcog_model, SOA_CRTcog_CRTmot_model)
  
  # Summarize the full best model
  summary(SOA_CRTcog_CRTmot_model)
}

#### 1.3 Visualize results ####

# Dot whisker plot of full best model
# The most complex model
# The most complex model
mytable(SOA_CRTcog_CRTmot_model, 'CRT [Cognitive]', 'CRT [Motor]', 'CRT')

sart_oddsratio <-myplot(SOA_CRTcog_CRTmot_model, 'CRT [Cognitive]', 'CRT [Motor]')
saveplot('SART', sart_oddsratio)


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
  'X2(2)= 9.0502 p  = 0.01083 * ( Baseline model; AIC = 28610, BIC= 28859; full model; AIC = 28605, BIC = 28870)
  Errors of commission do significantly improve model fit whilst controlling for interaction with age
  but not at the corrected alpha criterion level used to adjust for multiple comparisons'
  anova(SOA_SARTom_model, SOA_SARTcom_SARTom_model)
  
  # Likelihood ratio test for omission * SOA: Adjusted commission + omission * SOA model + Age * SOA vs. Adjusted commissions * SOA + omissions * SOA + Age * SOA 
  'X2(2)= 22.461p  = 1.327e-05 *** (Baseline; AIC = 28624, BIC= 28872; full model; AIC = 28605, BIC = 28870)
  Errors of omission significantly improve model fit whilst controlling for interaction with age'
  anova(SOA_SARTcom_model, SOA_SARTcom_SARTom_model)
  
}

#### 2.3 Visualize results ####

# The most complex model
mytable(SOA_SARTcom_SARTom_model, 'SART ommissions', 'SART comissions', 'SART')

sart_oddsratio <-myplot(SOA_SARTcom_SARTom_model, 'SART ommissions', 'SART comissions')
saveplot('SART', sart_oddsratio)

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
  SOA_CTT1_model <-glmer(
    Accuracy ~  age_W3 * SOA + COGtrail1time_W3 + COGtraildeltatime_W3 * SOA + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
      Shams_0B2F_W3 + (1|tilda_serial), 
    data = analysis_df_long_scaled, 
    family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  
  # Baseline CTT delta whilst controlling for CTT interaction model 
  SOA_CTTdelta_model <-glmer(
    Accuracy ~  age_W3 * SOA + COGtrail1time_W3*SOA + COGtraildeltatime_W3 + SOA + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
      Shams_0B2F_W3 + (1|tilda_serial), 
    data = analysis_df_long_scaled, 
    family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  
}

# Full CTT1*SOA and delta * SOA interaction model 
SOA_CTT1_CTTdelta_model <-glmer(
  Accuracy ~  age_W3 * SOA + COGtrail1time_W3*SOA + COGtraildeltatime_W3 * SOA + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))


#### 3.2 Test significance with likelihood ratio tests ####
if(!plotting_only){
  # If we are only plotting we don't need all of the models (save time, only get full model)
  # Likelihood ratio test for CTTdelta * SOA whilst controlling for CTT1 * SOA: Adjusted CTTdelta SOA + Age *SOA model + Sex * SOA vs. Adjusted CTTdelta * SOA + Age *SOA + Sex * SOA
  'X2(2) = 87.396  p  < 2.2e-16 ***(CTTdelta + SOA AIC = 28575, BIC = 28823 CTTdelta * SOA AIC = 28491, BIC = 28755)
CTT1, processing speed significantly improves model fit whilst controlling the delta * SOA term'
  anova(SOA_CTT1_model, SOA_CTT1_CTTdelta_model)
  
  # Likelihood ratio test for CTT1 * SOA whilst controlling for CTTdelta * SOA: Adjusted CTT1 SOA + Age *SOA model + Sex * SOA vs. Adjusted CTT1 * SOA + Age *SOA + Sex * SOA
  'X2(2) = 74.02  p  = < 2.2e-16 ***(CTTdelta + SOA AIC = 28561, BIC = 28810 CTTdelta * SOA AIC = 28491, BIC = 28755)
Delta, which represents the slowing caused by distractor circles in CTT2 significantly improves model fit whilst controlling the CTT1*SOA term'
  anova(SOA_CTTdelta_model, SOA_CTT1_CTTdelta_model)
  
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

# Saves word table of final model - NOTE: saves odds ratio not estimates
# The coefficients are in this case automatically converted (exponentiated). 
mytable(SOA_CTT1_CTTdelta_model_original, 'CTT1', 'CTT delta', 'CTT')
# Also save non transformed (log odds)
#tab_model(SOA_CTT1_CTTdelta_model_original, file = paste(table_outpath, 'CTT-non-transformed.doc'), transform = NULL)
# https://strengejacke.github.io/sjPlot/articles/plot_model_estimates.html
#plot_model(SOA_CTT1_CTTdelta_model_original, dot.size = 1)
# save a plot of the odds ratios
ctt_oddsratio <-myplot(SOA_CTT1_CTTdelta_model_original, 'CTT1', 'CTT delta')
saveplot('CTT', ctt_oddsratio)