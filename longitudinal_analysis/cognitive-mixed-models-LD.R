" Run logistic mixed effects models to predict accuracy on SIFI from 
the cognitive groups derived from cognitive-kml.R

Author: Hirst R J 

Run cognitive-kml.R in advance of this for a) immediate recall b) delayed recall and c) animal naming
On each run cognitive-kml will return a value df_with_clusters, use this to set 
delayed_recall_groups, animal_naming_groups and immediate_recall_groups respectively. 

"

# Import libraries
library(lme4) # for mixed effects models
library(qwraps2) # for formatted summary tables


# Initialize functions
# Function to extract number from a string (used for generating group labels)
library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# Dataframes containing each of the cognitive groups (first run cognitive-kml with each variable to get df_with_clusters)

delayed_recall_groups <- df_with_clusters # set this after running cognitive-kml.R with delayed recall as df
animal_naming_groups <- df_with_clusters # set this after running cognitive-kml.R with animal naming as df
immediate_recall_groups <- df_with_clusters # set this after running cognitive-kml.R with animal naming as df


#### make dataframes to be used in models ####

# a dataframe of accuracy on the main illusory conditions (2B1F) (tilda_dataW3W1W2W4W5 is created in select_longitudinal)
Accuracy_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial, Shams_2B1F_m230_W3, Shams_2B1F_m150_W3, Shams_2B1F_m70_W3, Shams_2B1F_70_W3, Shams_2B1F_150_W3, Shams_2B1F_230_W3)

# a dataframe with predictors for adjustment (as measured at same wave as SIFI)
predictor_df <-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial, age_W3, sex_W3, edu3_W3, VAS_W3, ph108_W3, ph102_W3, Shams_1B1F_W3, Shams_2B0F_70_W3, 
         Shams_0B2F_W3)

#### merge cognitive groups with full dataframe ####

# select the cognitive measure you want to focus on in this analysis 

#analysis_df<-merge(Accuracy_df, animal_naming_groups , by='tilda_serial')
#analysis_df<-merge(Accuracy_df, delayed_recall_groups, by='tilda_serial')
analysis_df<-merge(Accuracy_df, immediate_recall_groups, by='tilda_serial')
analysis_df<-merge(analysis_df, predictor_df, by='tilda_serial') # add predictors to the dataframe
# look at the data frame
View(analysis_df)


#### tabulate the demographics of each cognitive trajectory group ####

table(analysis_df$age_W3, analysis_df$nC3)
options(qwraps2_markup = "markdown")
data <- analysis_df
demographic_summary <-
  list("Age" =
         list("min" = ~ min(.data$age_W3),
              "max" = ~ max(.data$age_W3),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$age_W3)),
       "Sex" =
         list("Female" = ~ qwraps2::n_perc0(.data$sex_W3 == "Female"),
              "Male"  = ~ qwraps2::n_perc0(.data$sex_W3 == "Male")),
       "Education" =
         list("Primary/none" = ~ qwraps2::n_perc0(.data$edu3_W3 == "Primary/none"),
              "Secondary"  = ~ qwraps2::n_perc0(.data$edu3_W3 == "Secondary"),
              "Third/higher"  = ~ qwraps2::n_perc0(.data$edu3_W3 == "Third/higher")))

demo_table <- summary_table(dplyr::group_by(data, nC3), demographic_summary)
View(demo_table)

#### reshape to long format for mixed model ####

# rename the SIFI columns with standard formatting so that they can be reshaped
names(analysis_df)[names(analysis_df) ==  "Shams_2B1F_m230_W3"]<-"Pre_230"
names(analysis_df)[names(analysis_df) ==  "Shams_2B1F_m150_W3"]<-"Pre_150"
names(analysis_df)[names(analysis_df) ==  "Shams_2B1F_m70_W3"]<-"Pre_70"
names(analysis_df)[names(analysis_df) == "Shams_2B1F_70_W3"]<-"Post_70"
names(analysis_df)[names(analysis_df) == "Shams_2B1F_150_W3"]<-"Post_150"
names(analysis_df)[names(analysis_df) == "Shams_2B1F_230_W3"]<-"Post_230"

# view the dataframe
View(analysis_df)

# Reshape based on SOA - gives 6 rows per participant
analysis_df_long<- reshape(analysis_df, idvar="tilda_serial",
                           varying = c("Pre_230","Pre_150", "Pre_70", "Post_70", "Post_150", "Post_230"),
                           v.name=c("Accuracy"),
                           times=c("Pre_230","Pre_150", "Pre_70", "Post_70", "Post_150", "Post_230"),
                           direction="long")

# Make a column for the Pre/Post factor that is created from the time column
analysis_df_long$Pre_Post <- as.numeric(grepl('Pre', analysis_df_long$time, ignore.case=T))

# Make an SOA column from the time column SOA (uses custom numextract function)
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

# Re-level the cognitive group factor so that the most cognitively healthy group is the reference
analysis_df_long$nC3_orig <- analysis_df_long$nC3 # keep the original variable for reference

# The reference level for the factor "group" depends on the cognitive measure being analysed 
# - use relevant re-level accordingly

# For verbal fluency, animal_naming, group C is the healthiest, so they will be set as reference
analysis_df_long$nC3 <- relevel(analysis_df_long$nC3, ref = "C")

# For immediate and delayed recall, groups B are the healthiest, so they will be set as reference
analysis_df_long$nC3 <- relevel(analysis_df_long$nC3, ref = "B")

# View the dataframe
View(analysis_df_long)

#### Fit non adjusted mixed models  (with and without optimizers) ####
# Note: these models are reported in supplementary material, not the main manuscript

# Each model is run with and without an optimizer parameter to assess the most parsimonious approach 
# (i.e. the approach that enables convergence across most models.)

# Take into account how many trials per condition for a logistic model
analysis_df_long_scaled$nTrials<-2

# View the dataframe
View(analysis_df_long_scaled)

# Start with most basic model and gradually add variables of interest
# If convergence fails we will consider run parallel models for the Pre/Post factor as in Hernandez, Setti et al 2019
# First each model will be fitted with and without an optimization parameter. The model most parsimonious 
# approach in terms of convergence/interpretability will be reported. 

#### Basic model 1. effect of SOA on accuracy ####

# with optimizer
SOA_model <-glmer(
  Accuracy ~  SOA + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# without optimizer # same result

SOA_model_no <-glmer(
  Accuracy ~  SOA + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials)

#### Basic model 2. effect of SOA and cognitive trajectory group on accuracy (additive) ####

# with optimizer

SOA_nC3_additive <-glmer(
  Accuracy ~  nC3 + SOA + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# without optimizer # same result

SOA_nC3_additive <-glmer(
  Accuracy ~  nC3 + SOA + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials)


#### Basic model 3. effect of SOA and cognitive trajectory group on accuracy (with interaction) ####

# with optimizer

SOA_nC3_interaction <-glmer(
  Accuracy ~  nC3 * SOA + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# without optimizer # fails to converge

SOA_nC3_interaction <-glmer(
  Accuracy ~  nC3 * SOA + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials)


#### Basic model 4. effect of SOA, Pre/Post and cognitive trajectory group on accuracy (additive) ####

# with optimizer

SOA_nC3_PP_additive <-glmer(
  Accuracy ~  nC3 + SOA + Pre_Post + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# without optimizer

SOA_nC3_PP_additive <-glmer(
  Accuracy ~  nC3 + SOA + Pre_Post + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials)

#### Basic model 5. effect of SOA, Pre/Post and cognitive trajectory group on accuracy (with interaction term) ####

# with optimizer

SOA_nC3_PP_interaction <-glmer(
  Accuracy ~  nC3*SOA + Pre_Post + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# without an optimizer # fails to converge

SOA_nC3_PP_interaction <-glmer(
  Accuracy ~  nC3*SOA + Pre_Post + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials)

#### Model 5b. additive model without cognitive trajectory group factor ####

# with optimizer

SOA_PP_age_additive <-glmer(
  Accuracy ~  scale(age_W3) + SOA + Pre_Post + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# without optimizer 

SOA_PP_age_additive_no <-glmer(
  Accuracy ~  scale(age_W3) + SOA + Pre_Post + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials)

#### Model 6. effect of SOA, AGE and cognitive trajectory group on accuracy (additive) ####
# start introducing predictors into the model AGE SCALED

# with optimizer

SOA_nC3_PP_age_additive <-glmer(
  Accuracy ~  scale(age_W3) + nC3 + SOA + Pre_Post + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# without optimizer # failed to converge

SOA_nC3_PP_age_additive <-glmer(
  Accuracy ~  scale(age_W3) + nC3 + SOA + Pre_Post + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials)

#### Model 7. effect of SOA, age and cognitive trajectory group on accuracy - cog and SOA interaction ####

# with optimizer

SOA_nC3_PP_age_interaction <-glmer(
  Accuracy ~  scale(age_W3) + nC3*SOA + Pre_Post + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(SOA_nC3_PP_age_interaction)# show a summary of the model 
# without optimizer # failed to converge

SOA_nC3_PP_age_interaction<-glmer(
  Accuracy ~  scale(age_W3) + nC3*SOA + Pre_Post + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials)

#### likelihood ratio tests ####

# For models that converged. Assess significance of fixed effects with likelihood ratio tests.

# compare the full additive model  with cognitive trajectory group, to the null model, without cognitive trajectory group
anova(SOA_PP_age_additive, SOA_nC3_PP_age_additive) # cognitive group improves model fit

# compare the full additive model to a model with an interaction term 
anova(SOA_nC3_PP_age_additive, SOA_nC3_PP_age_interaction) # interaction improves model fit

#### plot model results ####

# plot the results of the models that did converge
a<-ggpredict(SOA_nC3_PP_age_interaction, c("SOA", "nC3"))

#make plot in black and white
plot(a,connect.lines=TRUE, limits=c(0, 1), colors="bw", dot.size = 3)+labs(x = "Stimulus Onset Asynchrony (ms)", shape=" ")

# dotwhisker plot of full model

library(dotwhisker)
dwplot(SOA_nC3_PP_age_interaction)

dwplot(SOA_nC3_PP_age_interaction,dodge_size = 1, vline=geom_vline(xintercept=0, colour="grey60", linetype=2),dot_args = list(aes(shape = model)), show_intercept = TRUE)


#### Fit adjusted mixed models  (with optimizer) ####
# Note: these are the models reported in the main manuscript 

# analysis of non adjusted models showed that inclusion of an optimizer was most parsimonious 
