" Run logistic mixed effects models to predict accuracy on SIFI from 
the cognitive groups derived from cognitive-kml.R

Author: Hirst R J 

Run cognitive-kml.R in advance of this for a) immediate recall b) delayed recall and c) animal naming
On each run cognitive-kml will return a value df_with_clusters, use this to set 
delayed_recall_groups, animal_naming_groups and immediate_recall_groups respectively. 

"

# set to "animal naming", "delayed recall" or "immediate recall"
this_cog_measure <- "immediate recall"

#If plotting only we will only run full models and plot them, likelihood ratio tests willnot be performed
plotting_only <- FALSE

#### Import libraries ####
library(lme4) # for mixed effects models
library(qwraps2) # for formatted summary tables
library(dotwhisker) # for dwplot 
library(chisq.posthoc.test) # for posthoc comparisons of chi squared
library(car) # for levenes test
library(FSA) # for dunns test
library(sjPlot) #For making html tables of results

#### Functions ####
# A set of custom functions used throughout analysis

# Extract number from a string
library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# Convert strings to Title case
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Parameters for saving plots, including outpath
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
# myplot_reduced (to limit the terms plotted - for slide presentations)
myplot_reduced <- function(model, var1, var2, var1_newname, var2_newname){
  plot_model(model, terms = c("age_W3", "SOA [150]", "SOA [230]", "sex_W3 [Female]", "age_W3:SOA [150]", 
                              "age_W3:SOA [230]", "SOA150:sex_W3Female", "SOA230:sex_W3Female", 
                              paste("SOA230:", var1, sep = ''), 
                              paste("SOA150:", var2, sep = ''), 
                              paste("SOA230:", var2, sep = ''), 
                              paste("SOA150:", var1, sep = ''), var1, var2),
             axis.labels = rev(c("Age", "SOA [150]", "SOA [230]", var1_newname, var2_newname, "Sex [Female]", "Age * SOA [150]",
                                 "Age * SOA [230]", paste("SOA [150] * ", var1_newname), paste("SOA [230] * ", var1_newname),
                                 paste("SOA [150] * ", var2_newname), paste("SOA [230] * ", var2_newname), "Sex [Female] * SOA [150]",
                                 "Sex [Female] * SOA [230]"))) + ggtitle(paste("Predicting Accuracy in 1B2F\n",str_to_title(this_cog_measure), sep = ''))
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

# dataframes containing each of the cognitive groups (first run cognitive-kml with each variable to get df_with_clusters)
if(this_cog_measure == "animal naming"){
  animal_naming_groups <- df_with_clusters # set this after running cognitive-kml.R with animal naming as df
}else if(this_cog_measure == "delayed recall"){
  delayed_recall_groups <- df_with_clusters # set this after running cognitive-kml.R with delayed recall as df
}else if (this_cog_measure == "immediate recall"){
  immediate_recall_groups <- df_with_clusters # set this after running cognitive-kml.R with animal naming as df
}else{warning(paste(this_cog_measure, " is not a group please use 'animal naming', 'delayed recall' or 'immediate recall'"))}

# A dataframe of accuracy on the main illusory conditions (2B1F) (tilda_dataW3W1W2W4W5 is created in select_longitudinal)
predictor_df<-tilda_dataW3W1W2W4W5%>% 
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

# Merge cognitive groups with full dataframe

# Select the cognitive measure you want to focus on in this analysis 
if(this_cog_measure == "animal naming"){
  analysis_df<-merge(predictor_df, animal_naming_groups , by='tilda_serial')
}else if(this_cog_measure == "delayed recall"){
  analysis_df<-merge(predictor_df, delayed_recall_groups, by='tilda_serial')
}else if (this_cog_measure == "immediate recall"){
  analysis_df<-merge(predictor_df, immediate_recall_groups, by='tilda_serial')
}else{warning(paste(this_cog_measure, " is not a group please use 'animal naming', 'delayed recall' or 'immediate recall'"))}

# View the data frame
View(analysis_df)

#### Tabulate the demographics  ####
# Show demographics of each cognitive trajectory group 

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

#### Test significant differences in age between groups ####

# Compute the analysis of variance
res.aov <- aov(age_W3 ~ nC3, data = analysis_df)
# Summary of the analysis
summary(res.aov)

# Assumption checks
# 1. Homogeneity of variances
plot(res.aov, 1)

# Levenes test for homogeneity of variance 
leveneTest(age_W3 ~ nC3, data = analysis_df)

# 2. Normality
plot(res.aov, 2)
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

' Use non parametric test to account for deviations from assumptions'
# non parametric anova
kruskal.test(age_W3 ~ nC3, data = analysis_df)

dunnTest(age_W3 ~ nC3, data = analysis_df,
              method="bh")    # Can adjust p-values;
# See ?p.adjust for options

"
Delayed Recall:
	Kruskal-Wallis rank sum test

data:  age_W3 by nC3
Kruskal-Wallis chi-squared = 291.13, df = 2, p-value < 2.2e-16

  Comparison          Z      P.unadj        P.adj
1      A - B   8.271645 1.321231e-16 1.321231e-16
2      A - C -10.281444 8.543753e-25 1.281563e-24
3      B - C -17.046473 3.712732e-65 1.113820e-64

Immediate recall
	Kruskal-Wallis rank sum test

data:  age_W3 by nC3
Kruskal-Wallis chi-squared = 311.06, df = 2, p-value < 2.2e-16
Dunn (1964) Kruskal-Wallis multiple comparison
  p-values adjusted with the Benjamini-Hochberg method.

  Comparison          Z      P.unadj        P.adj
1      A - B   9.110981 8.164075e-20 8.164075e-20
2      A - C -10.472041 1.161117e-25 1.741676e-25
3      B - C -17.581013 3.443658e-69 1.033097e-68

Animal naming:
	Kruskal-Wallis rank sum test

data:  age_W3 by nC3
Kruskal-Wallis chi-squared = 138.18, df = 2, p-value < 2.2e-16

Dunn (1964) Kruskal-Wallis multiple comparison
  p-values adjusted with the Benjamini-Hochberg method.

  Comparison         Z      P.unadj        P.adj
1      A - B -8.121516 4.603957e-16 6.905935e-16
2      A - C  4.897400 9.711312e-07 9.711312e-07
3      B - C 11.310463 1.164728e-29 3.494184e-29
"

boxplot(age_W3 ~ nC3, data = analysis_df,
        ylab="Age at wave 3",
        xlab="Cognitive group")

#### Test significant differences in sex between groups ####

# Tabulate sex
sextable <-table(analysis_df$nC3, analysis_df$sex_W3)

# Observed frequencies
obsfreq <- matrix(c(sextable[1, 2], sextable[1, 1], sextable[2, 2], sextable[2, 1], sextable[3, 2], sextable[3, 1]),nrow=2,ncol=3)

# Chi squared test
chisq.results <- chisq.test(obsfreq)

# Transpose table to conduct posthoc tests using chisq.posthoc.test package
sextable_transposed <- as.table(rbind(c(sextable[1, 2], sextable[2, 2], sextable[3, 2]), c(sextable[1, 1], sextable[2, 1], sextable[3, 1])))
dimnames(sextable_transposed) <- list(sex = c("F", "M"),
                    cogGroup = c("A","B", "C"))

# Run test and posthoc test
chisq.test(sextable_transposed) # sanity check against original result
chisq.posthoc.test(sextable_transposed, method = "bonferroni")
"
Animal Naming: 

	Pearson's Chi-squared test

data:  sextable_transposed
X-squared = 0.7548, df = 2, p-value = 0.6856


Delayed recall

	Pearson's Chi-squared test

data:  sextable_transposed
X-squared = 128.81, df = 2, p-value < 2.2e-16

  Dimension     Value         A         B         C
1         F Residuals -1.894657  9.835303 -9.168617
2         F  p values  0.348827  0.000000  0.000000
3         M Residuals  1.894657 -9.835303  9.168617
4         M  p values  0.348827  0.000000  0.000000

Immediate recall:
> chisq.test(sextable_transposed) # sanity check against original result

	Pearson's Chi-squared test

data:  sextable_transposed
X-squared = 121.96, df = 2, p-value < 2.2e-16

  Dimension     Value         A         B        C
1         F Residuals -2.351696  9.687398 -8.60265
2         F  p values  0.112128  0.000000  0.00000
3         M Residuals  2.351696 -9.687398  8.60265
4         M  p values  0.112128  0.000000  0.00000
"
#### Prep dataframe ####

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

# Take into account how many trials per condition for a logistic model
analysis_df_long_scaled$nTrials<-2

# Re-level the cognitive group factor so that the most cognitively healthy group is the reference
analysis_df_long_scaled$nC3_orig <- analysis_df_long$nC3 # keep the original variable for reference

if(this_cog_measure == "animal naming"){
  # For verbal fluency, animal_naming, group C is the healthiest, so is the reference
  analysis_df_long_scaled$nC3 <- relevel(analysis_df_long_scaled$nC3, ref = "C")
}else{# For immediate and delayed recall, groups B are the healthiest, so is the reference
  analysis_df_long_scaled$nC3 <- relevel(analysis_df_long_scaled$nC3, ref = "B")
}

#### Fit mixed models ####

if(!plotting_only){
  # If we are only plotting we don't need all of the models (save time, only get full model)
  
  # Adjusted baseline model: nC3 + SOA + age *SOA + sex * SOA
  SOA_additive <-glmer(
    Accuracy ~  age_W3*SOA + nC3 + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
      Shams_0B2F_W3 + (1|tilda_serial), 
    data = analysis_df_long_scaled, 
    family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  
  
}

# Adjusted full interaction model: nC3 * SOA + age *SOA + sex * SOA
SOA_interaction <-glmer(
  Accuracy ~  age_W3*SOA + nC3 * SOA + sex_W3 * SOA + edu3_W3 + Pre_Post + VAS_W3 + ph108_W3 + ph102_W3 + Shams_1B1F_W3 + Shams_2B0F_70_W3 + 
    Shams_0B2F_W3 + (1|tilda_serial), 
  data = analysis_df_long_scaled, 
  family = binomial(link = "logit"), weights = nTrials, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

if(!plotting_only){
  
  '
# Does the interaction with cognitive group remain significant after controlling fo the interaction with age and sex

delayed recall X2(4) = 65.514 , p = 2.005e-13 ***  (baseline; AIC =28651  BIC = 28884; full; AIC = 28593 , BIC = 28858
animal naming X2(4) = 85.559, p < 2.2e-16 *** (baseline; AIC = 28260 BIC =28493 ; full; AIC = 28182, BIC = 28446
immediate recall X2(4) = 89.198, p < 2.2e-16 *** (baseline; AIC = 28608 BIC = 28841; full; AIC = 28527, BIC = 28791
'
  # Likelihood ratio test comparing full model to model with key interaction term dropped
  # Running this 3 times for longitudinal models so interpret with corrected alpha of .016
  anova(SOA_additive, SOA_interaction)
  
}

# Plot and save figures and tables of model results
if(this_cog_measure == "animal naming"){
  mytable(SOA_interaction, "Trajectory Group [A]", "Trajectory Group [B]", this_cog_measure)
  oddsratio_plot <-myplot(SOA_interaction, "Trajectory Group [A]", "Trajectory Group [B]")
  oddsratio_plot_reduced <-myplot_reduced(SOA_interaction, "nC3A", "nC3B", "Trajectory Group [A]", "Trajectory Group [B]")
  }else{
  mytable(SOA_interaction, "Trajectory Group [A]", "Trajectory Group [C]", this_cog_measure)
    oddsratio_plot <-myplot(SOA_interaction, "Trajectory Group [A]", "Trajectory Group [C]")
    oddsratio_plot_reduced <-myplot_reduced(SOA_interaction, "nC3A", "nC3C", "Trajectory Group [A]", "Trajectory Group [C]")
  }
  
saveplot(this_cog_measure, oddsratio_plot)

saveplot(paste(this_cog_measure, "_selectedTerms", sep = ''), oddsratio_plot_reduced)
