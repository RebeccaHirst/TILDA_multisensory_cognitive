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

#### plot immediate recall by wave ####

# make immediate recall dataframe
immediate_recall_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial, age_W3,age3_W3, sex_W3,Shams_2B1F_m230_W3, Shams_2B1F_230_W3, immediaterecall_total_W1,
         immediaterecall_total_W2, immediaterecall_total_W3, immediaterecall_total_W4,
         immediaterecall_total_W5)

# drop na values
immediate_recall_df<-immediate_recall_df%>% 
  drop_na(immediaterecall_total_W1, immediaterecall_total_W2, immediaterecall_total_W3,
          immediaterecall_total_W4, immediaterecall_total_W5, Shams_2B1F_230_W3, Shams_2B1F_m230_W3)

# reshape to long
immediate_recall_df<- reshape(immediate_recall_df, idvar="tilda_serial",
                        varying = c("immediaterecall_total_W1","immediaterecall_total_W2", "immediaterecall_total_W3", "immediaterecall_total_W4", "immediaterecall_total_W5"),
                        v.name=c("immediate_recall"),
                        times=c("immediaterecall_total_W1","immediaterecall_total_W2", "immediaterecall_total_W3", "immediaterecall_total_W4", "immediaterecall_total_W5"),
                        direction="long")
immediate_recall_df$wave<-numextract(immediate_recall_df$time)

# summaries group means, sd and se
cdata <- ddply(immediate_recall_df, c("age3_W3", "wave"), summarise,
               N    = sum(!is.na(immediate_recall)),
               mean = mean(immediate_recall, na.rm=TRUE),
               sd   = sd(immediate_recall, na.rm=TRUE),
               se   = sd / sqrt(N)
)
cdata$immediate_recall<-cdata$mean
# Plot recall by wave per age group (basic)
ggplot(cdata , aes(x=wave, y=mean, group=age3_W3, color=age3_W3)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1,
  position=position_dodge(0))+
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal()+ylab('mean immediate recall')+
  labs(fill = "Age group (at wave 3)")+ylim(0, 20)


# Plot recall by wave per age group (with individual data points)
ggplot(immediate_recall_df, aes(x=wave, y=immediate_recall, group=age3_W3, color=age3_W3)) + 
  geom_point(alpha=.1, position = position_dodge2(0.5), size=.1)+
  geom_point(data = cdata, position=position_dodge(0.5))+
  geom_errorbar(data = cdata, aes(ymin=mean-se, ymax=mean+se), width=.1,
                                         position=position_dodge(0.5))+
  geom_line(data = cdata, position=position_dodge(0.5))+
  scale_color_brewer(palette="Paired")+theme_minimal()+ylab('mean immediate recall')+
  labs(fill = "Age group (at wave 3)")+ylim(0, 20)


#### plot animal naming by wave ####
# make animal naming dataframe
animal_naming_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial, age_W3,age3_W3, sex_W3,Shams_2B1F_m230_W3, Shams_2B1F_230_W3, 
         COGanimal_naming_W1, COGanimal_naming_W2, COGanimal_naming_W3,
         COGanimal_naming_W4, COGanimal_naming_W5)

# drop na values
animal_naming_df<-animal_naming_df%>% 
  drop_na(COGanimal_naming_W1, COGanimal_naming_W2, COGanimal_naming_W3,
          COGanimal_naming_W4, COGanimal_naming_W5)

# reshape to long
animal_naming_df<- reshape(animal_naming_df, idvar="tilda_serial",
                           varying = c("COGanimal_naming_W1","COGanimal_naming_W2", "COGanimal_naming_W3", "COGanimal_naming_W4", "COGanimal_naming_W5"),
                           v.name=c("animal_naming"),
                           times=c("COGanimal_naming_W1","COGanimal_naming_W2", "COGanimal_naming_W3", "COGanimal_naming_W4", "COGanimal_naming_W5"),
                           direction="long")
animal_naming_df$wave<-numextract(animal_naming_df$time)
  
# summaries group means, sd and se
andata <- ddply(animal_naming_df, c("age3_W3", "wave"), summarise,
               N    = sum(!is.na(animal_naming)),
               mean = mean(animal_naming, na.rm=TRUE),
               sd   = sd(animal_naming, na.rm=TRUE),
               se   = sd / sqrt(N)
)
andata$animal_naming<-andata$mean

ggplot(animal_naming_df, aes(x=wave, y=animal_naming, group=age3_W3, color=age3_W3)) + 
  geom_point(alpha=.1, position = position_dodge2(0.5), size=.1)+
  geom_point(data = andata, position=position_dodge(0.5))+
  geom_errorbar(data = andata, aes(ymin=animal_naming-se, ymax=animal_naming+se), width=.1,
                position=position_dodge(0.5))+
  geom_line(data = andata, position=position_dodge(0.5))+
  scale_color_brewer(palette="Paired")+theme_minimal()+ylab('animals named (N)')+
  labs(color = "Age group (at wave 3)")

#### plot delayed recall by wave ####

# make delayed recall dataframe
COGdelayed_recall_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial, age_W3,age3_W3, sex_W3,COGdelayedrecall_W1,Shams_2B1F_m230_W3, Shams_2B1F_230_W3, 
         COGdelayedrecall_W2, COGdelayedrecall_W3, COGdelayedrecall_W4,
         COGdelayedrecall_W5)

# drop na values
COGdelayed_recall_df<-COGdelayed_recall_df%>% 
  drop_na(COGdelayedrecall_W1, COGdelayedrecall_W2, COGdelayedrecall_W3,
          COGdelayedrecall_W4, COGdelayedrecall_W5)

# reshape to long
COGdelayed_recall_df<- reshape(COGdelayed_recall_df, idvar="tilda_serial",
                               varying = c("COGdelayedrecall_W1","COGdelayedrecall_W2", "COGdelayedrecall_W3", "COGdelayedrecall_W4", "COGdelayedrecall_W5"),
                               v.name=c("COGdelayed_recall"),
                               times=c("COGdelayedrecall_W1","COGdelayedrecall_W2", "COGdelayedrecall_W3", "COGdelayedrecall_W4", "COGdelayedrecall_W5"),
                               direction="long")
COGdelayed_recall_df$wave<-numextract(COGdelayed_recall_df$time)

# summaries group means, sd and se
drdata <- ddply(COGdelayed_recall_df, c("age3_W3", "wave"), summarise,
                N    = sum(!is.na(COGdelayed_recall)),
                mean = mean(COGdelayed_recall, na.rm=TRUE),
                sd   = sd(COGdelayed_recall, na.rm=TRUE),
                se   = sd / sqrt(N)
)

drdata$COGdelayed_recall<-drdata$mean
# plot n recalled by age
ggplot(COGdelayed_recall_df, aes(x=wave, y=COGdelayed_recall, group=age3_W3, color=age3_W3)) + 
  geom_point(alpha=.1, position = position_dodge2(0.5), size=.1)+
  geom_point(data = drdata, position=position_dodge(0.5))+
  geom_errorbar(data = drdata, aes(ymin=COGdelayed_recall-se, ymax=COGdelayed_recall+se), width=.1,
                position=position_dodge(0.5))+
  geom_line(data = drdata, position=position_dodge(0.5))+
  scale_color_brewer(palette="Paired")+theme_minimal()+ylab('Delayed recall')+
  labs(color = "Age group (at wave 3)")

#### prospective memory by wave ####


# make prospective memory dataframe
prospMem1_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial, age_W3,age3_W3, sex_W3,COGprosmem1_W1,
         COGprosmem1_W2, COGprosmem1_W3, COGprosmem1_W4,
         COGprosmem1_W5)

prospMem2_df<-tilda_dataW3W1W2W4W5%>% 
  select(tilda_serial, age_W3,age3_W3, sex_W3,COGprosmem1_W1,
         COGprosmem1_W2, COGprosmem1_W3, COGprosmem1_W4,
         COGprosmem1_W5)
