library(corrplot)
library(lme4)
library(ggplot2)
library(Rcpp)
library(psych)
library(dplyr)
library(tidyr)
library(jtools)
library(mediation)
library(psych)
library(ggsci)

rm(list = ls())

df <- read.csv('../helping.csv')

df <- mutate(df, race_bi= case_when(
  race == 1  ~ 1, 
  race == 2  ~ 2, 
  race == 3  ~ 2, 
  race == 4  ~ 2, 
  race == 5  ~ 2, 
  race == 6  ~ 2, 
))


#IRI

df <- mutate(df, iri1r= case_when(
  iri1 == 1  ~ 5, 
  iri1 == 2  ~ 4, 
  iri1 == 3  ~ 3, 
  iri1 == 4  ~ 2, 
  iri1 == 5  ~ 1, 
))

df <- mutate(df, iri2r= case_when(
  iri2 == 1  ~ 5, 
  iri2 == 2  ~ 4, 
  iri2 == 3  ~ 3, 
  iri2 == 4  ~ 2, 
  iri2 == 5  ~ 1, 
))

df <- mutate(df, iri8r= case_when(
  iri8 == 1  ~ 5, 
  iri8 == 2  ~ 4, 
  iri8 == 3  ~ 3, 
  iri8 == 4  ~ 2, 
  iri8 == 5  ~ 1, 
))

df <- mutate(df, iri9r= case_when(
  iri9 == 1  ~ 5, 
  iri9 == 2  ~ 4, 
  iri9 == 3  ~ 3, 
  iri9 == 4  ~ 2, 
  iri9 == 5  ~ 1, 
))

df <- mutate(df, iri10r= case_when(
  iri10 == 1  ~ 5, 
  iri10 == 2  ~ 4, 
  iri10 == 3  ~ 3, 
  iri10 == 4  ~ 2, 
  iri10 == 5  ~ 1, 
))

df <- mutate(df, iri15r= case_when(
  iri10 == 1  ~ 5, 
  iri10 == 2  ~ 4, 
  iri10 == 3  ~ 3, 
  iri10 == 4  ~ 2, 
  iri10 == 5  ~ 1, 
))

df <- mutate(df, iri16r= case_when(
  iri10 == 1  ~ 5, 
  iri10 == 2  ~ 4, 
  iri10 == 3  ~ 3, 
  iri10 == 4  ~ 2, 
  iri10 == 5  ~ 1, 
))

df <- mutate(df, iri22r= case_when(
  iri10 == 1  ~ 5, 
  iri10 == 2  ~ 4, 
  iri10 == 3  ~ 3, 
  iri10 == 4  ~ 2, 
  iri10 == 5  ~ 1, 
))

df <- mutate(df, iri23r= case_when(
  iri10 == 1  ~ 5, 
  iri10 == 2  ~ 4, 
  iri10 == 3  ~ 3, 
  iri10 == 4  ~ 2, 
  iri10 == 5  ~ 1, 
))


df$distress = rowMeans(df[,c("iri1r", "iri2r","iri3", "iri4","iri5", "iri6","iri7")])
df$concern = rowMeans(df[,c("iri8r", "iri9r","iri10r", "iri11","iri12", "iri13","iri14")])
df$perspect = rowMeans(df[,c("iri22r", "iri23r","iri24", "iri25","iri26", "iri27","iri28")])
df$fantasy = rowMeans(df[,c("iri15r", "iri16r","iri17", "iri18","iri19", "iri20","iri21")])
df$help = rowSums(df[,c("social_support1a", "social_support1b","social_support1c", "social_support1d")])


#emotion while listening to the story

df$story_pa =rowMeans(df[,c("story_connected","story_understanding","story_supportive","story_compassionate",
                            "story_loving","story_trusting","story_interested","story_satisfied","story_relaxed", 
                            "story_pleasant","story_good","story_accepting","story_comfortable","story_happy", 
                            "story_important")])

df$story_na =rowMeans(df[,c("story_stressed","story_nervous","story_anxious","story_annoyed","story_uncomfortable",
                            "story_dissatisfied","story_insecure","story_sad","story_failure","story_incomplete", 
                            "story_uninterested")])
################
#### METHOD ####
################
#Participants
summary(df$age)
sd(df$age)
table(df$race)

#Measures

# Crombach's alpha


df_concern <- df[ , c("iri8r", "iri9r","iri10r", "iri11","iri12", "iri13","iri14")]    
alpha(df_concern)

df_perspect <- df[ , c("iri22r", "iri23r","iri24", "iri25","iri26", "iri27","iri28")]    
alpha(df_perspect)

df_distress <- df[ , c("iri1r", "iri2r","iri3", "iri4","iri5", "iri6","iri7")]    
alpha(df_distress)

df_fantasy <- df[ , c("iri15r", "iri16r","iri17", "iri18","iri19", "iri20","iri21")]    
alpha(df_fantasy)



df_na <- df[ , c("story_stressed","story_nervous","story_anxious","story_annoyed","story_uncomfortable",
                 "story_dissatisfied","story_insecure","story_sad","story_failure","story_incomplete", "story_uninterested")]
alpha(df_na)

df_pa <- df[ , c("story_connected","story_understanding","story_supportive","story_compassionate",
                 "story_loving","story_trusting","story_interested","story_satisfied","story_relaxed", 
                 "story_pleasant","story_good","story_accepting","story_comfortable","story_happy", "story_important")]
alpha(df_pa)


# Demographics
table(df$race)
71/77*100 #92% White

##Participants’ age was positively associated with one of the primary outcome measures (positive affect while listening to the story; b=0.097, p=0.042).
summary(lm(story_pa ~ age, df)) 

##Participants’ race/ethnicity did not co-vary with any of the primary outcomes (p>0.30).
summary(lm(help ~ age, df))
summary(lm(story_na ~ age, df))
summary(lm(help ~ race, df))
summary(lm(story_pa ~ race, df))
summary(lm(story_na ~ race, df))

#Analysis Plan
##we did not find any significant interaction between our primary predictors and the intervention condition.

summary(lm(help ~ distress * cond + age + race_bi,  df))
summary(lm(help ~ concern * cond + age + race_bi,  df))
summary(lm(help ~ perspect * cond + age + race_bi,  df))

summary(lm(story_na ~ distress * cond  + age + race_bi,  df))
summary(lm(story_na ~ concern * cond  + age + race_bi,  df))
summary(lm(story_na ~ perspect * cond  + age + race_bi,  df))

summary(lm(story_pa ~ distress * cond  + age + race_bi,  df))
summary(lm(story_pa ~ concern * cond  + age + race_bi,  df))
summary(lm(story_pa ~ perspect * cond  + age + race_bi,  df))




#################
#### Results ####
################# 

## Relationships among Subcomponents of Empathy
cor.test(df$concern, df$perspect)
cor.test(df$distress, df$concern)
cor.test(df$distress, df$perspect)

## Empathy and Helping

test =lm(help ~ concern + age + race_bi ,  df)
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(help ~ perspect + age + race_bi ,  df)
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(help ~ distress + age + race_bi ,  df)
summary(test)
summ(test, digits = 3, confint = TRUE)



## Empathy and Affective Responses to Others Emotions

test =lm(story_pa ~ concern   + age + race_bi ,  df) #NA
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(story_pa ~ perspect + age + race_bi  ,  df) #NA
summary(test)
summ(test, digits = 3, confint = TRUE)



test =lm(story_na ~ distress +  age + race_bi ,  df) 
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(story_pa ~ distress + age + race_bi ,  df) #sig
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(story_na ~ concern +  age + race_bi ,  df) 
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(story_na ~ perspect +  age + race_bi ,  df) 
summary(test)
summ(test, digits = 3, confint = TRUE)



##Indirect Effects of Empathy on Helping through Affects

### Concern
full_model = lm(help ~ story_pa + concern   + age + race_bi , data= df)
summary(full_model)

mediate_model = lm(story_pa ~ concern  + age + race_bi  , data=df)
summary(mediate_model)

results = mediation:: mediate(mediate_model, full_model, treat="concern", 
                              mediator = "story_pa",
                              boot=TRUE, sims=500)
summary(results)

### Perspective taking
full_model = lm(help ~ story_pa + perspect   + age + race_bi , data= df)
summary(full_model)

mediate_model = lm(story_pa ~ perspect  + age + race_bi  , data=df)
summary(mediate_model)

results = mediation:: mediate(mediate_model, full_model, treat="perspect", 
                              mediator = "story_pa",
                              boot=TRUE, sims=500)
summary(results)

### Distress
full_model = lm(help ~ story_na + distress  + age + race_bi , data= df)
summary(full_model)

mediate_model = lm(story_na ~ distress + age + race_bi   , data=df)
summary(mediate_model)

results = mediation:: mediate(mediate_model, full_model, treat="distress", 
                              mediator = "story_na",
                              boot=TRUE, sims=500)
summary(results)

  

#################
#### FIGURES ####
#################

#Figure 2


ggplot(df,aes(concern,story_pa))+
  geom_smooth(method=lm,color="#D8D8D8")+
  geom_point(size=3)+
  geom_smooth(method=lm, fill=NA, size=1.5)+
  theme(text=element_text(size=15))+
  theme_bw() + theme(panel.grid.minor = element_blank()) + 
  labs(x = "concern", y = "positive affect") +
  scale_color_jama() +theme(panel.grid.major = element_blank())

ggplot(df,aes(concern,story_na))+
  geom_smooth(method=lm,color="#D8D8D8")+
  geom_point(size=3)+
  geom_smooth(method=lm, fill=NA, size=1.5)+
  theme(text=element_text(size=15))+
  theme_bw() + theme(panel.grid.minor = element_blank()) + 
  labs(x = "concern", y = "negativce affect") +
  scale_color_jama() +theme(panel.grid.major = element_blank())

ggplot(df,aes(perspect,story_pa))+
  geom_smooth(method=lm,color="#D8D8D8")+
  geom_point(size=3)+
  geom_smooth(method=lm, fill=NA, size=1.5)+
  theme(text=element_text(size=15))+
  theme_bw() + theme(panel.grid.minor = element_blank()) + 
  labs(x = "perspect", y = "positive affect") +
  scale_color_jama() +theme(panel.grid.major = element_blank())

ggplot(df,aes(perspect,story_na))+
  geom_smooth(method=lm,color="#D8D8D8")+
  geom_point(size=3)+
  geom_smooth(method=lm, fill=NA, size=1.5)+
  theme(text=element_text(size=15))+
  theme_bw() + theme(panel.grid.minor = element_blank()) + 
  labs(x = "perspect", y = "negativce affect") +
  scale_color_jama() +theme(panel.grid.major = element_blank())

ggplot(df,aes(distress,story_pa))+
  geom_smooth(method=lm,color="#D8D8D8")+
  geom_point(size=3)+
  geom_smooth(method=lm, fill=NA, size=1.5)+
  theme(text=element_text(size=15))+
  theme_bw() + theme(panel.grid.minor = element_blank()) + 
  labs(x = "distress", y = "positive affect") +
  scale_color_jama() +theme(panel.grid.major = element_blank())

ggplot(df,aes(distress,story_na))+
  geom_smooth(method=lm,color="#D8D8D8")+
  geom_point(size=3)+
  geom_smooth(method=lm, fill=NA, size=1.5)+
  theme(text=element_text(size=15))+
  theme_bw() + theme(panel.grid.minor = element_blank()) + 
  labs(x = "distress", y = "negative affect") +
  scale_color_jama() +theme(panel.grid.major = element_blank())



#######################
#### Supplementary ####
#######################

#SI1. The Fantasy subscale of the Interpersonal Reactivity Index

cor.test(df$fantasy, df$concern)
cor.test(df$fantasy, df$perspect)
cor.test(df$fantasy, df$distress)

summary(lm(help ~ fantasy + age + race_bi ,  df))
summary(lm(story_pa ~ fantasy + age + race_bi ,  df))
summary(lm(story_na ~ fantasy + age + race_bi ,  df))

##Indirect path


full_model = lm(help ~ story_pa + fantasy   + age + race_bi , data= df)
summary(full_model)

mediate_model = lm(story_pa ~ fantasy  + age + race_bi  , data=df)
summary(mediate_model)

results = mediation:: mediate(mediate_model, full_model, treat="fantasy", 
                              mediator = "story_pa",
                              boot=TRUE, sims=500)
summary(results)


# SI3. Mood
df$mDES_pos = df$mdes_T1_PA
df$mDES_neg = df$mdes_T1_NA
df$story_pos = df$story_pa
df$story_neg = df$story_na
df$perspective = df$perspect


summary(lm(mDES_pos ~ concern + age + race_bi, df)) #NA
summary(lm(mDES_neg ~ concern + age + race_bi, df)) #NA
summary(lm(mDES_pos ~ perspect + age + race_bi, df)) #NA
summary(lm(mDES_neg ~ perspect + age + race_bi, df)) #NA
summary(lm(mDES_pos ~ distress + age + race_bi, df)) #NA
summary(lm(mDES_neg ~ distress + age + race_bi, df)) #NA

#those who were in a better mood at baseline also felt more positive in response to another’s suffering 
summary(lm(story_pos ~ mDES_pos  + age + race_bi, df))
summary(lm(story_pos ~ concern + mDES_pos  + age + race_bi, df)) #NA
summary(lm(story_pos ~ perspect + mDES_pos  + age + race_bi, df)) #NA

#The baseline negative mood was not associated with the degree to which people felt negative in response to another’s suffering
summary(lm(mDES_neg ~ story_neg + age + race_bi, df)) #NA
summary(lm(story_pos ~ mDES_pos + concern + age + race_bi, df)) #NA


#Fig.SI3. Bivariate correlations among empathy, compassion, and baseline positive and negative mood

cor = df %>%
  dplyr::select("concern", "perspective", "distress","mDES_pos", "mDES_neg","story_pos","story_neg")

M <- Hmisc::rcorr(as.matrix(cor))

corrplot(M$r, p.mat = M$P, insig = "label_sig",
         sig.level = c(.001, .05), pch.cex=0.9, pch.col = "white",
         method="color", type="lower")


#SI4. Demographic covariates

## Empathy and Helping
summary(lm(story_pa ~ distress + age,  df))

summary(lm(help ~ concern + age,  df))
summary(lm(help ~ perspect + age,  df))
summary(lm(help ~ distress + age,  df))

summary(lm(story_na ~ distress + age,  df))

summary(lm(story_pa ~ concern + age,  df))
summary(lm(story_pa ~ perspect + age,  df))

summary(lm(story_na ~ concern + age,  df))
summary(lm(story_na ~ perspect + age,  df))


## mediation
### Concern
full_model = lm(help ~ story_pa + concern + age, data= df)
summary(full_model)

mediate_model = lm(story_pa ~ concern + age , data=df)
summary(mediate_model)

results = mediation:: mediate(mediate_model, full_model, treat="concern", 
                              mediator = "story_pa",
                              boot=TRUE, sims=500)
summary(results)

### Perspective
full_model = lm(help ~ story_pa + perspect + age, data= df)
summary(full_model)

mediate_model = lm(story_pa ~ perspect + age , data=df)
summary(mediate_model)

results = mediation:: mediate(mediate_model, full_model, treat="perspect", 
                              mediator = "story_pa",
                              boot=TRUE, sims=500)
summary(results)

### Distress
full_model = lm(help ~ story_na + distress + age, data= df)
summary(full_model)

mediate_model = lm(story_na ~ distress + age , data=df)
summary(mediate_model)

results = mediation:: mediate(mediate_model, full_model, treat="distress", 
                              mediator = "story_na",
                              boot=TRUE, sims=500)
summary(results)




#SI5. Correction for multiple comparisons

## FDR correction 
summary(lm(help ~ concern + age + race_bi ,  df))
summary(lm(story_pa ~ concern + age + race_bi ,  df))
summary(lm(story_pa ~ perspect + age + race_bi ,  df))
summary(lm(story_na ~ distress +  age + race_bi ,  df))
#ACME ps for mediation analysis = 0.036 and 0.028
p = c(0.0361,0.000299,0.000905, 0.0205, 0.036, 0.028)
round(p.adjust(p, "BH"), 6)




