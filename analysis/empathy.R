

library(boot)
library(corrplot)
library(irr)
library(lme4)
library(ggplot2)
library(Rcpp)
library(psych)
library(dplyr)
library(tidyr)
library(jtools)
library(psych)
library(ggsci)

rm(list = ls())

df <- read.csv('../helping.csv')

# Interpersonal Reactivity Index(IRI)
## reverse code items 

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
  iri15 == 1  ~ 5, 
  iri15 == 2  ~ 4, 
  iri15 == 3  ~ 3, 
  iri15 == 4  ~ 2, 
  iri15 == 5  ~ 1, 
))

df <- mutate(df, iri16r= case_when(
  iri16 == 1  ~ 5, 
  iri16 == 2  ~ 4, 
  iri16 == 3  ~ 3, 
  iri16 == 4  ~ 2, 
  iri16 == 5  ~ 1, 
))

df <- mutate(df, iri22r= case_when(
  iri22 == 1  ~ 5, 
  iri22 == 2  ~ 4, 
  iri22 == 3  ~ 3, 
  iri22 == 4  ~ 2, 
  iri22 == 5  ~ 1, 
))

df <- mutate(df, iri23r= case_when(
  iri23 == 1  ~ 5, 
  iri23 == 2  ~ 4, 
  iri23 == 3  ~ 3, 
  iri23 == 4  ~ 2, 
  iri23 == 5  ~ 1, 
))

## calculate means for personal distress ("distress"), empathic concern ("concern"), perspective taking ("perspect"), and fantasy ("fantasy") 
df$distress = rowMeans(df[,c("iri1r", "iri2r","iri3", "iri4","iri5", "iri6","iri7")])
df$concern = rowMeans(df[,c("iri8r", "iri9r","iri10r", "iri11","iri12", "iri13","iri14")])
df$perspect = rowMeans(df[,c("iri22r", "iri23r","iri24", "iri25","iri26", "iri27","iri28")])
df$fantasy = rowMeans(df[,c("iri15r", "iri16r","iri17", "iri18","iri19", "iri20","iri21")])

# Helping
## sum the amount of help given to others in the past month
df$help = rowSums(df[,c("social_support1a", "social_support1b","social_support1c", "social_support1d")])

# Emotion experienced while listening to someone else's emotional life story

## positive affect ("story_pa")
df$story_pa =rowMeans(df[,c("story_connected","story_understanding","story_supportive","story_compassionate",
                            "story_loving","story_trusting","story_interested","story_satisfied","story_relaxed", 
                            "story_pleasant","story_good","story_accepting","story_comfortable","story_happy", 
                            "story_important")])

## negative affect ("story_na")
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
summary(lm(help ~ race, df))
summary(lm(story_pa ~ race, df))
summary(lm(story_na ~ race, df))

#Analysis Plan
##we did not find any significant interaction between our primary predictors and the intervention condition.

summary(lm(help ~ distress * cond + age,  df))
summary(lm(help ~ concern * cond + age,  df))
summary(lm(help ~ perspect * cond + age,  df))

summary(lm(story_na ~ distress * cond  + age,  df))
summary(lm(story_na ~ concern * cond  + age,  df))
summary(lm(story_na ~ perspect * cond  + age,  df))

summary(lm(story_pa ~ distress * cond  + age,  df))
summary(lm(story_pa ~ concern * cond  + age,  df))
summary(lm(story_pa ~ perspect * cond  + age,  df))


#################
#### Results ####
################# 

## Relationships among Subcomponents of Empathy
cor.test(df$concern, df$perspect)
cor.test(df$distress, df$concern)
cor.test(df$distress, df$perspect)

## Empathy and Helping

test =lm(help ~ concern + age ,  df)
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(help ~ perspect + age  ,  df)
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(help ~ distress + age  ,  df)
summary(test)
summ(test, digits = 3, confint = TRUE)



## Empathy and Affective Responses to Others Emotions

test =lm(story_pa ~ concern   + age ,  df) #NA
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(story_pa ~ perspect + age  ,  df) #NA
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(story_pa ~ distress + age  ,  df) 
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(story_na ~ distress +  age  ,  df) 
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(story_na ~ concern +  age ,  df) 
summary(test)
summ(test, digits = 3, confint = TRUE)

test =lm(story_na ~ perspect +  age  ,  df) 
summary(test)
summ(test, digits = 3, confint = TRUE)



##Indirect Effects of Empathy on Helping through Affects

df$Y= df$help 
df$C= df$age
## enter the correct variable names for each model
df$X = df$distress  #concern perspect distress
df$M = df$story_na #story_pa story_na

model_a <- lm(M ~ X + C, df)
a <- coef(model_a)["X"]

model_b <- lm(Y ~ M + X + C, df)
b <- coef(model_b)["M"]

indirect_effect <- a * b

set.seed(123)
bootstrap_results <- boot::boot(
  data = df,
  statistic = function(data, indices) {
    d <- data[indices, ]
    a <- coef(lm(M ~ X +  C , data = d))["X"]
    b <- coef(lm(Y ~ M + X +  age , data = d))["M"]
    return(a * b)
  },
  R = 5000
)

bootstrap_results
boot.ci(bootstrap_results, type = "bca")



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


###############
#### Table ####
###############

#Empathic Concern→ Positive Affect → Helping

##a path
test =lm(story_pa ~ concern + age, df)
summary(test)
summ(test, digits = 3, confint = TRUE)

##b and c' paths
test =lm(help ~ concern + story_pa + age, df)
summary(test)
summ(test, digits = 3, confint = TRUE)

##c path
test =lm(help ~ concern + age, df)
summary(test)
summ(test, digits = 3, confint = TRUE)

## a*b rerun the mediation analysis from above
bootstrap_results
boot.ci(bootstrap_results, type = "bca")

#Perspective Taking → Positive Affect → Helping

##a path
test =lm(story_pa ~ perspect + age, df)
summary(test)

##b and c' paths
test =lm(help ~ perspect + story_pa + age, df)
summary(test)
summ(test, digits = 3, confint = TRUE, standardize = FALSE)

##c path
test =lm(help ~ perspect + age, df)
summary(test)
summ(test, digits = 3, confint = TRUE, standardize = FALSE)

## a*b rerun the mediation analysis from above
bootstrap_results
boot.ci(bootstrap_results, type = "bca")

#Personal Distress → Positive Affect → Helping

##a path
test =lm(story_pa ~ distress + age, df)
summary(test)
summ(test, digits = 3, confint = TRUE)

##b and c' paths
test =lm(help ~ distress + story_pa + age, df)
summary(test)
summ(test, digits = 3, confint = TRUE)

##c path
test =lm(help ~ distress + age, df)
summary(test)
summ(test, digits = 3, confint = TRUE)

## a*b rerun the mediation analysis from above
bootstrap_results
boot.ci(bootstrap_results, type = "bca")


#Personal Distress → Negative Affect → Helping
##a path
test =lm(story_na ~ distress + age, df)
summary(test)
summ(test, digits = 3, confint = TRUE)

##b and c' paths
test =lm(help ~ distress + story_na + age, df)
summary(test)
summ(test, digits = 3, confint = TRUE)

##c path
test =lm(help ~ distress + age, df)
summary(test)
summ(test, digits = 3, confint = TRUE)

## a*b rerun the mediation analysis from above
bootstrap_results
boot.ci(bootstrap_results, type = "bca")


#######################
#### Supplementary ####
#######################

#SI1. The Fantasy subscale of the Interpersonal Reactivity Index

cor.test(df$fantasy, df$concern)
cor.test(df$fantasy, df$perspect)
cor.test(df$fantasy, df$distress)

summary(lm(help ~ fantasy + age,  df))
summary(lm(story_pa ~ fantasy + age,  df))
summary(lm(story_na ~ fantasy + age,  df))


# SI3. Mood
df$mDES_pos = df$mdes_T1_PA
df$mDES_neg = df$mdes_T1_NA
df$story_pos = df$story_pa
df$story_neg = df$story_na
df$perspective = df$perspect


summary(lm(mDES_pos ~ concern + age, df)) #NA
summary(lm(mDES_neg ~ concern + age, df)) #NA
summary(lm(mDES_pos ~ perspect + age, df)) #NA
summary(lm(mDES_neg ~ perspect + age, df)) #NA
summary(lm(mDES_pos ~ distress + age, df)) #NA
summary(lm(mDES_neg ~ distress + age, df)) #NA

#those who were in a better mood at baseline also felt more positive in response to another’s suffering 
summary(lm(story_pos ~ mDES_pos  + age, df))
summary(lm(story_pos ~ concern + mDES_pos  + age, df)) #NA
summary(lm(story_pos ~ perspect + mDES_pos  + age, df)) #NA

#The baseline negative mood was not associated with the degree to which people felt negative in response to another’s suffering
summary(lm(mDES_neg ~ story_neg + age, df)) #NA


#Fig.SI3. Bivariate correlations among empathy, compassion, and baseline positive and negative mood

cor = df %>%
  dplyr::select("concern", "perspective", "distress","mDES_pos", "mDES_neg","story_pos","story_neg")

M <- Hmisc::rcorr(as.matrix(cor))

corrplot(M$r, p.mat = M$P, insig = "label_sig",
         sig.level = c(.001, .05), pch.cex=0.9, pch.col = "white",
         method="color", type="lower")


#SI4. Outliers

## Univariate outliers
df$concern_z = scale(df$concern)
table(df$concern_z) # no outlier beyond +/- 3SD

df$perspect_z = scale(df$perspect)
table(df$perspect_z) # no outlier beyond +/- 3SD

df$distress_z = scale(df$distress)
table(df$distress_z) # no outlier beyond +/- 3SD

df$help_z = scale(df$help) 
table(df$help_z) # no outlier beyond +/- 3SD

df$story_pa_z = scale(df$story_pa) 
table(df$story_pa_z) #1 outlier +3SD

df$story_na_z = scale(df$story_na) 
table(df$story_na_z) #1 outlier +3SD

## Winsorize univariate outliers

### positive affect
mean_story_pa <- mean(df$story_pa, na.rm=T)
sd_story_pa <- sd(df$story_pa, na.rm=T)

lower_bound <- mean_story_pa - 3 * sd_story_pa
upper_bound <- mean_story_pa + 3 * sd_story_pa

df$story_pa_winsorized <- pmin(pmax(df$story_pa, lower_bound), upper_bound)

### negative affect
mean_story_na <- mean(df$story_na, na.rm=T)
sd_story_na <- sd(df$story_na, na.rm=T)

lower_bound <- mean_story_na - 3 * sd_story_na
upper_bound <- mean_story_na + 3 * sd_story_na

df$story_na_winsorized <- pmin(pmax(df$story_na, lower_bound), upper_bound)

## Reanalyze using winsorized variables
summary(lm(story_pa_winsorized ~ concern + age, df))
summary(lm(story_pa_winsorized ~ perspect + age, df))

summary(lm(story_na_winsorized ~ concern + age, df))
summary(lm(story_na_winsorized ~ perspect + age, df))

summary(lm(story_pa_winsorized ~ distress + age, df))
summary(lm(story_na_winsorized ~ distress + age, df))

### indirect
df$Y= df$help 
df$C= df$age
## plug in the correct variable names for each model
df$X = df$distress  #concern perspect distress
df$M = df$story_na_winsorized #story_pa_winsorized story_na_winsorized

model_a <- lm(M ~ X + C, df)
a <- coef(model_a)["X"]

model_b <- lm(Y ~ M + X + C, df)
b <- coef(model_b)["M"]

indirect_effect <- a * b

set.seed(123)
bootstrap_results <- boot::boot(
  data = df,
  statistic = function(data, indices) {
    d <- data[indices, ]
    a <- coef(lm(M ~ X +  C , data = d))["X"]
    b <- coef(lm(Y ~ M + X +  age , data = d))["M"]
    return(a * b)
  },
  R = 5000
)

bootstrap_results
boot.ci(bootstrap_results, type = "bca")


## Bivariate outliers (none detected using Mahalanobis distance)
### plug in the correct variable names to visualize outliers
df$var1 = df$perspect # concern perspect distress
df$var2 = df$story_na #story_pa story_na

df_sub <- df[, c("var1", "var2")]

mahalanobis_dist <- mahalanobis(df_sub, colMeans(df_sub), cov(df_sub))

### Set threshold for outliers (using 95% confidence level for 2 variables)
threshold <- qchisq(0.95, df = 2)

outliers <- which(mahalanobis_dist > threshold)

### Plot with outliers highlighted
plot(df$var1, df$var2, col = ifelse(mahalanobis_dist > threshold, "red", "black"),
     xlab = "Var1", ylab = "Var2", main = "Scatter Plot with Outliers Highlighted")
legend("topright", legend = c("Normal", "Outlier"), col = c("black", "red"), pch = 1)

#SI5. Correction for multiple comparisons

## FDR correction 
summary(lm(help ~ concern + age,  df))
summary(lm(story_pa ~ concern + age,  df))
summary(lm(story_pa ~ perspect + age,  df))
summary(lm(story_pa ~ distress +  age,  df))
summary(lm(story_na ~ distress +  age,  df))

p = c(0.0344,0.000308,0.00173, 0.04438, 0.0165)
round(p.adjust(p, "BH"), 5)


