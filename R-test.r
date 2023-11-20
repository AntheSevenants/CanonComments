# set libraries
library(lme4)
library(effects)
library(dplyr)
library(lmerTest)
library(Matrix)
Sys.setenv(JAVA_HOME = "C:/Program Files (x86)/Java/jre-1.8/")

options(scipen = 10)

# set working directory + opening files
setwd("C:/Projects/CanonComments/R data")
ER <- read.csv("C:/Projects/CanonComments/sorted_data/Combined_OPEN.csv", header = TRUE, stringsAsFactors = TRUE)
WN <- read.csv("C:/Projects/CanonComments/data/4299WordNorms Moors et al.csv", header = TRUE, stringsAsFactors = TRUE)
DA <- read.csv("data/data_anon.csv", header = TRUE, stringsAsFactors = TRUE)
str(ER)

rm(list = ls())

ER$UniqueID <- as.factor(1:nrow(ER))
str(ER)
WN$Lemmata <- as.character(WN$Words)
str(WN)
ER$Lemmata <- as.character(ER$lemma)

LH$ResponseID <- as.character(LH$response_id)
DA$ResponseId <- as.character(DA$ResponseId)
# merging the dataframe
LH <- left_join(ER, WN, by = "Lemmata")
LHT2 <- left_join(LH, DA, by = "ResponseId")
str(LH)

summary(LH$M.V)

nrow(LH) - 374892

LHT <- filter(LH, !is.na(M.V))
str(LHT)

summary(lm_value <- lm(M.V ~ Grammatical.category, data=LH))

lmer_value <- lmer(M.V ~ Grammatical.category + (1|response_id), data = LHT)
plot(allEffects(lmer_value))

remove.packages('lme4', 'C:/Users/lean/AppData/Local/R/win-library/4.3')
install.packages('lme4')

install.packages("lme4", type = "source")
