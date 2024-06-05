# libraries
library(lme4)
library(Matrix)
library(effects)
library(dplyr)
library(lmerTest)
library(officer)
library(flextable)

# working directory + inlezen dataset
setwd("/Users/huysmanl/Projects/CanonComments/CanonComments")
LHT <- read.csv("sorted_data/Combined_Ratings-NEW.csv", header = TRUE,
                stringsAsFactors = TRUE)

## algemene modellen
# leeftijdmodellen
Age <- lmer(Valence ~ QPERS_AGE + (1 | response_id)
            + (1 | questions), data = LHT)
summary(Age)
eff <- effect("QPERS_AGE", Age)
eff_data <- as.data.frame(eff)
#visualisatie
plot(eff_data$QPERS_AGE, eff_data$fit, type = "l",
     col = "orange", lwd = 2,
     main = "Effect van leeftijd op valentie",
     xlab = "leeftijd", ylab = "valentie",
     xlim = range(eff_data$QPERS_AGE),
     ylim = range(c(eff_data$lower, eff_data$upper)))
polygon(c(eff_data$QPERS_AGE, rev(eff_data$QPERS_AGE)),
        c(eff_data$lower, rev(eff_data$upper)),
        col = rgb(1, 0.5, 0, 0.2), border = NA)
rug(eff_data$QPERS_AGE, col = "orange")
grid(col = "grey", lty = "dotted")

# filteren op gender
LHT_filtered_gender <- LHT %>% filter(QPERS_GENDER %in% c("Man", "Vrouw"))
Gender <- lmer(Happiness ~ QPERS_GENDER + (1 | response_id) + (1 | questions),
                data = LHT_filtered_gender)
summary(Gender)
eff <- effect("QPERS_GENDER", Gender)
plot(eff, multiline = TRUE)


# groeperen op opleiding
LHT$QPERS_EDU_Grouped <- ifelse(LHT$QPERS_EDU ==
                                  "Hoger onderwijs (universitair)",
                                "universitair",
                                "niet-universitair")
edu_grouped <- lmer(Surprise ~ QPERS_EDU_Grouped + (1 | response_id) +
                      (1 | questions), data = LHT)
summary(edu_grouped)
eff_grouped <- effect("QPERS_EDU_Grouped", edu_grouped)
plot(eff_grouped)

# groeperen op afkomst
LHT_filtered_birthcountry <- LHT %>%
  mutate(QPERS_BIRTH_COUNTRY_Grouped = case_when(
    QPERS_BIRTH_COUNTRY %in% c("België", "Nederland") ~ "De lage landen",
    QPERS_BIRTH_COUNTRY %in% c("Curaçao", "Aruba", "Suriname", "Indonesië",
                               "Bonaire")
    ~ "(Voormalige) Nederlandstalige gebieden buiten Europa",
    TRUE ~ "Andere" # Default case for all other countries
  ))
BirthCountry <- lmer(Disgust ~ QPERS_BIRTH_COUNTRY_Grouped + (1 | response_id)
                     + (1 | questions), data = LHT_filtered_birthcountry)
summary(BirthCountry)
eff <- effect("QPERS_BIRTH_COUNTRY_Grouped", BirthCountry)
plot(eff)

# Filter the dataset for only "België" and "Nederland"
LHT_filtered_bene <- LHT %>% 
  filter(QPERS_BIRTH_COUNTRY %in% c("België", "Nederland"))

# Model voor België en Nederland
BirthCountry_bene <- lmer(Surprise ~ QPERS_BIRTH_COUNTRY + (1 | response_id)
                          + (1 | questions), data = LHT_filtered_BENE)
summary(BirthCountry_BENE)
eff <- effect("QPERS_BIRTH_COUNTRY", BirthCountry_BENE)
plot(eff)


## onderwerpspecifieke modellen
LHT_filtered_gender <- LHT %>% filter(QPERS_GENDER %in% c("Man", "Vrouw"))
dependent_vars <- c("Valence", "Arousal", "Happiness", "Anger",
                    "Fear", "Sadness", "Disgust", "Surprise")
question_vars <- c("QI", "QC", "Q9", "Q10", "Q11",
                   "Q12_B", "Q14", "Q15", "Q16", "Q17")

# eerste lapply funcite die IsSingular() foutcode geeft
models <- lapply(question_vars, function(question) {
  data_subset <- LHT_filtered_gender[LHT_filtered_gender$questions
                                    == question, ]
  lapply(dependent_vars, function(var) {
    formula <- as.formula(paste(var, "~ QPERS_AGE + QPERS_GENDER
                           + (1 | response_id)"))
    model <- lmer(formula, data = data_subset)
    summary(model)  # Store the summary object
  })
})

models[[1]]

# code om alle modellen te bouwen voor onderwerpspecifieke modellen
models2 <- lapply(question_vars, function(question) {
  data_subset <- LHT_filtered_gender[LHT_filtered_gender$questions
                                     == question, ]
  lapply(dependent_vars, function(var) {
    formula <- as.formula(paste(var, "~ QPERS_AGE + QPERS_GENDER"))
    model <- lm(formula, data = data_subset)
    summary(model)
  })
})
models2[[1]]


# Structuur voor modellen waar gender niet significant is
data_subset <- subset(LHT_filtered_gender, questions == "QI")
model <- lm(Arousal ~ QPERS_AGE, data = data_subset)
summary(model)
eff_age <- effect("QPERS_AGE", model)
eff_age_df <- as.data.frame(eff_age)
data_subset_male <- subset(data_subset, QPERS_GENDER == "Man")
model_male <- lm(Arousal ~ QPERS_AGE, data = data_subset_male)
eff_age_male <- effect("QPERS_AGE", model_male)
eff_age_male_df <- as.data.frame(eff_age_male)
data_subset_female <- subset(data_subset, QPERS_GENDER == "Vrouw")
model_female <- lm(Arousal ~ QPERS_AGE, data = data_subset_female)
eff_age_female <- effect("QPERS_AGE", model_female)
eff_age_female_df <- as.data.frame(eff_age_female)
plot(eff_age_male_df$QPERS_AGE, eff_age_male_df$fit, type = "l",
     col = "darkgrey",
     main = "Effect van leeftijd op opwinding voor QI",
     xlab = "leeftijd", ylab = "geluk",
     xlim = c(min(data_subset$QPERS_AGE), max(data_subset$QPERS_AGE)),
     ylim = range(c(eff_age_df$lower, eff_age_df$upper)))
polygon(c(eff_age_male_df$QPERS_AGE, rev(eff_age_male_df$QPERS_AGE)),
        c(eff_age_male_df$lower, rev(eff_age_male_df$upper)),
        col = rgb(0.5, 0.5, 0.5, 0.2), border = NA)
rug(data_subset_male$QPERS_AGE, col = "darkgrey")
lines(eff_age_female_df$QPERS_AGE, eff_age_female_df$fit, col = "lightgrey")
polygon(c(eff_age_female_df$QPERS_AGE, rev(eff_age_female_df$QPERS_AGE)),
        c(eff_age_female_df$lower, rev(eff_age_female_df$upper)),
        col = rgb(0.8, 0.8, 0.8, 0.2), border = NA)
rug(data_subset_female$QPERS_AGE, col = "lightgrey")
lines(eff_age_df$QPERS_AGE, eff_age_df$fit, col = "orange")
polygon(c(eff_age_df$QPERS_AGE, rev(eff_age_df$QPERS_AGE)), 
        c(eff_age_df$lower, rev(eff_age_df$upper)),
        col = rgb(1, 0.5, 0, 0.2), border = NA)
rug(data_subset$QPERS_AGE, col = "orange")
grid()
legend("topleft", legend = c("algemeen", "mannen", "vrouwen"),
col = c("orange", "darkgrey", "lightgrey"), lty = 1,
fill = c(rgb(1, 0.5, 0, 0.2), rgb(0.5, 0.5, 0.5, 0.2), rgb(0.8, 0.8, 0.8, 0.2)))

### Copilot code ###
# Functie om modelresumés om te zetten naar een flextable
# met afgeronde waarden en vertalingen
model_summary_to_flextable <- function(model_summary, dependent_var,
                                       question_var) {
  coef_df <- as.data.frame(model_summary$coefficients)
  coef_df <- cbind(variabele = rownames(coef_df), coef_df)
  rownames(coef_df) <- NULL
  coef_df <- coef_df %>%
    mutate(across(where(is.numeric), ~ round(., 3))) %>%
    rename(schatting = Estimate, standaardfout = `Std. Error`,
           `t-waarde` = `t value`, `p-waarde` = `Pr(>|t|)`)
  coef_df$variabele <- ifelse(coef_df$variabele == "(Intercept)",
                              "constante (man)", coef_df$variabele)
  coef_df$variabele <- ifelse(coef_df$variabele == "QPERS_GENDERVrouw",
                              "verschil (vrouw - man)", coef_df$variabele)
  coef_df$variabele <- ifelse(coef_df$variabele == "QPERS_AGE",
                              "leeftijd", coef_df$variabele)
  coef_df <- mutate(coef_df, afhankelijke_variabele
                    = dependent_var, vraag = question_var)
  flextable(coef_df)
}

# Creëer een nieuw Word-document
doc <- read_docx()

# Vertaal de namen van de afhankelijke variabelen naar het Nederlands
afhankelijke_variabelen <- c("Valence" = "valentie", "Arousal" = "opwinding",
                             "Happiness" = "geluk", "Anger" = "woede",
                             "Fear" = "angst", "Sadness" = "verdriet",
                             "Disgust" = "afgunst", "Surprise" = "verrassing")

# Converteer alle modelresumés naar flextables en voeg ze toe aan het document
for (i in seq_along(question_vars)) {
  for (j in seq_along(dependent_vars)) {
    model_summary <- models2[[i]][[j]]
    question_var <- question_vars[[i]]
    dependent_var <- dependent_vars[[j]]
    afhankelijke_variabele <- afhankelijke_variabelen[[dependent_var]]
    ft <- model_summary_to_flextable(model_summary,
                                     afhankelijke_variabele, question_var)
    # Voeg een titel toe vóór de tabel
    title <- paste(afhankelijke_variabele)
    doc <- body_add_par(doc, value = title, style = "Normal")
    # Voeg de flextable toe
    doc <- body_add_flextable(doc, value = ft)
    # Voeg een lege regel toe tussen de tabellen
    doc <- body_add_par(doc, value = "", style = "Normal")
  }
}
# Sla het Word-document op
print(doc, target = "model_summaries_dutch.docx")
