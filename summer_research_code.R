# Install and load necessary packages. Note: You will need to install these libraries if they have not been installed.
library(plm)
library(dplyr)
library(lmtest)
library(car)
library(lme4)
library(stargazer)
library(texreg)
library(tseries)
library(tidyr)
library(knitr)

# Modified data
data <- read.csv("C:/Users/seanp/Downloads/summer_research_data_complete_1.csv")

# Create columns with real GDP per capita for each year
# Create new columns for each year
data <- data %>%
  group_by(Country) %>%
  mutate(
    LOGRGDPPC2000 = ifelse(Year == 2000, LOGRGDPPC, NA),
    LOGRGDPPC2001 = ifelse(Year == 2001, LOGRGDPPC, NA),
    LOGRGDPPC2002 = ifelse(Year == 2002, LOGRGDPPC, NA),
    LOGRGDPPC2003 = ifelse(Year == 2003, LOGRGDPPC, NA),
    LOGRGDPPC2004 = ifelse(Year == 2004, LOGRGDPPC, NA),
    LOGRGDPPC2005 = ifelse(Year == 2005, LOGRGDPPC, NA),
    LOGRGDPPC2006 = ifelse(Year == 2006, LOGRGDPPC, NA),
    LOGRGDPPC2007 = ifelse(Year == 2007, LOGRGDPPC, NA),
    LOGRGDPPC2008 = ifelse(Year == 2008, LOGRGDPPC, NA),
    LOGRGDPPC2009 = ifelse(Year == 2009, LOGRGDPPC, NA),
    LOGRGDPPC2010 = ifelse(Year == 2010, LOGRGDPPC, NA),
    LOGRGDPPC2011 = ifelse(Year == 2011, LOGRGDPPC, NA),
    LOGRGDPPC2012 = ifelse(Year == 2012, LOGRGDPPC, NA),
    LOGRGDPPC2013 = ifelse(Year == 2013, LOGRGDPPC, NA),
    LOGRGDPPC2014 = ifelse(Year == 2014, LOGRGDPPC, NA),
    LOGRGDPPC2015 = ifelse(Year == 2015, LOGRGDPPC, NA),
    LOGRGDPPC2016 = ifelse(Year == 2016, LOGRGDPPC, NA),
    LOGRGDPPC2017 = ifelse(Year == 2017, LOGRGDPPC, NA),
    LOGRGDPPC2018 = ifelse(Year == 2018, LOGRGDPPC, NA),
    LOGRGDPPC2019 = ifelse(Year == 2019, LOGRGDPPC, NA),
    LOGRGDPPC2020 = ifelse(Year == 2020, LOGRGDPPC, NA),
    LOGRGDPPC2021 = ifelse(Year == 2021, LOGRGDPPC, NA),
    LOGRGDPPC2022 = ifelse(Year == 2022, LOGRGDPPC, NA),
    LOGRGDPPC2023 = ifelse(Year == 2023, LOGRGDPPC, NA)
  ) %>%
  ungroup()

# Fill in the NA values with the actual logRGDPPC values for each country
data <- data %>%
  group_by(Country) %>%
  fill(starts_with("LOGRGDPPC"), .direction = "downup") %>%
  ungroup()


# Function to convert plm models to lm models
convert_to_lm_model <- function(plm_model) {
  lm_model <- lm(formula = formula(plm_model), data = model.frame(plm_model))
  return(lm_model)
}

# Function to calculate VIF for a given regression model
calculate_vif <- function(model) {
  model_matrix <- model.matrix(model)
  vif_values <- numeric(ncol(model_matrix))
  
  for (i in 1:ncol(model_matrix)) {
    lm_model <- lm(model_matrix[, i] ~ ., data = as.data.frame(model_matrix[, -i]))
    vif_values[i] <- 1 / (1 - summary(lm_model)$r.squared)
  }
  
  names(vif_values) <- colnames(model_matrix)
  return(vif_values)
}

# Names, descriptions, and sources of the variables
# Define the data as a data frame
data_sources <- data.frame(
  `Variable Name` = c(
    "GDP per capita growth (annual %)", 
    "Total early-stage entrepreneurial activity", 
    "Foreign direct investment, net inflows (% of GDP)", 
    "GDP per capita, PPP (constant 2021 international $)*", 
    "Research and development expenditure (% of GDP)", 
    "Urban population (% of total population)", 
    "Mean years of schooling (ISCED 1 or higher), population 25+ years, both sexes", 
    "Gross capital formation (% of GDP)", 
    "Trade (% of GDP)", 
    "Unemployment, total (% of total labor force) (modeled ILO estimate)", 
    "Inflation, consumer prices (annual %)", 
    "Government expenditure (% of GDP)", 
    "Domestic credit to private sector (% of GDP)", 
    "Fixed broadband subscriptions (per 100 people)", 
    "Political stability and absence of violence/terrorism: estimate"
  ),
  Abbreviation = c(
    "GROWTH", "TEA", "FDI", "LOGRGDPPC", "RD", "URBAN", 
    "EDUC", "GCF", "TRADE", "UNEMPLOYMENT", "INFLATION", 
    "GOVT", "CREDIT", "BROADBAND", "POLSTAB"
  ),
  Source = c(
    "World Bank Open Data", 
    "Global Entrepreneurship Monitor (GEM) Project", 
    "World Bank Open Data", 
    "World Bank Open Data", 
    "World Bank Open Data", 
    "World Bank Open Data", 
    "UNESCO Institute for Statistics", 
    "World Bank Open Data", 
    "World Bank Open Data", 
    "World Bank Open Data", 
    "World Bank Open Data", 
    "Our World in Data, cited from International Monetary Fund (IMF)", 
    "World Bank Open Data", 
    "World Bank Open Data", 
    "World Bank Open Data"
  ),
  stringsAsFactors = FALSE
)

# Print the table in a formatted style using kable
kable(
  data_sources, 
  col.names = c("Variable Name", "Abbreviation", "Source"), 
  caption = "Variable and Data Sources. *The natural log of this variable was used."
)

# Correlation matrix
selected_vars <- c("GROWTH", "TEA", "FDI", "LOGRGDPPC", "INITIAL", "RD", "URBAN", 
                   "EDUC", "GCF", "TRADE", "UNEMPLOYMENT", "INFLATION", 
                   "GOVT", "CREDIT", "BROADBAND", "POLSTAB")
data_correlation_matrix2 <- data %>% 
  select(all_of(selected_vars))
cor_matrix2 <- cor(data_correlation_matrix2, use = "pairwise.complete.obs")
latex_table <- kable(cor_matrix2, format = "latex", booktabs = TRUE, 
                     caption = "Correlation Matrix",
                     align = rep('r', length(selected_vars)),
                     digits = 2) # Adjust digits as needed
cat(latex_table)

# Test of which variables are correlated with lag of log real GDP per capita

pooledOLS <- lm(formula = LOGRGDPPC ~ INITIAL + BROADBAND + TELEPHONE + POLSTAB + URBAN,
                data = data)
summary(pooledOLS)
data$LOGRGDPPCestimate <- predict(pooledOLS, newdata = data)

data <- data %>% select(-code, -Code, -RGDPPC, -POPULATION, -PATENTS, -LOGPATENTS)

# Convert to panel format
panel <- pdata.frame(x = data, index = c("Country", "Year"))

# Filtered panel with the data used for the regressions
filtered_panel <- panel %>%
  filter(!is.na(GROWTH) & !is.na(TEA) & !is.na(FDI))
filtered_panel <- filtered_panel %>%
  select(-Country, -Year, -PATENT, -RGDPPC2001, -LOGPATENT, -MOBILE, -TELEPHONE,
         -matches("LOGRGDPPC20(0[0-9]|1[0-9]|2[0-3])"))
  
View(filtered_panel)

# Summary statistics
summary(filtered_panel)

# Compute summary statistics
panel_summary <- filtered_panel %>%
  summarise(
    across(everything(), list(
      N = ~sum(!is.na(.)),
      Mean = ~mean(. , na.rm = TRUE),
      SD = ~sd(. , na.rm = TRUE),
      Min = ~min(. , na.rm = TRUE),
      P25 = ~quantile(., probs = 0.25, na.rm = TRUE),
      Median = ~median(. , na.rm = TRUE),
      P75 = ~quantile(., probs = 0.75, na.rm = TRUE),
      Max = ~max(. , na.rm = TRUE)
    ), .names = "{col}_{fn}")
  )

# Reshape the summary statistics to a long format
panel_summary_long <- panel_summary %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "statistic"),
    names_sep = "_"
  )

# Reshape the long format data to a wide format
panel_summary_wide <- panel_summary_long %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  )

# Generate LaTeX code for the wide summary statistics table
latex_table <- kable(panel_summary_wide, format = "latex", booktabs = TRUE, 
                     caption = "Wide Summary Statistics Table",
                     align = 'lrrrrrr') # Adjust alignment as needed

# Print the LaTeX code
cat(latex_table)

# Count number of countries
filtered_panel2 <- panel %>%
  filter(!is.na(GROWTH) & !is.na(TEA) & !is.na(FDI))
num_countries <- filtered_panel2 %>%
  summarize(num_countries = n_distinct(Country))
num_countries

# Models using growth, TEA, and initial real GDP per capita

# Pooled OLS with growth and TEA
pooledOLSBasic <- plm(formula = GROWTH ~ TEA, 
                      data = panel,
                      index = c("Country", "Year"),
                      model = "pooling")
summary(pooledOLSBasic)
coeftest(pooledOLSBasic, vcovHC(pooledOLSBasic, type = "HC3", method = "arellano"))

# Add a squared variable for nonlinearity
pooledOLSSquared <- plm(formula = GROWTH ~ TEA + I(TEA^2), 
                      data = panel,
                      index = c("Country", "Year"),
                      model = "pooling")
summary(pooledOLSSquared)
coeftest(pooledOLSSquared, vcovHC(pooledOLSSquared, type = "HC3", method = "arellano"))

# Control for real GDP per capita estimate
pooledOLSrgdppc <- plm(formula = GROWTH ~ TEA + lag(LOGRGDPPCestimate), 
                       data = panel,
                       index = c("Country", "Year"),
                       model = "pooling")
summary(pooledOLSrgdppc)
coeftest(pooledOLSrgdppc, vcovHC(pooledOLSrgdppc, type = "HC3", method = "arellano"))

pooledOLSrgdppcSquared <- plm(formula = GROWTH ~ TEA + I(TEA^2) + lag(LOGRGDPPCestimate), 
                       data = panel,
                       index = c("Country", "Year"),
                       model = "pooling")
summary(pooledOLSrgdppcSquared)
coeftest(pooledOLSrgdppcSquared, vcovHC(pooledOLSrgdppcSquared, type = "HC3", method = "arellano"))

# Add interaction terms
pooledOLSrgdppcInteraction <- plm(formula = GROWTH ~ TEA + I(lag(LOGRGDPPCestimate)*TEA) + lag(LOGRGDPPCestimate), 
                       data = panel,
                       index = c("Country", "Year"),
                       model = "pooling")
summary(pooledOLSrgdppcInteraction)
coeftest(pooledOLSrgdppcInteraction, vcovHC(pooledOLSrgdppcInteraction, type = "HC3", method = "arellano"))

pooledOLSrgdppcInteractionAugmented <- plm(formula = GROWTH ~ TEA + I(TEA^2) + lag(LOGRGDPPCestimate)+ I(lag(LOGRGDPPCestimate)*TEA) + I(lag(LOGRGDPPCestimate)*TEA^2), 
                              data = panel,
                              index = c("Country", "Year"),
                              model = "pooling")
summary(pooledOLSrgdppcInteractionAugmented)
coeftest(pooledOLSrgdppcInteractionAugmented, vcovHC(pooledOLSrgdppcInteractionAugmented, type = "HC3", method = "arellano"))

# Calculate average TEA over time for each country
avgTEA <- aggregate(TEA ~ Country, data = panel, FUN = mean)
panel <- merge(panel, avgTEA, by = "Country", suffixes = c("", "_avg"))

# Random effects
randomEffects <- plm(formula = GROWTH ~ TEA,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "random")

randomEffectsSquared <- plm(formula = GROWTH ~ TEA + I(TEA^2),
                            data = panel,
                            index = c("Country", "Year"),
                            model = "random")

randomTwowayEffects <- plm(formula = GROWTH ~ TEA,
                           data = panel,
                           index = c("Country", "Year"),
                           model = "random",
                           effect = "twoways")

randomTwowayEffectsSquared <- plm(formula = GROWTH ~ TEA + I(TEA^2),
                           data = panel,
                           index = c("Country", "Year"),
                           model = "random",
                           effect = "twoways")

summary(randomEffects)
summary(randomEffectsSquared)
summary(randomTwowayEffects)
summary(randomTwowayEffectsSquared)

# Pooled OLS or random effects
phtest(randomTwowayEffectsSquared, pooledOLSSquared)

# Random or fixed effects
correlatedRandomEffects <- plm(formula = GROWTH ~ TEA + TEA_avg,
                               data = panel,
                               index = c("Country", "Year"),
                               model = "random")
summary(correlatedRandomEffects)
coeftest(correlatedRandomEffects, vcovHC(correlatedRandomEffects, type = "HC3", method = "arellano"))

correlatedRandomTwowayEffects <- plm(formula = GROWTH ~ TEA + TEA_avg,
                                     data = panel,
                                     index = c("Country", "Year"),
                                     model = "random",
                                     effect = "twoways")
summary(correlatedRandomTwowayEffects)
coeftest(correlatedRandomTwowayEffects, vcovHC(correlatedRandomTwowayEffects, type = "HC3", method = "arellano"))

# Fixed effects

# Models of interest

fixedEffects1 <- plm(formula = GROWTH ~ TEA + I(TEA^2) + FDI,
                                    data = panel,
                                    index = c("Country", "Year"),
                                    model = "within",
                                    effect = "twoways")
summary(fixedEffects1)
coeftest(fixedEffects1, vcov = vcovHC(fixedEffects1, type = "HC3", method = "arellano"))

fixedEffects2 <- plm(formula = GROWTH ~ TEA + I(TEA*FDI) + FDI,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects2)
coeftest(fixedEffects2, vcov = vcovHC(fixedEffects2, type = "HC3", method = "arellano"))

fixedEffects3 <- plm(formula = GROWTH ~ TEA + I(TEA*RD) + FDI + RD + URBAN,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects3)
coeftest(fixedEffects3, vcov = vcovHC(fixedEffects3, type = "HC3", method = "arellano"))

fixedEffects4 <- plm(formula = GROWTH ~ TEA + I(TEA*URBAN) + FDI + RD + URBAN,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects4)
coeftest(fixedEffects4, vcov = vcovHC(fixedEffects4, type = "HC3", method = "arellano"))

fixedEffects5 <- plm(formula = GROWTH ~ TEA + I(TEA*EDUC) + FDI + EDUC + GCF + TRADE,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects5)
coeftest(fixedEffects5, vcov = vcovHC(fixedEffects5, type = "HC3", method = "arellano"))

fixedEffects6 <- plm(formula = GROWTH ~ TEA + I(TEA*UNEMPLOYMENT) + FDI + UNEMPLOYMENT + INFLATION + GOVT,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects6)
coeftest(fixedEffects6, vcov = vcovHC(fixedEffects6, type = "HC3", method = "arellano"))

fixedEffects7 <- plm(formula = GROWTH ~ TEA + I(TEA*CREDIT) + FDI + CREDIT + BROADBAND + POLSTAB,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects7)
coeftest(fixedEffects7, vcov = vcovHC(fixedEffects7, type = "HC3", method = "arellano"))

fixedEffects8 <- plm(formula = GROWTH ~ TEA + I(TEA*BROADBAND) + FDI + CREDIT + BROADBAND + POLSTAB,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects8)
coeftest(fixedEffects8, vcov = vcovHC(fixedEffects8, type = "HC3", method = "arellano"))

# Put regression results into tex code for a table
models <- list(fixedEffects1, fixedEffects2, fixedEffects3,
               fixedEffects4, fixedEffects5, fixedEffects6,
               fixedEffects7, fixedEffects8)
texreg(models, 
      stars = c(0.01, 0.05, 0.10),
      custom.model.names = c("1", "2", "3", "4",
      "5", "6", "7", "8"),
      digits = 3,
      symbol = ".",
      center = TRUE,
      caption = "Regression Results, Part 1",
      caption.above = TRUE,
      dcolumn = TRUE,
      sideways = TRUE)

# Repeat regression using lags for TEA and FDI
fixedEffects1 <- plm(formula = GROWTH ~ lag(TEA) + I(lag(TEA)^2) + lag(FDI),
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects1)
coeftest(fixedEffects1, vcov = vcovHC(fixedEffects1, type = "HC3", method = "arellano"))

fixedEffects2 <- plm(formula = GROWTH ~ lag(TEA) + I(lag(TEA)*lag(FDI)) + lag(FDI),
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects2)
coeftest(fixedEffects2, vcov = vcovHC(fixedEffects2, type = "HC3", method = "arellano"))

fixedEffects3 <- plm(formula = GROWTH ~ lag(TEA) + I(lag(TEA)*RD) + lag(FDI) + RD + URBAN,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects3)
coeftest(fixedEffects3, vcov = vcovHC(fixedEffects3, type = "HC3", method = "arellano"))

fixedEffects4 <- plm(formula = GROWTH ~ lag(TEA) + I(lag(TEA)*URBAN) + lag(FDI) + RD + URBAN,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects4)
coeftest(fixedEffects4, vcov = vcovHC(fixedEffects4, type = "HC3", method = "arellano"))

fixedEffects5 <- plm(formula = GROWTH ~ lag(TEA) + I(lag(TEA)*EDUC) + lag(FDI) + EDUC + GCF + TRADE,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects5)
coeftest(fixedEffects5, vcov = vcovHC(fixedEffects5, type = "HC3", method = "arellano"))

fixedEffects6 <- plm(formula = GROWTH ~ lag(TEA) + I(lag(TEA)*UNEMPLOYMENT) + lag(FDI) + UNEMPLOYMENT + INFLATION + GOVT,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects6)
coeftest(fixedEffects6, vcov = vcovHC(fixedEffects6, type = "HC3", method = "arellano"))

fixedEffects7 <- plm(formula = GROWTH ~ lag(TEA) + I(lag(TEA)*CREDIT) + lag(FDI) + CREDIT + BROADBAND + POLSTAB,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects7)
coeftest(fixedEffects7, vcov = vcovHC(fixedEffects7, type = "HC3", method = "arellano"))

fixedEffects8 <- plm(formula = GROWTH ~ lag(TEA) + I(lag(TEA)*BROADBAND) + lag(FDI) + CREDIT + BROADBAND + POLSTAB,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within",
                     effect = "twoways")
summary(fixedEffects8)
coeftest(fixedEffects8, vcov = vcovHC(fixedEffects8, type = "HC3", method = "arellano"))

# More fixed effects models

fixedEffects <- plm(formula = GROWTH ~ TEA,
                     data = panel,
                     index = c("Country", "Year"),
                     model = "within")
summary(fixedEffects)
coeftest(fixedEffects, vcov = vcovHC(fixedEffects, type = "HC3", method = "arellano"))

fixedEffectsSquared <- plm(formula = GROWTH ~ TEA + I(TEA^2),
                            data = panel,
                            index = c("Country", "Year"),
                            model = "within")
summary(fixedEffectsSquared)
coeftest(fixedEffectsSquared, vcov = vcovHC(fixedEffectsSquared, type = "HC3", method = "arellano"))

fixedTwowayEffects <- plm(formula = GROWTH ~ TEA,
                           data = panel,
                           index = c("Country", "Year"),
                           model = "within",
                           effect = "twoways")
summary(fixedTwowayEffects)
coeftest(fixedTwowayEffects, vcov = vcovHC(fixedTwowayEffects, type = "HC3", method = "arellano"))

fixedTwowayEffectsSquared <- plm(formula = GROWTH ~ TEA + I(TEA^2),
                                  data = panel,
                                  index = c("Country", "Year"),
                                  model = "within",
                                  effect = "twoways")
summary(fixedTwowayEffectsSquared)
coeftest(fixedTwowayEffectsSquared, vcov = vcovHC(fixedTwowayEffectsSquared, type = "HC3", method = "arellano"))

# Add lag of log real GDP per capita estimate to fixed effects
fixedEffectsR <- plm(formula = GROWTH ~ TEA + lag(LOGRGDPPCestimate),
                    data = panel,
                    index = c("Country", "Year"),
                    model = "within")
summary(fixedEffectsR)
coeftest(fixedEffectsR, vcov = vcovHC(fixedEffectsR, type = "HC3", method = "arellano"))

fixedEffectsSquaredR <- plm(formula = GROWTH ~ TEA + I(TEA^2) + lag(LOGRGDPPCestimate),
                           data = panel,
                           index = c("Country", "Year"),
                           model = "within")
summary(fixedEffectsSquaredR)
coeftest(fixedEffectsSquaredR, vcov = vcovHC(fixedEffectsSquaredR, type = "HC3", method = "arellano"))

fixedTwowayEffectsR <- plm(formula = GROWTH ~ TEA + lag(LOGRGDPPCestimate),
                          data = panel,
                          index = c("Country", "Year"),
                          model = "within",
                          effect = "twoways")
summary(fixedTwowayEffectsR)
coeftest(fixedTwowayEffectsR, vcov = vcovHC(fixedTwowayEffectsR, type = "HC3", method = "arellano"))

fixedTwowayEffectsSquaredR <- plm(formula = GROWTH ~ TEA + I(TEA^2) + lag(LOGRGDPPCestimate),
                                 data = panel,
                                 index = c("Country", "Year"),
                                 model = "within",
                                 effect = "twoways")
summary(fixedTwowayEffectsSquaredR)
coeftest(fixedTwowayEffectsSquaredR, vcov = vcovHC(fixedTwowayEffectsSquaredR, type = "HC3", method = "arellano"))

fixedTwowayEffectsInteractionR <- plm(formula = GROWTH ~ TEA + I(TEA*lag(LOGRGDPPCestimate) + lag(LOGRGDPPCestimate)),
                                      data = panel,
                                      index = c("Country", "Year"),
                                      model = "within")
summary(fixedTwowayEffectsInteractionR)
coeftest(fixedTwowayEffectsInteractionR, vcov = vcovHC(fixedTwowayEffectsInteractionR, type = "HC3", method = "arellano"))

# Testing how good the estimate of LOGRGDPPC is

estimate1 <- plm(formula = LOGRGDPPC ~ LOGRGDPPCestimate,
                 data = panel,
                 index = c("Country", "Year"),
                 model = "pooling")
summary(estimate1)
coeftest(estimate1, vcov = vcovHC(estimate1, type = "HC3", method = "arellano"))

estimate2 <- plm(formula = LOGRGDPPC ~ LOGRGDPPCestimate,
                 data = panel,
                 index = c("Country", "Year"),
                 model = "within")
summary(estimate2)
coeftest(estimate2, vcov = vcovHC(estimate2, type = "HC3", method = "arellano"))

estimate3 <- plm(formula = LOGRGDPPC ~ LOGRGDPPCestimate,
                                      data = panel,
                                      index = c("Country", "Year"),
                                      model = "within",
                                      effect = "twoways")
summary(estimate3)
coeftest(estimate3, vcov = vcovHC(estimate3, type = "HC3", method = "arellano"))

# Introduce FDI



# Introduce other control variables

fixedTwowayEffectsSquaredFinancial <- plm(formula = GROWTH ~ TEA + I(TEA^2) + FDI + CREDIT,
                                    data = panel,
                                    index = c("Country", "Year"),
                                    model = "within",
                                    effect = "twoways")
summary(fixedTwowayEffectsSquaredFinancial)
coeftest(fixedTwowayEffectsSquaredFinancial, vcov = vcovHC(fixedTwowayEffectsSquaredFinancial, type = "HC3", method = "arellano"))

fixedTwowayEffectsSquaredInnovation <- plm(formula = GROWTH ~ TEA + I(TEA^2) + FDI + RD,
                                           data = panel,
                                           index = c("Country", "Year"),
                                           model = "within",
                                           effect = "twoways")
summary(fixedTwowayEffectsSquaredInnovation)
coeftest(fixedTwowayEffectsSquaredInnovation, vcov = vcovHC(fixedTwowayEffectsSquaredInnovation, type = "HC3", method = "arellano"))

fixedTwowayEffectsSquaredAgglomeration <- plm(formula = GROWTH ~ TEA + I(TEA^2) + FDI + URBAN,
                                           data = panel,
                                           index = c("Country", "Year"),
                                           model = "within",
                                           effect = "twoways")
summary(fixedTwowayEffectsSquaredAgglomeration)
coeftest(fixedTwowayEffectsSquaredAgglomeration, vcov = vcovHC(fixedTwowayEffectsSquaredAgglomeration, type = "HC3", method = "arellano"))

fixedTwowayEffectsSquaredProductionFactors <- plm(formula = GROWTH ~ TEA + I(TEA^2) + FDI + EDUC + GCF + TRADE,
                                              data = panel,
                                              index = c("Country", "Year"),
                                              model = "within",
                                              effect = "twoways")
summary(fixedTwowayEffectsSquaredProductionFactors)
coeftest(fixedTwowayEffectsSquaredProductionFactors, vcov = vcovHC(fixedTwowayEffectsSquaredProductionFactors, type = "HC3", method = "arellano"))

fixedTwowayEffectsSquaredProductionFactors2 <- plm(formula = GROWTH ~ TEA + I(TEA^2) + FDI + GCF + TRADE,
                                                  data = panel,
                                                  index = c("Country", "Year"),
                                                  model = "within",
                                                  effect = "twoways")
summary(fixedTwowayEffectsSquaredProductionFactors2)
coeftest(fixedTwowayEffectsSquaredProductionFactors2, vcov = vcovHC(fixedTwowayEffectsSquaredProductionFactors2, type = "HC3", method = "arellano"))

fixedTwowayEffectsSquaredProductionFactors3 <- plm(formula = GROWTH ~ TEA + I(TEA^2) + FDI + EDUC,
                                                   data = panel,
                                                   index = c("Country", "Year"),
                                                   model = "within",
                                                   effect = "twoways")
summary(fixedTwowayEffectsSquaredProductionFactors3)
coeftest(fixedTwowayEffectsSquaredProductionFactors3, vcov = vcovHC(fixedTwowayEffectsSquaredProductionFactors3, type = "HC3", method = "arellano"))

fixedTwowayEffectsSquaredMacroeconomic <- plm(formula = GROWTH ~ TEA + I(TEA^2) + FDI + UNEMPLOYMENT + INFLATION,
                                                  data = panel,
                                                  index = c("Country", "Year"),
                                                  model = "within",
                                                  effect = "twoways")
summary(fixedTwowayEffectsSquaredMacroeconomic)
coeftest(fixedTwowayEffectsSquaredMacroeconomic, vcov = vcovHC(fixedTwowayEffectsSquaredMacroeconomic, type = "HC3", method = "arellano"))

fixedTwowayEffectsSquaredPublicSector <- plm(formula = GROWTH ~ TEA + I(TEA^2) + FDI + GOVT,
                                             data = panel,
                                             index = c("Country", "Year"),
                                             model = "within",
                                             effect = "twoways")
summary(fixedTwowayEffectsSquaredPublicSector)
coeftest(fixedTwowayEffectsSquaredPublicSector, vcov = vcovHC(fixedTwowayEffectsSquaredPublicSector, type = "HC3", method = "arellano"))

fixedTwowayEffectsSquaredPoliticalStability <- plm(formula = GROWTH ~ TEA + I(TEA^2) + FDI + POLSTAB,
                                             data = panel,
                                             index = c("Country", "Year"),
                                             model = "within",
                                             effect = "twoways")
summary(fixedTwowayEffectsSquaredPoliticalStability)
coeftest(fixedTwowayEffectsSquaredPoliticalStability, vcov = vcovHC(fixedTwowayEffectsSquaredPoliticalStability, type = "HC3", method = "arellano"))

fixedTwowayEffectsSquaredInfrastructure <- plm(formula = GROWTH ~ TEA + I(TEA^2) + FDI + BROADBAND,
                                                   data = panel,
                                                   index = c("Country", "Year"),
                                                   model = "within",
                                                   effect = "twoways")
summary(fixedTwowayEffectsSquaredInfrastructure)
coeftest(fixedTwowayEffectsSquaredInfrastructure, vcov = vcovHC(fixedTwowayEffectsSquaredInfrastructure, type = "HC3", method = "arellano"))

fixedTwowayEffectsSquaredMultipleControls <- plm(formula = GROWTH ~ TEA + I(TEA^2) + FDI + BROADBAND + INFLATION + POLSTAB + CREDIT,
                                               data = panel,
                                               index = c("Country", "Year"),
                                               model = "within",
                                               effect = "twoways")
summary(fixedTwowayEffectsSquaredMultipleControls)
coeftest(fixedTwowayEffectsSquaredMultipleControls, vcov = vcovHC(fixedTwowayEffectsSquaredMultipleControls, type = "HC3", method = "arellano"))

fixedTwowayEffectsFDIInteraction1 <- plm(formula = GROWTH ~ TEA + I(TEA*FDI) + FDI,
                                         data = panel,
                                         index = c("Country", "Year"),
                                         model = "within",
                                         effect = "twoways")
summary(fixedTwowayEffectsFDIInteraction1)
coeftest(fixedTwowayEffectsFDIInteraction1, vcov = vcovHC(fixedTwowayEffectsFDIInteraction1, type = "HC3", method = "arellano"))

fixedTwowayEffectsFDIInteraction2 <- plm(formula = GROWTH ~ TEA + I(TEA*FDI) + FDI + CREDIT,
                                         data = panel,
                                         index = c("Country", "Year"),
                                         model = "within",
                                         effect = "twoways")
summary(fixedTwowayEffectsFDIInteraction2)
coeftest(fixedTwowayEffectsFDIInteraction2, vcov = vcovHC(fixedTwowayEffectsFDIInteraction2, type = "HC3", method = "arellano"))

fixedTwowayEffectsFDIInteraction3 <- plm(formula = GROWTH ~ TEA + I(TEA*FDI) + FDI + RD,
                                         data = panel,
                                         index = c("Country", "Year"),
                                         model = "within",
                                         effect = "twoways")
summary(fixedTwowayEffectsFDIInteraction3)
coeftest(fixedTwowayEffectsFDIInteraction3, vcov = vcovHC(fixedTwowayEffectsFDIInteraction3, type = "HC3", method = "arellano"))

fixedTwowayEffectsFDIInteraction4 <- plm(formula = GROWTH ~ TEA + I(TEA*FDI) + FDI + GCF + TRADE,
                                         data = panel,
                                         index = c("Country", "Year"),
                                         model = "within",
                                         effect = "twoways")
summary(fixedTwowayEffectsFDIInteraction4)
coeftest(fixedTwowayEffectsFDIInteraction4, vcov = vcovHC(fixedTwowayEffectsFDIInteraction4, type = "HC3", method = "arellano"))

fixedTwowayEffectsFDIInteraction5 <- plm(formula = GROWTH ~ TEA + I(TEA*FDI) + FDI + EDUC,
                                         data = panel,
                                         index = c("Country", "Year"),
                                         model = "within",
                                         effect = "twoways")
summary(fixedTwowayEffectsFDIInteraction5)
coeftest(fixedTwowayEffectsFDIInteraction5, vcov = vcovHC(fixedTwowayEffectsFDIInteraction5, type = "HC3", method = "arellano"))

fixedTwowayEffectsFDIInteraction6 <- plm(formula = GROWTH ~ TEA + I(TEA*FDI) + FDI + UNEMPLOYMENT + INFLATION,
                                         data = panel,
                                         index = c("Country", "Year"),
                                         model = "within",
                                         effect = "twoways")
summary(fixedTwowayEffectsFDIInteraction6)
coeftest(fixedTwowayEffectsFDIInteraction6, vcov = vcovHC(fixedTwowayEffectsFDIInteraction6, type = "HC3", method = "arellano"))

fixedTwowayEffectsFDIInteraction7 <- plm(formula = GROWTH ~ TEA + I(TEA*FDI) + FDI + GOVT,
                                         data = panel,
                                         index = c("Country", "Year"),
                                         model = "within",
                                         effect = "twoways")
summary(fixedTwowayEffectsFDIInteraction7)
coeftest(fixedTwowayEffectsFDIInteraction7, vcov = vcovHC(fixedTwowayEffectsFDIInteraction7, type = "HC3", method = "arellano"))

fixedTwowayEffectsFDIInteraction8 <- plm(formula = GROWTH ~ TEA + I(TEA*FDI) + FDI + POLSTAB,
                                         data = panel,
                                         index = c("Country", "Year"),
                                         model = "within",
                                         effect = "twoways")
summary(fixedTwowayEffectsFDIInteraction8)
coeftest(fixedTwowayEffectsFDIInteraction8, vcov = vcovHC(fixedTwowayEffectsFDIInteraction8, type = "HC3", method = "arellano"))

fixedTwowayEffectsFDIInteraction9 <- plm(formula = GROWTH ~ TEA + I(TEA*FDI) + FDI + BROADBAND,
                                         data = panel,
                                         index = c("Country", "Year"),
                                         model = "within",
                                         effect = "twoways")
summary(fixedTwowayEffectsFDIInteraction9)
coeftest(fixedTwowayEffectsFDIInteraction9, vcov = vcovHC(fixedTwowayEffectsFDIInteraction9, type = "HC3", method = "arellano"))

fixedTwowayEffectsInnovationInteraction <- plm(formula = GROWTH ~ TEA + I(TEA*RD) + RD,
                                               data = panel,
                                               index = c("Country", "Year"),
                                               model = "within",
                                               effect = "twoways")
summary(fixedTwowayEffectsInnovationInteraction)
coeftest(fixedTwowayEffectsInnovationInteraction, vcov = vcovHC(fixedTwowayEffectsInnovationInteraction, type = "HC3", method = "arellano"))

fixedTwowayEffectsAgglomerationInteraction <- plm(formula = GROWTH ~ TEA + I(TEA*URBAN) + URBAN,
                                               data = panel,
                                               index = c("Country", "Year"),
                                               model = "within",
                                               effect = "twoways")
summary(fixedTwowayEffectsAgglomerationInteraction)
coeftest(fixedTwowayEffectsAgglomerationInteraction, vcov = vcovHC(fixedTwowayEffectsAgglomerationInteraction, type = "HC3", method = "arellano"))

fixedTwowayEffectsEducationInteraction <- plm(formula = GROWTH ~ TEA + I(TEA*EDUC) + EDUC,
                                                 data = panel,
                                                 index = c("Country", "Year"),
                                                 model = "within",
                                                 effect = "twoways")
summary(fixedTwowayEffectsEducationInteraction)
coeftest(fixedTwowayEffectsEducationInteraction, vcov = vcovHC(fixedTwowayEffectsEducationInteraction, type = "HC3", method = "arellano"))

fixedTwowayEffectsUnemploymentInteraction <- plm(formula = GROWTH ~ TEA + I(TEA*UNEMPLOYMENT) + UNEMPLOYMENT + INFLATION,
                                                  data = panel,
                                                  index = c("Country", "Year"),
                                                  model = "within",
                                                  effect = "twoways")
summary(fixedTwowayEffectsUnemploymentInteraction)
coeftest(fixedTwowayEffectsUnemploymentInteraction, vcov = vcovHC(fixedTwowayEffectsUnemploymentInteraction, type = "HC3", method = "arellano"))

fixedTwowayEffectsInfrastructureInteraction <- plm(formula = GROWTH ~ TEA + I(TEA*BROADBAND) + BROADBAND,
                                                 data = panel,
                                                 index = c("Country", "Year"),
                                                 model = "within",
                                                 effect = "twoways")
summary(fixedTwowayEffectsInfrastructureInteraction)
coeftest(fixedTwowayEffectsInfrastructureInteraction, vcov = vcovHC(fixedTwowayEffectsInfrastructureInteraction, type = "HC3", method = "arellano"))

# Unit root tests
panel$time_trend <- as.numeric(panel$Year)

fixedEffectsUnitRootLOGRGDPPC <- plm(formula = diff(LOGRGDPPC) ~ lag(LOGRGDPPC) + diff(lag(LOGRGDPPC))
                                     + diff(lag(LOGRGDPPC, 2)) + diff(lag(LOGRGDPPC, 3)) + diff(lag(LOGRGDPPC, 4)) + time_trend,
                         data = panel,
                         index = c("Country", "Year"),
                         model = "within")
summary(fixedEffectsUnitRootLOGRGDPPC)
coeftest(fixedEffectsUnitRootLOGRGDPPC, vcov = vcovHC(fixedEffectsUnitRootLOGRGDPPC, type = "HC3", method = "arellano"))

fixedEffectsUnitRootGROWTH <- plm(formula = diff(GROWTH) ~ lag(GROWTH) + diff(lag(GROWTH))
                                     + diff(lag(GROWTH, 2)) + diff(lag(GROWTH, 3)) + diff(lag(GROWTH, 4)) + time_trend,
                                     data = panel,
                                     index = c("Country", "Year"),
                                     model = "within")
summary(fixedEffectsUnitRootGROWTH)
coeftest(fixedEffectsUnitRootGROWTH, vcov = vcovHC(fixedEffectsUnitRootGROWTH, type = "HC3", method = "arellano"))

fixedEffectsUnitRootTEA <- plm(formula = diff(TEA) ~ lag(TEA) + diff(lag(TEA))
                                     + diff(lag(TEA, 2)) + diff(lag(TEA, 3)) + diff(lag(TEA, 4)) + time_trend,
                                     data = panel,
                                     index = c("Country", "Year"),
                                     model = "within")
summary(fixedEffectsUnitRootTEA)
coeftest(fixedEffectsUnitRootTEA, vcov = vcovHC(fixedEffectsUnitRootTEA, type = "HC3", method = "arellano"))

fixedEffectsUnitRootFDI <- plm(formula = diff(FDI) ~ lag(FDI) + diff(lag(FDI))
                               + diff(lag(FDI, 2)) + diff(lag(FDI, 3)) + diff(lag(FDI, 4)) + time_trend,
                               data = panel,
                               index = c("Country", "Year"),
                               model = "within")
summary(fixedEffectsUnitRootFDI)
coeftest(fixedEffectsUnitRootFDI, vcov = vcovHC(fixedEffectsUnitRootFDI, type = "HC3", method = "arellano"))




# Between models

# Create data with averages for the different countries
averages <- data %>%
  group_by(Country) %>%
  summarise(
    avgGROWTH = mean(GROWTH, na.rm = TRUE),
    avgTEA = mean(TEA, na.rm = TRUE),
    avgFDI = mean(FDI, na.rm = TRUE),
    INITIAL = mean(INITIAL, na.rm = TRUE),
    avgCREDIT = mean(CREDIT, na.rm = TRUE),
    avgRD = mean(RD, na.rm = TRUE),
    avgLOGPATENT = mean(LOGPATENT, na.rm = TRUE),
    avgURBAN = mean(URBAN, na.rm = TRUE),
    avgEDUC = mean(EDUC, na.rm = TRUE),
    avgGCF = mean(GCF, na.rm = TRUE),
    avgTRADE = mean(TRADE, na.rm = TRUE),
    avgUNEMPLOYMENT = mean(UNEMPLOYMENT, na.rm = TRUE),
    avgINFLATION = mean(INFLATION, na.rm = TRUE),
    avgGOVT = mean(GOVT, na.rm = TRUE),
    avgPOLSTAB = mean(POLSTAB, na.rm = TRUE),
    avgBROADBAND = mean(BROADBAND, na.rm = TRUE),
    avgMOBILE = mean(MOBILE, na.rm = TRUE),
    avgTELEPHONE = mean(TELEPHONE, na.rm = TRUE)
  )

# Filter the data
averages <- averages %>%
  filter(!(is.na(avgGROWTH) & is.na(avgTEA)))

# View the resulting data frame
View(averages)

# Run the models

# Key models

betweenInitialSquared <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA^2) + avgFDI + INITIAL, 
                            data = averages)
summary(betweenInitialSquared)
coeftest(betweenInitialSquared, vcov = vcovHC(betweenInitialSquared, type = "HC3"))

betweenInitialInteraction <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*INITIAL) + INITIAL + avgFDI, 
                                data = averages)
summary(betweenInitialInteraction)
coeftest(betweenInitialInteraction, vcov = vcovHC(betweenInitialInteraction, type = "HC3"))

betweenFDIInteraction <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgFDI) + avgFDI + INITIAL, 
                            data = averages)
summary(betweenFDIInteraction)
coeftest(betweenFDIInteraction, vcov = vcovHC(betweenFDIInteraction, type = "HC3"))

betweenRDUrban1 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgRD) + avgFDI + INITIAL + avgRD + avgURBAN, 
                     data = averages)
summary(betweenRDUrban1)
coeftest(betweenRDUrban1, vcov = vcovHC(betweenRDUrban1, type = "HC3"))

betweenRDUrban2 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgURBAN) + avgFDI + INITIAL + avgRD + avgURBAN, 
                      data = averages)
summary(betweenRDUrban2)
coeftest(betweenRDUrban2, vcov = vcovHC(betweenRDUrban2, type = "HC3"))

betweenProductionInputs <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgEDUC) + avgFDI + INITIAL + avgEDUC + avgGCF + avgTRADE, 
                              data = averages)
summary(betweenProductionInputs)
coeftest(betweenProductionInputs, vcov = vcovHC(betweenProductionInputs, type = "HC3"))

betweenMacroeconomicGovernment <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgUNEMPLOYMENT) + avgFDI + INITIAL + 
                                       avgUNEMPLOYMENT + avgINFLATION + avgGOVT, 
                              data = averages)
summary(betweenMacroeconomicGovernment)
coeftest(betweenMacroeconomicGovernment, vcov = vcovHC(betweenMacroeconomicGovernment, type = "HC3"))

betweenMiscellaneous1 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgCREDIT) + avgFDI + INITIAL + 
                                       avgCREDIT + avgBROADBAND + avgPOLSTAB, 
                                     data = averages)
summary(betweenMiscellaneous1)
coeftest(betweenMiscellaneous1, vcov = vcovHC(betweenMiscellaneous1, type = "HC3"))

betweenMiscellaneous2 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgBROADBAND) + avgFDI + INITIAL + 
                              avgCREDIT + avgBROADBAND + avgPOLSTAB, 
                            data = averages)
summary(betweenMiscellaneous2)
coeftest(betweenMiscellaneous2, vcov = vcovHC(betweenMiscellaneous2, type = "HC3"))

# Put regression results into tex code for a table
models <- list(betweenInitialSquared, betweenInitialInteraction, betweenFDIInteraction,
                 betweenRDUrban1, betweenRDUrban2, betweenProductionInputs,
                 betweenMacroeconomicGovernment, betweenMiscellaneous1, betweenMiscellaneous2)
texreg(models, 
      stars = c(0.01, 0.05, 0.10),
      custom.model.names = c("1", "2", "3", "4",
                            "5", "6", "7", "8", "9"),
      digits = 3,
      symbol = ".",
      center = TRUE,
      caption = "Regression Results, Part 1",
      caption.above = TRUE,
      dcolumn = TRUE,
      sideways = TRUE)

# Other models

betweenBasic <- lm(formula = avgGROWTH ~ avgTEA, 
                    data = averages)
summary(betweenBasic)
coeftest(betweenBasic, vcov = vcovHC(betweenBasic, type = "HC3"))

betweenSquared <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA^2), 
                     data = averages)
summary(betweenSquared)
coeftest(betweenSquared, vcov = vcovHC(betweenSquared, type = "HC3"))

betweenFDI <- lm(formula = avgGROWTH ~ avgTEA + avgFDI, 
                 data = averages)
summary(betweenFDI)
coeftest(betweenFDI, vcov = vcovHC(betweenFDI, type = "HC3"))

betweenFDISquared <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA^2) + avgFDI, 
                 data = averages)
summary(betweenFDISquared)
coeftest(betweenFDISquared, vcov = vcovHC(betweenFDISquared, type = "HC3"))

betweenInitial <- lm(formula = avgGROWTH ~ avgTEA + avgFDI + INITIAL, 
                      data = averages)
summary(betweenInitial)
coeftest(betweenInitial, vcov = vcovHC(betweenInitial, type = "HC3"))



betweenFinancial <- lm(formula = avgGROWTH ~ avgTEA + avgFDI + INITIAL + avgCREDIT, 
                        data = averages)
summary(betweenFinancial)
coeftest(betweenFinancial, vcov = vcovHC(betweenFinancial, type = "HC3"))

betweenInnovation <- lm(formula = avgGROWTH ~ avgTEA + avgFDI + INITIAL + avgRD, 
                         data = averages)
summary(betweenInnovation)
coeftest(betweenInnovation, vcov = vcovHC(betweenInnovation, type = "HC3"))

betweenInnovationAgglomeration <- lm(formula = avgGROWTH ~ avgTEA + avgFDI + INITIAL + avgURBAN, 
                        data = averages)
summary(betweenInnovationAgglomeration)
coeftest(betweenInnovationAgglomeration, vcov = vcovHC(betweenInnovationAgglomeration, type = "HC3"))

betweenProductionFactors <- lm(formula = avgGROWTH ~ avgTEA + avgFDI + INITIAL + avgEDUC + avgGCF + avgTRADE, 
                                data = averages)
summary(betweenProductionFactors)
coeftest(betweenProductionFactors, vcov = vcovHC(betweenProductionFactors, type = "HC3"))

betweenMacroeconomic <- lm(formula = avgGROWTH ~ avgTEA + avgFDI + INITIAL + avgUNEMPLOYMENT + avgINFLATION, 
                            data = averages)
summary(betweenMacroeconomic)
coeftest(betweenMacroeconomic, vcov = vcovHC(betweenMacroeconomic, type = "HC3"))

betweenPublicSector <- lm(formula = avgGROWTH ~ avgTEA + avgFDI + INITIAL + avgGOVT, 
                           data = averages)
summary(betweenPublicSector)
coeftest(betweenPublicSector, vcov = vcovHC(betweenPublicSector, type = "HC3"))

betweenPoliticalStability <- lm(formula = avgGROWTH ~ avgTEA + avgFDI + INITIAL + avgPOLSTAB, 
                          data = averages)
summary(betweenPoliticalStability)
coeftest(betweenPoliticalStability, vcov = vcovHC(betweenPoliticalStability, type = "HC3"))

betweenInfrastructure <- lm(formula = avgGROWTH ~ avgTEA + avgFDI + INITIAL + avgBROADBAND, 
                            data = averages)
summary(betweenInfrastructure)
coeftest(betweenInfrastructure, vcov = vcovHC(betweenInfrastructure, type = "HC3"))

betweenMultipleControls <- lm(formula = avgGROWTH ~ avgTEA + avgFDI + INITIAL + avgBROADBAND + avgINFLATION + avgPOLSTAB + avgCREDIT, 
              data = averages)
summary(betweenMultipleControls)
coeftest(betweenMultipleControls, vcov = vcovHC(betweenMultipleControls, type = "HC3"))

betweenInitialInteraction1 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*INITIAL) + INITIAL, 
                          data = averages)
summary(betweenInitialInteraction1)
coeftest(betweenInitialInteraction1, vcov = vcovHC(betweenInitialInteraction1, type = "HC3"))



betweenInitialInteraction3 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*INITIAL) + INITIAL + avgFDI + avgCREDIT, 
                                 data = averages)
summary(betweenInitialInteraction3)
coeftest(betweenInitialInteraction3, vcov = vcovHC(betweenInitialInteraction3, type = "HC3"))

betweenInitialInteraction4 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*INITIAL) + INITIAL + avgFDI + avgRD, 
                                 data = averages)
summary(betweenInitialInteraction4)
coeftest(betweenInitialInteraction4, vcov = vcovHC(betweenInitialInteraction4, type = "HC3"))

betweenInitialInteraction5 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*INITIAL) + INITIAL + avgFDI + avgURBAN, 
                                 data = averages)
summary(betweenInitialInteraction5)
coeftest(betweenInitialInteraction5, vcov = vcovHC(betweenInitialInteraction5, type = "HC3"))

betweenInitialInteraction6 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*INITIAL) + INITIAL + avgFDI + avgEDUC + avgGCF + avgTRADE, 
                                 data = averages)
summary(betweenInitialInteraction6)
coeftest(betweenInitialInteraction6, vcov = vcovHC(betweenInitialInteraction6, type = "HC3"))

betweenInitialInteraction7 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*INITIAL) + INITIAL + avgFDI + avgUNEMPLOYMENT + avgINFLATION, 
                                 data = averages)
summary(betweenInitialInteraction7)
coeftest(betweenInitialInteraction7, vcov = vcovHC(betweenInitialInteraction7, type = "HC3"))

betweenInitialInteraction8 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*INITIAL) + INITIAL + avgFDI + avgGOVT, 
                                 data = averages)
summary(betweenInitialInteraction8)
coeftest(betweenInitialInteraction8, vcov = vcovHC(betweenInitialInteraction8, type = "HC3"))

betweenInitialInteraction9 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*INITIAL) + INITIAL + avgFDI + avgPOLSTAB, 
                                 data = averages)
summary(betweenInitialInteraction9)
coeftest(betweenInitialInteraction9, vcov = vcovHC(betweenInitialInteraction9, type = "HC3"))

betweenInitialInteraction10 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*INITIAL) + INITIAL + avgFDI + avgBROADBAND, 
                                 data = averages)
summary(betweenInitialInteraction10)
coeftest(betweenInitialInteraction10, vcov = vcovHC(betweenInitialInteraction10, type = "HC3"))


betweenFDIInteraction1 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgFDI) + avgFDI, 
                             data = averages)
summary(betweenFDIInteraction1)
coeftest(betweenFDIInteraction1, vcov = vcovHC(betweenFDIInteraction1, type = "HC3"))



betweenFDIInteraction3 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgFDI) + avgFDI + INITIAL + avgCREDIT, 
                        data = averages)
summary(betweenFDIInteraction3)
coeftest(betweenFDIInteraction3, vcov = vcovHC(betweenFDIInteraction3, type = "HC3"))

betweenFDIInteraction4 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgFDI) + avgFDI + INITIAL + avgCREDIT, 
                             data = averages)
summary(betweenFDIInteraction4)
coeftest(betweenFDIInteraction4, vcov = vcovHC(betweenFDIInteraction4, type = "HC3"))

betweenFDIInteraction5 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgFDI) + avgFDI + INITIAL + avgRD, 
                             data = averages)
summary(betweenFDIInteraction5)
coeftest(betweenFDIInteraction5, vcov = vcovHC(betweenFDIInteraction5, type = "HC3"))

betweenFDIInteraction6 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgFDI) + avgFDI + INITIAL + avgURBAN, 
                             data = averages)
summary(betweenFDIInteraction6)
coeftest(betweenFDIInteraction6, vcov = vcovHC(betweenFDIInteraction6, type = "HC3"))

betweenFDIInteraction6 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgFDI) + avgFDI + INITIAL + avgURBAN, 
                             data = averages)
summary(betweenFDIInteraction6)
coeftest(betweenFDIInteraction6, vcov = vcovHC(betweenFDIInteraction6, type = "HC3"))

betweenFDIInteraction7 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgFDI) + avgFDI + INITIAL + avgEDUC + avgGCF + avgTRADE, 
                             data = averages)
summary(betweenFDIInteraction7)
coeftest(betweenFDIInteraction7, vcov = vcovHC(betweenFDIInteraction7, type = "HC3"))

betweenFDIInteraction8 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgFDI) + avgFDI + INITIAL + avgUNEMPLOYMENT + avgINFLATION, 
                             data = averages)
summary(betweenFDIInteraction8)
coeftest(betweenFDIInteraction8, vcov = vcovHC(betweenFDIInteraction8, type = "HC3"))

betweenFDIInteraction9 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgFDI) + avgFDI + INITIAL + avgGOVT, 
                             data = averages)
summary(betweenFDIInteraction9)
coeftest(betweenFDIInteraction9, vcov = vcovHC(betweenFDIInteraction9, type = "HC3"))

betweenFDIInteraction10 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgFDI) + avgFDI + INITIAL + avgPOLSTAB, 
                             data = averages)
summary(betweenFDIInteraction10)
coeftest(betweenFDIInteraction10, vcov = vcovHC(betweenFDIInteraction10, type = "HC3"))

betweenFDIInteraction11 <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgFDI) + avgFDI + INITIAL + avgBROADBAND, 
                              data = averages)
summary(betweenFDIInteraction11)
coeftest(betweenFDIInteraction11, vcov = vcovHC(betweenFDIInteraction11, type = "HC3"))

betweenInnovationInteraction <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgRD) + avgFDI + INITIAL + avgRD, 
                                     data = averages)
summary(betweenInnovationInteraction)
coeftest(betweenInnovationInteraction, vcov = vcovHC(betweenInnovationInteraction, type = "HC3"))

betweenAgglomerationInteraction <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgURBAN) + avgFDI + INITIAL + avgURBAN, 
                                       data = averages)
summary(betweenAgglomerationInteraction)
coeftest(betweenAgglomerationInteraction, vcov = vcovHC(betweenAgglomerationInteraction, type = "HC3"))

betweenEducInteraction <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgEDUC) + avgFDI + INITIAL + avgEDUC + avgGCF + avgTRADE, 
                              data = averages)
summary(betweenEducInteraction)
coeftest(betweenEducInteraction, vcov = vcovHC(betweenEducInteraction, type = "HC3"))

betweenUnemploymentInteraction <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgUNEMPLOYMENT) + avgFDI + INITIAL + avgUNEMPLOYMENT + avgINFLATION, 
                                      data = averages)
summary(betweenUnemploymentInteraction)
coeftest(betweenUnemploymentInteraction, vcov = vcovHC(betweenUnemploymentInteraction, type = "HC3"))

betweenInfrastructureInteraction <- lm(formula = avgGROWTH ~ avgTEA + I(avgTEA*avgBROADBAND) + avgFDI + INITIAL + avgBROADBAND, 
                                        data = averages)
summary(betweenInfrastructureInteraction)
coeftest(betweenInfrastructureInteraction, vcov = vcovHC(betweenInfrastructureInteraction, type = "HC3"))

# Split data into two periods and aggregate and perform regression models

# Convert Year to numeric
panel_data <- panel %>%
  mutate(Year = as.numeric(as.character(Year)))

# Define Periods
panel_data <- panel_data %>%
  mutate(Period = case_when(
    Year <= 2011 ~ "1",  # Period 1 for 2001-2011
    Year >= 2012 ~ "2",  # Period 2 for 2012-2022
    TRUE ~ NA_character_  # Handle cases where Year does not fit any period
  ))

# Extract initial values for periods 1 and 2
initial_values <- panel_data %>%
  group_by(Country) %>%
  summarise(
    INITIAL_1 = mean(LOGRGDPPC2001, na.rm = TRUE),
    INITIAL_2 = mean(LOGRGDPPC2012, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = starts_with("INITIAL"), names_to = "Period", values_to = "INITIAL") %>%
  mutate(
    Period = case_when(
      Period == "INITIAL_1" ~ "1",
      Period == "INITIAL_2" ~ "2"
    )
  )

# Aggregate averages for all numeric columns
aggregated_values <- panel_data %>%
  group_by(Country, Period) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "avg_{col}"), .groups = 'drop')

# Merge aggregated data with initial values
averages2 <- aggregated_values %>%
  left_join(initial_values, by = c("Country", "Period"))

averages2 <- averages2 %>%
  select(-avg_Year, -avg_INITIAL)

# Convert to pdata.frame
panel_averages2 <- pdata.frame(x = averages2, index = c("Country", "Period"))

# First difference model

fixedEffectsLongRun <- plm(formula = avg_GROWTH ~ avg_TEA, 
                           data = panel_averages2, 
                           index = c("Country", "Period"),
                           model = "within",
                           effect = "twoways")
summary(fixedEffectsLongRun)
coeftest(fixedEffectsLongRun, vcov = vcovHC(fixedEffectsLongRun, type = "HC3"))

fixedEffectsLongRun1 <- plm(formula = avg_GROWTH ~ avg_TEA + I(avg_TEA^2) + avg_FDI + INITIAL, 
                                  data = panel_averages2, 
                                  index = c("Country", "Period"),
                                  model = "within",
                                  effect = "twoways")
summary(fixedEffectsLongRun1)
coeftest(fixedEffectsLongRun1, vcov = vcovHC(fixedEffectsLongRun1, type = "HC3", method = "arellano"))

fixedEffectsLongRun2 <- plm(formula = avg_GROWTH ~ avg_TEA + I(avg_TEA*INITIAL) + avg_FDI + INITIAL, 
                            data = panel_averages2, 
                            index = c("Country", "Period"),
                            model = "within",
                            effect = "twoways")
summary(fixedEffectsLongRun2)
coeftest(fixedEffectsLongRun2, vcov = vcovHC(fixedEffectsLongRun2, type = "HC3", method = "arellano"))

fixedEffectsLongRun3 <- plm(formula = avg_GROWTH ~ avg_TEA + I(avg_TEA*avg_FDI) + avg_FDI + INITIAL, 
                            data = panel_averages2, 
                            index = c("Country", "Period"),
                            model = "within",
                            effect = "twoways")
summary(fixedEffectsLongRun3)
coeftest(fixedEffectsLongRun3, vcov = vcovHC(fixedEffectsLongRun3, type = "HC3", method = "arellano"))

fixedEffectsLongRun4 <- plm(formula = avg_GROWTH ~ avg_TEA + I(avg_TEA*avg_RD) + avg_FDI + INITIAL + avg_RD + avg_URBAN, 
                            data = panel_averages2, 
                            index = c("Country", "Period"),
                            model = "within",
                            effect = "twoways")
summary(fixedEffectsLongRun4)
coeftest(fixedEffectsLongRun4, vcov = vcovHC(fixedEffectsLongRun4, type = "HC3", method = "arellano"))

fixedEffectsLongRun5 <- plm(formula = avg_GROWTH ~ avg_TEA + I(avg_TEA*avg_URBAN) + avg_FDI + INITIAL + avg_RD + avg_URBAN, 
                            data = panel_averages2, 
                            index = c("Country", "Period"),
                            model = "within",
                            effect = "twoways")
summary(fixedEffectsLongRun5)
coeftest(fixedEffectsLongRun5, vcov = vcovHC(fixedEffectsLongRun5, type = "HC3", method = "arellano"))

fixedEffectsLongRun6 <- plm(formula = avg_GROWTH ~ avg_TEA + I(avg_TEA*avg_EDUC) + avg_FDI + INITIAL + avg_EDUC + avg_GCF + avg_TRADE, 
                            data = panel_averages2, 
                            index = c("Country", "Period"),
                            model = "within",
                            effect = "twoways")
summary(fixedEffectsLongRun6)
coeftest(fixedEffectsLongRun6, vcov = vcovHC(fixedEffectsLongRun6, type = "HC3", method = "arellano"))

fixedEffectsLongRun7 <- plm(formula = avg_GROWTH ~ avg_TEA + I(avg_TEA*avg_UNEMPLOYMENT) + avg_FDI + INITIAL + avg_UNEMPLOYMENT + avg_INFLATION
                            + avg_GOVT, 
                            data = panel_averages2, 
                            index = c("Country", "Period"),
                            model = "within",
                            effect = "twoways")
summary(fixedEffectsLongRun7)
coeftest(fixedEffectsLongRun7, vcov = vcovHC(fixedEffectsLongRun7, type = "HC3", method = "arellano"))

fixedEffectsLongRun8 <- plm(formula = avg_GROWTH ~ avg_TEA + I(avg_TEA*avg_CREDIT) + avg_FDI + INITIAL + avg_CREDIT + avg_BROADBAND
                            + avg_POLSTAB, 
                            data = panel_averages2, 
                            index = c("Country", "Period"),
                            model = "within",
                            effect = "twoways")
summary(fixedEffectsLongRun8)
coeftest(fixedEffectsLongRun8, vcov = vcovHC(fixedEffectsLongRun8, type = "HC3", method = "arellano"))

fixedEffectsLongRun9 <- plm(formula = avg_GROWTH ~ avg_TEA + I(avg_TEA*avg_BROADBAND) + avg_FDI + INITIAL + avg_CREDIT + avg_BROADBAND
                            + avg_POLSTAB, 
                            data = panel_averages2, 
                            index = c("Country", "Period"),
                            model = "within",
                            effect = "twoways")
summary(fixedEffectsLongRun9)
coeftest(fixedEffectsLongRun9, vcov = vcovHC(fixedEffectsLongRun9, type = "HC3", method = "arellano"))

# Put regression results into tex code for a table
models <- list(fixedEffectsLongRun1, fixedEffectsLongRun2, fixedEffectsLongRun3,
               fixedEffectsLongRun4, fixedEffectsLongRun5, fixedEffectsLongRun6,
               fixedEffectsLongRun7, fixedEffectsLongRun8, fixedEffectsLongRun9)
texreg(models, 
       stars = c(0.01, 0.05, 0.10),
       custom.model.names = c("1", "2", "3", "4",
                              "5", "6", "7", "8", "9"),
       digits = 3,
       symbol = ".",
       center = TRUE,
       caption = "Regression Results, Part 1",
       caption.above = TRUE,
       dcolumn = TRUE,
       sideways = TRUE)

fixedEffectsLongRunFinancial <- plm(formula = avg_GROWTH ~ avg_TEA + avg_FDI + INITIAL + avg_CREDIT, 
                                    data = panel_averages2, 
                                    index = c("Country", "Period"),
                                    model = "within",
                                    effect = "twoways")
summary(fixedEffectsLongRunFinancial)
coeftest(fixedEffectsLongRunFinancial, vcov = vcovHC(fixedEffectsLongRunFinancial, type = "HC3"))

fixedEffectsLongRunInnovation <- plm(formula = avg_GROWTH ~ avg_TEA + avg_FDI + INITIAL + avg_RD, 
                                    data = panel_averages2, 
                                    index = c("Country", "Period"),
                                    model = "within",
                                    effect = "twoways")
summary(fixedEffectsLongRunInnovation)
coeftest(fixedEffectsLongRunInnovation, vcov = vcovHC(fixedEffectsLongRunInnovation, type = "HC3"))

fixedEffectsLongRunAgglomeration <- plm(formula = avg_GROWTH ~ avg_TEA + avg_FDI + INITIAL + avg_URBAN, 
                                     data = panel_averages2, 
                                     index = c("Country", "Period"),
                                     model = "within",
                                     effect = "twoways")
summary(fixedEffectsLongRunAgglomeration)
coeftest(fixedEffectsLongRunAgglomeration, vcov = vcovHC(fixedEffectsLongRunAgglomeration, type = "HC3"))

fixedEffectsLongRunProductionFactors <- plm(formula = avg_GROWTH ~ avg_TEA + avg_FDI + INITIAL + avg_EDUC + avg_GCF + avg_TRADE, 
                                        data = panel_averages2, 
                                        index = c("Country", "Period"),
                                        model = "within",
                                        effect = "twoways")
summary(fixedEffectsLongRunProductionFactors)
coeftest(fixedEffectsLongRunProductionFactors, vcov = vcovHC(fixedEffectsLongRunProductionFactors, type = "HC3"))

# Panel data Granger causality

# Define the number of lags
lags <- 2

# Define the minimum required length
min_length <- 5 + 3 * lags



# Between estimators
between1 <- plm(formula = GROWTH ~ TEA,
                data = panel, 
                model = "between")
summary(between1)
# coeftest(between1, vcovHC(between1, type = "HC1"))
calculate_vif(between1)

between2 <- plm(formula = GROWTH ~ TEA + I(TEA^2), 
                data = panel, 
                model = "between")
summary(between2)
# coeftest(between2, vcovHC(between2, type = "HC3"))
calculate_vif(between2)

between3 <- plm(formula = GROWTH ~ TEA + INITIAL,
                data = panel, 
                model = "between")
summary(between3)
# coeftest(between3, vcovHC(between3, type = "HC3"))
calculate_vif(between3)

between4 <- plm(formula = GROWTH ~ TEA + I(TEA^2) + INITIAL, 
                data = panel, 
                model = "between")
summary(between4)
# coeftest(between4, vcovHC(between4, type = "HC3"))
calculate_vif(between4)

between5 <- plm(formula = GROWTH ~ TEA + INITIAL + I(INITIAL*TEA),
                data = panel, 
                model = "between")
summary(between5)
# coeftest(between5, vcovHC(between5, type = "HC3"))
calculate_vif(between5)

between6 <- plm(formula = GROWTH ~ TEA + INITIAL + 
                    CREDIT + RD + LOGPATENT + GCF + UNEMPLOYMENT + TRADE +
                    URBAN + POLSTAB + MOBILE + BROADBAND + TELEPHONE + FDI + 
                    GOVT + EDUC + INFLATION, 
                  data = panel, 
                  model = "between")
summary(between6)
# coeftest(between6, vcovHC(between6, type = "HC3"))
calculate_vif(between6)

between7 <- plm(formula = GROWTH ~ TEA + I(TEA^2) + INITIAL + 
                  CREDIT + RD + LOGPATENT + GCF + UNEMPLOYMENT + TRADE +
                  URBAN + POLSTAB + MOBILE + BROADBAND + TELEPHONE + FDI + 
                  GOVT + EDUC + INFLATION, 
                data = panel, 
                model = "between")
summary(between7)
# coeftest(between7, vcovHC(between7, type = "HC3"))
calculate_vif(between7)


# Conduct Granger causality test
pgrangertest(TEA ~ GROWTH, data = panel_filtered, order = 2L)
pgrangertest(GROWTH ~ TEA, data = panel_filtered, order = 2L)

granger1 <- plm(formula = GROWTH ~ I(lag(GROWTH, 1) + lag(GROWTH, 2) + lag(GROWTH, 3)) + I(lag(TEA, 1) + lag(TEA, 2) + lag(TEA, 3)), 
                data = panel, 
                index = c("Country", "Year"),
                model = "within")
granger2 <- plm(formula = TEA ~ I(lag(GROWTH, 1) + lag(GROWTH, 2) + lag(GROWTH, 3)) + I(lag(TEA, 1) + lag(TEA, 2) + lag(TEA, 3)),
                data = panel, 
                index = c("Country", "Year"),
                model = "within")

summary(granger1)
summary(granger2)


# Fixed effects but differently

data$Country <- as.factor(data$Country)
data$Year <- as.factor(data$Year)

fixedEffects1_lm <- lm(
  formula = GROWTH ~ TEA + I(TEA^2) + FDI + factor(Country) + factor(Year),
  data = data
)

summary(fixedEffects1_lm)

coeftest(fixedEffects1_lm, vcov = vcovHC(fixedEffects1_lm, type = "HC1", method = "arellano"))
