library(ggplot2)
library(dplyr)
library(stargazer)
library(tidyr)
library(readr)
library(readxl)
library(zoo)
library(xtable)

# UNITED STATES #

# DOWNLOAD US DATA
data <- read_excel("~/. Stanford/1. ECON 101/Final Paper/data1.xlsx")
View(data)

# CLEAN DATA
colnames(data) <- c("date", "output_gap", "inflation", "ffr_shadow")
data <- na.omit(data)
data$date <- as.yearqtr(data$date, format = "%Yq%q")
data$output_gap <- as.numeric(data$output_gap)
data$inflation <- as.numeric(data$inflation)
data$ffr_shadow <- as.numeric(data$ffr_shadow)

# FILTER US DATA
data_1985_2019 <- data %>% filter(date >= as.yearqtr("1985 Q1") & date <= as.yearqtr("2019 Q4"))
data_1985_2007 <- data %>% filter(date >= as.yearqtr("1985 Q1") & date <= as.yearqtr("2007 Q4"))
data_2009_2019 <- data %>% filter(date >= as.yearqtr("2009 Q1") & date <= as.yearqtr("2019 Q4"))
data_1985_2023 <- data %>% filter(date >= as.yearqtr("1985 Q1") & date <= as.yearqtr("2023 Q4"))



# US REGRESSIONS
regression_1985_2019 <- lm(ffr_shadow ~ inflation + output_gap, data = data_1985_2019)
regression_1985_2007 <- lm(ffr_shadow ~ inflation + output_gap, data = data_1985_2007)
regression_2009_2019 <- lm(ffr_shadow ~ inflation + output_gap, data = data_2009_2019)
regression_1985_2023 <- lm(ffr_shadow ~ inflation + output_gap, data = data_1985_2023)


# LATEX
stargazer(regression_1985_2019, type = "latex", title = "Regression Results: 1985-2019")
stargazer(regression_1985_2007, type = "latex", title = "Regression Results: 1985-2007")
stargazer(regression_2009_2019, type = "latex", title = "Regression Results: 2009-2019")
stargazer(regression_1985_2023, type = "latex", title = "Regression Results: 1985-2023")


# R * value
calculate_R_star <- function(regression_model, inflation_var) {
  coefficients <- coef(regression_model)
  constant <- coefficients["(Intercept)"]
  inflation_coeff <- coefficients[inflation_var]
  R_star <- constant + ((inflation_coeff - 1) * 2)
  return(R_star)
}

R_star_1985_2019 <- calculate_R_star(regression_1985_2019, "inflation")
R_star_1985_2007 <- calculate_R_star(regression_1985_2007, "inflation")
R_star_2009_2019 <- calculate_R_star(regression_2009_2019, "inflation")
R_star_1985_2023 <- calculate_R_star(regression_1985_2023, "inflation")

R_star_summary <- data.frame(
  Period = c("1985-2019", "1985-2007", "2009-2019", "1985-2023"),
  R_star = c(R_star_1985_2019, R_star_1985_2007, R_star_2009_2019, R_star_1985_2023)
)

latex_table <- xtable(R_star_summary, caption = "Summary of R* Values", label = "tab:r_star_summary")

# Print the LaTeX code for the table
print(latex_table, type = "latex", include.rownames = FALSE, booktabs = TRUE)

stargazer(R_star_summary, type = "latex", summary = FALSE, title = "Summary of R* Values", label = "tab:r_star_summary", rownames = FALSE)

####

## US Inertial
rho = .85

data <- data %>%
  mutate(ffr_shadow_lag = lag(ffr_shadow, 1))

data <- data %>%
  mutate(
    inertia = rho * ffr_shadow_lag,
    adjusted_inflation = (1 - rho) * inflation,
    adjusted_output_gap = (1 - rho) * output_gap
  )

regression_us_inertial_manual <- lm(ffr_shadow ~ inertia + adjusted_inflation + adjusted_output_gap, data = data)

summary(regression_us_inertial_manual)

stargazer(regression_us_inertial_manual, type = "latex", summary = FALSE, title = "Regression Results: U.S. Inertial")

R_star_inertial <- calculate_R_star(regression_us_inertial_manual, "adjusted_inflation")
print(R_star_inertial)

R_star_summary_inertial <- data.frame(
  Period = c("1985 Q1 - 2023 Q4"),
  R_star = c(R_star_inertial)
)

stargazer(R_star_summary_inertial, type = "latex", summary = FALSE, title = "Summary of Domestic Inertial R* Value", label = "tab:r_star_summary_inertial", rownames = FALSE)



###############################


# INTERNATIONAL #
intl_data <- read_excel("~/. Stanford/1. ECON 101/Final Paper/International Data_Econ 101.xlsx")
View(intl_data)

intl_data$Date <- as.numeric(intl_data$Date)

## summary stats
summary(intl_data$OutputGap_UK)
summary(intl_data$OutputGap_EU)
summary(intl_data$ExchangeRate_UK)
summary(intl_data$ExchangeRate_EU)

intl_data <- na.omit(intl_data)

# separate out data
intl_data_1985_2019 <- intl_data %>% filter(Date >= 1985 & Date <= 2019)
intl_data_1985_2007 <- intl_data %>% filter(Date >= 1985 & Date <= 2007)
intl_data_2009_2019 <- intl_data %>% filter(Date >= 2009 & Date <= 2019)
intl_data_1985_2023 <- intl_data %>% filter(Date >= 1985 & Date <= 2023)


### UK ###

# run regressions
regression_uk_1985_2019 <- lm(InterestRate_UK ~ CPI_UK + OutputGap_UK, data = intl_data_1985_2019)
regression_uk_1985_2007 <- lm(InterestRate_UK ~ CPI_UK + OutputGap_UK, data = intl_data_1985_2007)
regression_uk_2009_2019 <- lm(InterestRate_UK ~ CPI_UK + OutputGap_UK, data = intl_data_2009_2019)
regression_uk_1985_2023 <- lm(InterestRate_UK ~ CPI_UK + OutputGap_UK, data = intl_data_1985_2023)

stargazer(regression_uk_1985_2023, type = "latex", summary = FALSE, title = "Regression Results: UK 1985-2023")

# calculate R*
uk_R_star_1985_2019 <- calculate_R_star(regression_uk_1985_2019, "CPI_UK")
uk_R_star_1985_2007 <- calculate_R_star(regression_uk_1985_2007, "CPI_UK")
uk_R_star_2009_2019 <- calculate_R_star(regression_uk_2009_2019, "CPI_UK")
uk_R_star_1985_2023 <- calculate_R_star(regression_uk_1985_2023, "CPI_UK")

R_star_summary_uk <- data.frame(
  Period = c("1985-2019", "1985-2007", "2009-2019", "1985-2023"),
  R_star = c(uk_R_star_1985_2019, uk_R_star_1985_2007, uk_R_star_2009_2019, uk_R_star_1985_2023)
)

latex_table_uk <- xtable(R_star_summary_uk, caption = "Summary of R* Values for UK", label = "tab:r_star_summary_uk")

# Print the LaTeX code for the table
print(latex_table_uk, type = "latex", include.rownames = FALSE, booktabs = TRUE)

stargazer(R_star_summary_uk, type = "latex", summary = FALSE, title = "Summary of R* Values for UK", label = "tab:r_star_summary_uk", rownames = FALSE)


### EU ###

# run regressions
regression_eu_1985_2019 <- lm(InterestRate_EU ~ CPI_EU + OutputGap_EU, data = intl_data_1985_2019)
regression_eu_1985_2007 <- lm(InterestRate_EU ~ CPI_EU + OutputGap_EU, data = intl_data_1985_2007)
regression_eu_2009_2019 <- lm(InterestRate_EU ~ CPI_EU + OutputGap_EU, data = intl_data_2009_2019)
regression_eu_1985_2023 <- lm(InterestRate_EU ~ CPI_EU + OutputGap_EU, data = intl_data_1985_2023)

stargazer(regression_eu_1985_2023, type = "latex", summary = FALSE, title = "Regression Results: EU 1985-2023")

# calculate R*
eu_R_star_1985_2019 <- calculate_R_star(regression_eu_1985_2019, "CPI_EU")
eu_R_star_1985_2007 <- calculate_R_star(regression_eu_1985_2007, "CPI_EU")
eu_R_star_2009_2019 <- calculate_R_star(regression_eu_2009_2019, "CPI_EU")
eu_R_star_1985_2023 <- calculate_R_star(regression_eu_1985_2023, "CPI_EU")

R_star_summary_eu <- data.frame(
  Period = c("1985-2019", "1985-2007", "2009-2019", "1985-2023"),
  R_star = c(eu_R_star_1985_2019, eu_R_star_1985_2007, eu_R_star_2009_2019, eu_R_star_1985_2023)
)

latex_table_eu <- xtable(R_star_summary_eu, caption = "Summary of R* Values for EU", label = "tab:r_star_summary_eu")

# Print the LaTeX code for the table
print(latex_table_eu, type = "latex", include.rownames = FALSE, booktabs = TRUE)

stargazer(R_star_summary_eu, type = "latex", summary = FALSE, title = "Summary of R* Values for EU", label = "tab:r_star_summary_eu", rownames = FALSE)


### JAPAN ###

# run regressions
regression_jpn_1985_2019 <- lm(InterestRate_JPN ~ CPI_JPN + OutputGap_JPN, data = intl_data_1985_2019)
regression_jpn_1985_2007 <- lm(InterestRate_JPN ~ CPI_JPN + OutputGap_JPN, data = intl_data_1985_2007)
regression_jpn_2009_2019 <- lm(InterestRate_JPN ~ CPI_JPN + OutputGap_JPN, data = intl_data_2009_2019)
regression_jpn_1985_2023 <- lm(InterestRate_JPN ~ CPI_JPN + OutputGap_JPN, data = intl_data_1985_2023)


# calculate R*
jpn_R_star_1985_2019 <- calculate_R_star(regression_jpn_1985_2019, "CPI_JPN")
jpn_R_star_1985_2007 <- calculate_R_star(regression_jpn_1985_2007, "CPI_JPN")
jpn_R_star_2009_2019 <- calculate_R_star(regression_jpn_2009_2019, "CPI_JPN")
jpn_R_star_1985_2023 <- calculate_R_star(regression_jpn_1985_2023, "CPI_JPN")

stargazer(regression_jpn_1985_2023, type = "latex", summary = FALSE, title = "Regression Results: Japan 1985-2023")


R_star_summary_jpn <- data.frame(
  Period = c("1985-2019", "1985-2007", "2009-2019", "1985-2023"),
  R_star = c(jpn_R_star_1985_2019, jpn_R_star_1985_2007, jpn_R_star_2009_2019, jpn_R_star_1985_2023)
)

latex_table_jpn <- xtable(R_star_summary_jpn, caption = "Summary of R* Values for Japan", label = "tab:r_star_summary_jpn")

# Print the LaTeX code for the table
print(latex_table_jpn, type = "latex", include.rownames = FALSE, booktabs = TRUE)

stargazer(R_star_summary_jpn, type = "latex", summary = FALSE, title = "Summary of R* Values for Japan", label = "tab:r_star_summary_jpn", rownames = FALSE)


## SUMMARY OF INTERNATIONAL R* VALUES
R_star_summary_2 <- data.frame(
  Location = c("UK", "EU", "Japan"),
  R_star = c(uk_R_star_1985_2023, eu_R_star_1985_2023, jpn_R_star_1985_2023)
)

print(R_star_summary_2)
stargazer(R_star_summary_2, type = "latex", summary = FALSE, title = "Summary of International R* Values", label = "tab:R_star_summary_2", rownames = FALSE)


## INTERNATIONAL REAL EXCHANGE RATE REGRESSIONS
intl_data <- read_excel("~/. Stanford/1. ECON 101/Final Paper/International Data_Econ 101.xlsx")
View(intl_data)

regression_uk_exchange <- lm(InterestRate_UK ~ CPI_UK + OutputGap_UK + ExchangeRate_UK, data = intl_data)
regression_eu_exchange <- lm(InterestRate_EU ~ CPI_EU + OutputGap_EU + ExchangeRate_EU, data = intl_data)
regression_jpn_exchange <- lm(InterestRate_JPN ~ CPI_JPN + OutputGap_JPN + ExchangeRate_JPN, data = intl_data)

stargazer(regression_uk_exchange,type = "latex")
stargazer(regression_eu_exchange,type = "latex")
stargazer(regression_jpn_exchange,type = "latex")


### INTERNATIONAL INERTIAL ###

rho = .85

# lag interest rates
intl_data <- intl_data %>%
  mutate(InterestRate_UK_lag = lag(InterestRate_UK, 1),
         InterestRate_EU_lag = lag(InterestRate_EU, 1),
         InterestRate_JPN_lag = lag(InterestRate_JPN, 1))

# compute rho * interest rates for inertia
intl_data <- intl_data %>%
  mutate(
    uk_inertia = rho * InterestRate_UK_lag,
    eu_inertia = rho * InterestRate_EU_lag,
    jpn_inertia = rho * InterestRate_JPN_lag)

# compute (1 - rho) * other variables
intl_data <- intl_data %>%
  mutate(
    adj_CPI_UK = (1 - rho) * CPI_UK,
    adj_CPI_EU = (1 - rho) * CPI_EU,
    adj_CPI_JPN = (1 - rho) * CPI_JPN,
    adj_OutputGap_UK = (1 - rho) * OutputGap_UK,
    adj_OutputGap_EU = (1 - rho) * OutputGap_EU,
    adj_OutputGap_JPN = (1 - rho) * OutputGap_JPN,
    adj_ExchangeRate_UK = (1 - rho) * ExchangeRate_UK,
    adj_ExchangeRate_EU = (1 - rho) * ExchangeRate_EU,
    adj_ExchangeRate_JPN = (1 - rho) * ExchangeRate_JPN
  )


regression_uk_inertial_manual <- lm(InterestRate_UK ~ uk_inertia + adj_CPI_UK + adj_OutputGap_UK, data = intl_data)
regression_eu_inertial_manual <- lm(InterestRate_EU ~ eu_inertia + adj_CPI_EU + adj_OutputGap_EU, data = intl_data)
regression_jpn_inertial_manual <- lm(InterestRate_JPN ~ jpn_inertia + adj_CPI_JPN + adj_OutputGap_JPN, data = intl_data)


summary(regression_uk_inertial_manual)
summary(regression_eu_inertial_manual)
summary(regression_jpn_inertial_manual)

stargazer(regression_uk_inertial_manual,type = "latex", summary = FALSE, title = "Regression Results: UK Inertial")
stargazer(regression_eu_inertial_manual,type = "latex", summary = FALSE, title = "Regression Results: EU Inertial")
stargazer(regression_jpn_inertial_manual,type = "latex", summary = FALSE, title = "Regression Results: JPN Inertial")

R_star_inertial_uk <- calculate_R_star(regression_uk_inertial_manual, "adj_CPI_UK")
R_star_inertial_eu <- calculate_R_star(regression_eu_inertial_manual, "adj_CPI_EU")
R_star_inertial_jpn <- calculate_R_star(regression_jpn_inertial_manual, "adj_CPI_JPN")

R_star_summary_inertial_2 <- data.frame(
  Location = c("UK", "EU", "Japan"),
  R_star = c(R_star_inertial_uk, R_star_inertial_eu, R_star_inertial_jpn)
)

print(R_star_summary_inertial_2)
stargazer(R_star_summary_inertial_2, type = "latex", summary = FALSE, title = "Summary of International Inertial R* Values", label = "tab:R_star_summary_inertial_2", rownames = FALSE)
