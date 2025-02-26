library(ggplot2); library(stringr)
library(dplyr); library(tidyr); library(purrr)
library(stargazer)
library(tidyr)
library(readr)
library(zoo)
library(readxl)
library(lubridate)
library(tidyfit)

data <- read_excel("~/. Stanford/1. ECON 101/Final Paper/data1.xlsx")
colnames(data) <- c("date", "output_gap", "inflation", "ffr_shadow")
data$date <- as.yearqtr(data$date, format = "%Yq%q")
data$date <- as.Date(data$date)

View(data)

# create the rolling regression function
rolling_regression <- function(data, window_size) {
  results <- data.frame(date = as.Date(character()), 
                        intercept = numeric(), 
                        inflation = numeric(), 
                        output_gap = numeric(), 
                        stringsAsFactors = FALSE)
  for (i in seq_len(nrow(data) - window_size + 1)) {
    window_data <- data[i:(i + window_size - 1), ]
    model <- lm(ffr_shadow ~ inflation + output_gap, data = window_data)
    coefs <- coef(model)
    
    results <- rbind(results, data.frame(
      window_end_quarter = window_data$date[window_size],
      intercept = coefs[1],
      inflation = coefs[2],
      output_gap = coefs[3]
    ))
  }
  return(results)
}

# compute the rolling regression and export the file
window_size <- 50
rolling_regression <- rolling_regression(data, window_size)

rolling_r_star <- rolling_regression %>%
  mutate(r_star = intercept + ((inflation - 1) * 2))

rolling_r_star$window_end_quarter <- as.yearqtr(rolling_r_star$window_end_quarter, format = "%YQ%q")

View(rolling_r_star)

write.csv(rolling_r_star, "rolling_r_star_50.csv", row.names = FALSE)

# graph a series of R* over time
series <- ggplot(rolling_r_star, aes(x=window_end_quarter, y=r_star)) +
  geom_line() +
  scale_x_yearqtr(format = "%Y-Q%q") +
  labs(title="Series of R* Over Time",
       x="End Quarter of Rolling Regression Window",
       y="R*") +
  theme_minimal()

# save to jpeg file
ggsave("r_star_series.jpeg", plot = series, width = 10, height = 6, dpi = 300)

