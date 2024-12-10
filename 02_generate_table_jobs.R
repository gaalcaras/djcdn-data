# Generate table 1
# Copyright Gabriel Alcaras, 2024

library(tidyverse)
library(magrittr)
library(janitor)

# Loading data ----

contributors_dayjobs <- read_csv(here::here("data", "contributors_dayjobs.csv"))

# Recoding data ----

contributors_dayjobs_r <- contributors_dayjobs %>%
  filter(!is.na(position)) %>%
  mutate(position_simple = case_when(
    str_detect(position, "(Engineer|Developer|System Administrator|Executives|Other)") ~ "Industry",
    str_detect(position, "(Universitaire|Étudiant)") ~ "Université",
    str_detect(position, "GSoC") ~ "GSoC, Outreachy"
  ))

# Generate tabyl ----

# High-level position
contributors_dayjobs_r %>%
  tabyl(position_simple) %>%
  adorn_pct_formatting(digits = 1) %>%
  arrange(desc(n))

# Detailed position
contributors_dayjobs_r %>%
  tabyl(position) %>%
  adorn_pct_formatting(digits = 1) %>%
  arrange(desc(n)) %>%
  print(n = Inf)
