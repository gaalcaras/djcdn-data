# Generate figures 3 & 4
# Copyright Gabriel Alcaras, 2024

library(tidyverse)

library(dplyr)
library(stringr)
library(lubridate)
library(glue)
library(purrr)
library(fuzzyjoin)


library(cowplot)
library(ggplot2)
library(viridis)

source(here::here("utils.R"))

emails <- read_csv(here::here("data", "emails.csv"))

# Useful functions ----

# We want to get correct each email date to get the exact local time.
# workhours_map does that from a contributors_data tibble, that provides contributor_id
# and the main timezone for each contributor.
workhours_map <- function(contributors_data) {
  # Generate data for each contributor
  gen_data <- function(id) {
    # Keep only tz data from contributor
    contributor_data <- contributors_data %>%
      filter(contributor_id == id)
    
    message(glue("{id}: {contributor_data$tz}"))
    
    # We create a `date_tz` column that uses the provided tz for a given contributor
    emails %>%
      filter(contributor_id == id) %>%
      rowwise() %>%
      mutate(date_tz = force_tz(with_tz(date, contributor_data$tz))) %>%
      # We remove rows that failed to compute correct datetimes
      filter(!is.na(date_tz))
  }
  
  map_dfr(contributors_data$contributor_id, gen_data)
}

# We now want workhours data before Git becomes a contributor's day job and after that event.
# Getting the correct local time now has to account for the fact that when contributors get hired,
# they can move across the globe and then their timezone changes.
# workhours_split_map does that given a contributors_data tibble:
# - contributor_id -> hashes
# - tz_before -> timezone before Git as a dayjob
# - tz -> timezone when Git is a dayjob
# - split -> date when the switch happens
workhours_split_map <- function(contributors_data) {
  # Generate data for each contributor
  gen_data <- function(id) {
    # Keep only tz data from contributor
    contributor_data <- contributors_data %>%
      filter(contributor_id == id)
    
    message(glue("{id}: {contributor_data$tz}"))
    
    emails %>%
      filter(contributor_id == id) %>%
      mutate(quarter = map_chr(date, quarter_from_date)) %>%
      mutate(contributor_id = id,
             split = factor(case_when(
               quarter < contributor_data$split ~ "AVANT",
               quarter >= contributor_data$split ~ "APRÈS"),
               levels = c("AVANT", "APRÈS"))
      ) %>%
      rowwise() %>%
      mutate(date_tz = case_when(
        split == "AVANT" ~ force_tz(with_tz(date, contributor_data$tz_before)),
        split == "APRÈS" ~ force_tz(with_tz(date, contributor_data$tz))
      )) %>%
      filter(!is.na(date_tz))
  }
  
  map_dfr(contributors_data$contributor_id, gen_data)
}

# Generate better contributor labels for plots
generate_contributor_labels <- function(contributors, hashes, pseudos) {
  contributors %>%
    mutate(contributor_id = factor(contributor_id, levels = hashes)) %>%
    count(contributor_id) %>%
    add_column(pseudo = pseudos) %>%
    mutate(label = glue('{pseudo} (n = {n})'))
}
  
# Generate volunteers workhours plots ----

volunteers_hashes <- c(
  "83bc5689fe5491834192d02520e0b5d5d9ee1dae660f892a22e04bd61dcf2dab",
  "a71d1a3f1b524e5d4fe0d4c7d0150f8ad9528ea55a2cbfc0fd5f6af2a4f41bcc",
  "65cc4980233ab59c1451f2420cc141099b142455e3ddd8f34adebcd042cfbcdd",
  "db936b2503a8119f5ddfacb602809179a23a9b7c32f374aba1e3c61263175738",
  "971f8918a9eb1d26e47a1f97b171aec31534c94edb8356eb344a3d8bb7c4aeca",
  "9033018d236025f090410846724588ac300f75573f5207c2b771d0f9f6ac8427",
  "8c0493174ef5383de54d54fedb44a1d6a04034ff993c1d58f9a7abc8aefca187",
  "4ff79722db9d94a6d752f42cf21499317937c7ce54ca5a31e71d952482296d6d"
)

volunteers_timezones <- c(
  "Europe/Berlin",
  "Europe/Paris",
  "America/Los_Angeles",
  "CEST",
  "CEST",
  "Asia/Ho_Chi_Minh",
  "America/Los_Angeles",
  "UTC"
)

volunteers_fakenames <- c(
  "David",
  "Nicolas",
  "Ryan",
  "Andreas",
  "Marek",
  "Tran",
  "Steven",
  "Alexander"
)

volunteers <- workhours_map(tibble(
    contributor_id = volunteers_hashes,
    tz = volunteers_timezones
  )) %>%
  mutate(hour = hour(date_tz),
         day = wday(date_tz, label = TRUE, locale = "fr_FR.UTF-8",
                    abbr = FALSE, week_start = 1))

# Generate nicer labels for the contributor_id
volunteers_label <- generate_contributor_labels(volunteers, volunteers_hashes, volunteers_fakenames)

volunteers %>%
  # We want each line to be the number of emails sent in a day/hour
  # ie 24*7 rows for each contributor
  group_by(contributor_id, day, hour) %>%
  summarise(n_emails = n()) %>%
  ungroup() %>%
  # Add empty rows when necessary, because if someone has never sent an email
  # at 3am on a Sunday, then there is no row for it
  complete(contributor_id, day, hour, fill = list(n_emails = 0)) %>%
  # Scale each number of emails to be a number between 0 and 1
  group_by(contributor_id) %>%
  mutate(n_emails = scale(n_emails),
         contributor_id = factor(contributor_id,
                            levels = volunteers_label$contributor_id,
                            labels = volunteers_label$label,
                            ordered = TRUE
                            )) %>%
  # Let's generate the plot!
  ggplot(aes(x = hour, y = reorder(day, desc(day)), fill = n_emails)) +
  geom_tile() +
  facet_wrap(~ contributor_id, ncol = 2, nrow = 4) +
  theme_minimal() +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = "top",
    legend.title = element_text(margin = margin(-12, 0, 0, 0, "pt"))
  ) +
  scale_fill_viridis_c(breaks = c(-1, 3),
                       labels = c("-", "+")) +
  scale_y_discrete(labels = c("dim", "sam", "ven", "jeu", "mer", "mar", "lun")) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 23),
                     labels = c("0h", "6h", "12h", "18h", "23h")) +
  labs(x = "", y = "",
       fill = "Niveau d'activité")

ggsave(here::here("outputs", "volunteers_workhours.pdf"),
       height = 8.27, width = 11.75, units = "in", dpi=150 # A4 width
)

# Generate paid contributors workhours plot ----

paid_hashes <- c(
  "67cc65d80b96af9dc378ac65cf23024189a624f4a944a37d84207fbc972e5c78",
  "827bab46f20aa27738d7eb3e6c8c99fc19e8df2b71cffd86c8a611fc8d6af2f3",
  "4edf5724e844b1f515146cc3bc0afff81f1aa4d3574e5919a3780390d57ceace",
  "07ee4a71cfce20ee665339cd8e512eaa1364ec5b386007fdb9c4e4e55d21e524",
  "aadbc578b8f9d9c18b74f5f825caded346272bdf5ad1199a504407a814180ea1"
  )

paid <- workhours_split_map(tibble(
    contributor_id = paid_hashes,
    tz = c("America/Los_Angeles", "America/Chicago", "Europe/Berlin", "America/Los_Angeles", "America/Los_Angeles"),
    tz_before = c("America/Los_Angeles", "America/Chicago", "Europe/Berlin", "Europe/Berlin", "America/Los_Angeles"),
    split = c("2010-04-01", "2011-04-01", "2015-10-01", "2014-10-01", "2017-10-01")
  )) %>%
  mutate(hour = hour(date_tz),
         day = wday(date_tz, label = TRUE, locale = "fr_FR.UTF-8",
                           abbr = FALSE, week_start = 1))

paid_fakenames <- c(
  "Ryo",
  "Tom",
  "Matthias",
  "Martin",
  "Matthew"
)

# Generate nicer labels for the contributor_id
paid_label <- generate_contributor_labels(paid, paid_hashes, paid_fakenames)

paid %>%
  # We want each line to be the number of emails sent in a day/hour/split
  # ie 24*7*2 rows for each contributor
  group_by(contributor_id, split, day, hour) %>%
  summarise(n_emails = n()) %>%
  ungroup() %>%
  # Add empty rows when necessary, because if someone has never sent an email
  # at 3am on a Sunday, then there is no row for it
  complete(contributor_id, split, day, hour, fill = list(n_emails = 0)) %>%
  group_by(contributor_id, split) %>%
  # Scale each number of emails to be a number between 0 and 1
  mutate(n_emails = scale(n_emails),
         split = factor(split, levels = c("AVANT", "APRÈS"),
                        labels = c("Bénévole ou temps partiel", "Pleinement rémunéré"),
                        ordered = TRUE),
         contributor_id = factor(contributor_id,
                            levels = paid_label$contributor_id,
                            labels = paid_label$label,
                            ordered = TRUE
         )) %>%
  # Let's generate the plot!
  ggplot(aes(x = hour, y = reorder(day, desc(day)), fill = n_emails)) +
  geom_tile() +
  facet_grid(contributor_id ~ split) +
  theme_minimal() +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = "top",
    legend.title = element_text(margin = margin(-12, 0, 0, 0, "pt"))
  ) +
  scale_fill_viridis_c(breaks = c(-1, 4),
                       labels = c("-", "+")) +
  scale_y_discrete(labels = c("dim", "sam", "ven", "jeu", "mer", "mar", "lun")) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 23),
                     labels = c("0h", "6h", "12h", "18h", "23h")) +
  labs(x = "", y = "",
       fill = "Niveau d'activité")

ggsave(here::here("outputs", "paid_workhours.pdf"),
       height = 8.27, width = 11.75, units = "in", dpi=150 # A4 width
)
