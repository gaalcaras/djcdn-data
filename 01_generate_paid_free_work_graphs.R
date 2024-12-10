# Generate figures 1 & 2
# Copyright Gabriel Alcaras, 2024


library(tidyverse)
library(fuzzyjoin)
library(glue)
library(cowplot)
library(ggplot2)

# Loading data ----

top_contributors_perq <- read_csv(here::here("data", "top_contributors_perq.csv"))
contributors_dayjobs <- read_csv(here::here("data", "contributors_dayjobs.csv"))

# Useful homemade functions ----
source(here::here("utils.R"))

# Preparing data ----

# In contributors_dayjobs, each line represents a job for one person (2005-2020)
# (one person with several jobs would have several lines in that db).
contributors_dayjobs_q <- contributors_dayjobs %>%
  # From the start date of the job, we deduce the quarter when the person started the job.
  # For instance, if someone starts a job on Feb 1st, then the starting quarter for the job is Jan 1st.
  # This may cause some commits to be inaccurately linked to a job (commits from Jan-01 to Jan-31),
  # but starting dates for jobs may not be exact (the day the job started). In the end, this should
  # cause only minor differences in estimations because : (1) in the worst case, this would only apply
  # to commits in a 3 months window (2) contributors tend to stay in their jobs for long period of
  # times and (3) since the same process happens at the end of the job, this should compensate these
  # small differences in the aggregate.
  mutate(quarter = map_chr(dates, quarter_from_date)) %>%
  mutate(git = factor(case_when(
    git == 1 ~ "Rémunéré",
    git > 0  ~ "Partiellement rémunéré",
    git == 0 ~ "Gratuit",
    is.na(git) ~ "Inconnu",
    TRUE     ~ as.character(git)
  ), ordered = TRUE,
  levels = c("Rémunéré", "Partiellement rémunéré", "Gratuit", "Inconnu"))
  )

# We have start dates for jobs, but no end dates. Using fuzzy joins, we deduce the end date of
# a job using the start date of the next job.
contributors_dayjobs_dates <- contributors_dayjobs_q %>%
  fuzzy_left_join(contributors_dayjobs_q,
                  by = c("contributor_id" = "contributor_id",
                         "quarter" = "quarter"
                  ),
                  match_fun = list(`==`, `<`)
  ) %>%
  # Since we have no "upper" boundary for the date (x < ...), we have multiple
  # wrong matches after the first good match. For instance, if a contributor has 3 jobs,
  # each starting Jan 1st of 2005, 2006 and 2007, we will get the following after the
  # fuzzy join: Job1 (2005-2006) ; Job1 (2005-2007) ; Job2 (2006-2007).
  # Thankfully, jobs are listed in chronological order. So we know that when we get several
  # lines for the same starting quarter (quarter.x) with different ending quarter (quarter.y)
  # we can keep the first one as the good match. Let's clean up:
  distinct(contributor_id.x, quarter.x, .keep_all = TRUE) %>%
  select(contributor_id = contributor_id.x,
         start = quarter.x,
         end = quarter.y,
         position = position.x,
         git = git.x
  ) %>%
  # In case we don't know when a job starts or ends (this happens when we have no job
  # information), we just say the job starts in 2050 (this is a trick to avoid bad joins when
  # we merge this dataset with the next one)
  mutate(start = ifelse(is.na(start), "2050-01-01", start),
         end = ifelse(is.na(end), "2050-01-01", end))

# We now go to the top_contributors_perq database, listing the 80% top contributors
# for each quarter from 2005 to 2020. A contributor may appear in several lines if they
# make it several times in the top 80% of each quarter. For each line, we want to know if
# the contributor is being paid, using the da
top_contributors_perq_dayjobs <- top_contributors_perq %>%
  mutate(quarter = map_chr(quarter, quarter_from_q)) %>%
  fuzzy_left_join(contributors_dayjobs_dates,
                  by = c("contributor_id" = "contributor_id",
                         "quarter" = "start",
                         "quarter" = "end"
                  ),
                  match_fun = list(`==`, `>=`, `<`)
  ) %>%
  select(contributor_id = contributor_id.x,
         quarter, commit_nb,
         git) %>%
  mutate(quarter = glue('{year(quarter)} T{quarter(quarter)}'),
         commit_nb = as.integer(commit_nb),
         git = fct_na_value_to_level(git, "Inconnu")) %>%
  # We remove data after 2020 Q2 just to get a neat 15 years of contribution.
  filter(quarter != "2020 T2") %>%
  filter(quarter != "2020 T3") %>%
  filter(quarter != "2020 T4")

# Generating contributors plots ----

contributors_barplot <- top_contributors_perq_dayjobs %>%
  ggplot(aes(x = reorder(quarter, desc(quarter)))) +
  geom_bar() +
  labs(x = "",
       y = "Nombre de contributeurs")

contributors_propplot <- top_contributors_perq_dayjobs %>%
  ggplot(aes(x = reorder(quarter, desc(quarter)),
             fill = fct_rev(git),
  )) +
  geom_bar(position = "fill") +
  labs(y = "Proportion de travail rémunéré",
       fill = "Travail de contribution") +
  scale_y_log10(breaks = c(1, 1.5, 2, 3, 10),
                labels = c("0", "5%", "10%", "30%", "100%"))

draw_whole_plot(contributors_propplot, contributors_barplot)

ggsave(here::here("outputs", "contributors.pdf"),
       width = 8.3, height = 11.7, units = "in" # A4 width
)

# Generating commits plots ----

commits_barplot <- top_contributors_perq_dayjobs %>%
  ggplot(aes(x = reorder(quarter, desc(quarter)), y = commit_nb)) +
  geom_bar(stat = "identity") +
  labs(x = "",
       y = "Nombre de contributions")

commits_propplot <- top_contributors_perq_dayjobs %>%
  add_count(quarter) %>%
  ggplot(aes(x = reorder(quarter, desc(quarter)),
             y = commit_nb,
             fill = fct_rev(git))) +
  geom_bar(position = "fill", stat = "identity") +
  labs(y = "Proportion de contributions rémunérées",
       fill = "Travail de contribution") +
  scale_y_continuous(labels = scales::percent_format())

draw_whole_plot(commits_propplot, commits_barplot)

ggsave(here::here("outputs", "commits.pdf"),
       width = 8.3, height = 11.7, units = "in" # A4 width
)

