library(glue)
library(lubridate)

# Useful functions for processing quarters ----

# Format year-quarter as YYYY-MM-DD
# e.g fmt_yq(1, 2020) returns "2020-01-01"
fmt_yq <- function(quarter, year) {
  if (quarter == 1) {
    result <- glue("{year}-01-01")
  } else if (quarter == 2) {
    result <- glue("{year}-04-01")
  } else if (quarter == 3) {
    result <- glue("{year}-07-01")
  } else {
    result <- glue("{year}-10-01")
  }
  
  return(result)
}

# Get formatted year-quarter from "YYYY Qn"
# eg. quarter_from_q("1990 Q2") returns "1990-04-01"
quarter_from_q <- function(x) {
  if (is.na(x)) {
    return(NA_character_)
  }
  
  q <- str_sub(x, -1)
  year <- str_sub(x, 1, 4)
  
  return(fmt_yq(q, year))
}

# Get formatted year-quarter from any date
# eg. quarter_from_date("2014-02-05") returns "2014-01-01"
quarter_from_date <- function(x) {
  if (is.na(x)) {
    return(NA_character_)
  }
  
  x <- as_date(x)
  q <- quarter(x)
  year <- year(x)
  
  return(fmt_yq(q, year))
}

# Useful ggplot2 functions ----

# Apply consistent styling to each barplot
format_barplot <- function(plot) {
  plot + 
    coord_flip() +
    theme_minimal() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_fill_viridis_c()
}

# Apply consistent styling to each proportion plot
format_propplot <- function(plot) {
  plot + 
    coord_flip() +
    # Remove the legend for now, so that the two plots align nicely later
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_line(colour = "gray90"),
          axis.ticks.x = element_line(colour = "gray90"),
          plot.margin = unit(c(0, -0.2, 0, 0), "cm")
    ) +
    # The two following lines matter to get proper color order for the fill legend
    scale_fill_viridis_d(direction = -1) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_discrete(position = "top")
}

# Draw the whole plot by combining the two base plots
draw_whole_plot <- function(proportion_plot, barplot) {
  # Extract legend
  legend <- get_legend(
    proportion_plot +
      theme(legend.position = "top") +
      # Important: apply same formatting of format_propplot
      scale_fill_viridis_d(direction = -1) +
      guides(fill = guide_legend(reverse = TRUE))
  )
  
  plot_grid(legend,
            plot_grid(format_propplot(proportion_plot), format_barplot(barplot), rel_widths = c(1, 1)),
            ncol = 1,
            rel_heights = c(0.05, 1))
}