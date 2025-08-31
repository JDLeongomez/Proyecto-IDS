library(readr)
library(ganttrify)
library(ggplot2)

crono <- read_csv("crono.csv") |> 
  ganttrify(mark_quarters = TRUE,
            size_activity = 2,
            project_start_date = "2025-01",
            mark_years = TRUE,
            month_label_string = "",
            month_date_label = FALSE,
            size_text_relative = 1,
            label_wrap = 65,
            font_family = "Roboto Condensed")

ggsave("cronograma.png", crono, 
       width = 1200, height = 400, 
       units = "px", dpi = 300)
