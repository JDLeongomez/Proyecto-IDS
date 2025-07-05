run_power_sim <- function(dat, n_sample, n_sim = 500, alpha = 0.05) {
  # Run `n_sim` simulations for each sample size
  map_dfr(seq_len(n_sim), \(i) {
    # Sample participants
    ids <- dat |> distinct(ID) |> slice_sample(n = n_sample) |> pull(ID)
    sampled_data <- dat |> filter(ID %in% ids)
    
    # Fit model
    mod <- lmer(fixation_time ~ f0_sd * f0_mean * Df + (1 | ID), data = sampled_data)
    
    # Extract ANOVA results
    anova_res <- anova(mod)
    
    # Grab all terms of interest
    term_names <- rownames(anova_res)
    
    # Loop over all fixed terms
    map_dfr(term_names, \(term) {
      tibble(
        sim = i,
        term = term,
        n_sample = n_sample,
        p_value = anova_res[term, "Pr(>F)"]
      )
    })
  }) |>
    # Clean up and aggregate
    filter(!is.na(p_value))
}

sample_sizes <- seq(20, 160, by = 20)

library(furrr)
plan(multisession)  # Parallel if available

power_results <- map_dfr(
  sample_sizes,
  ~ run_power_sim(simulated_data, n_sample = .x, n_sim = 1000),
  .id = "sample_step"
)

library(scales)

term_order <- c(
  "f0_sd",
  "f0_mean",
  "Df",
  "f0_sd:f0_mean",
  "f0_sd:Df",
  "f0_mean:Df",
  "f0_sd:f0_mean:Df"
)

power_res <- power_results |>
  group_by(term, n_sample) |>
  summarise(
    power = mean(p_value < alpha_lev, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(term = factor(term, levels = term_order))
  

library(stringr)
library(ggpubr)

main_power_res <- power_res |> 
  filter(!str_detect(term, ":"))
int_power_res <- power_res |> 
  filter(str_detect(term, ":"))

term_labels <- c(
  "f0_sd"              = "italic(f)[0]*' SD'",
  "f0_mean"            = "italic(f)[0]*' mean'",
  "Df"                 = "italic(D)[f]",
  "f0_sd:f0_mean"      = "italic(f)[0]*' SD × '*italic(f)[0]*' mean'",
  "f0_sd:Df"           = "italic(f)[0]*' SD × '*italic(D)[f]",
  "f0_mean:Df"         = "italic(f)[0]*' mean × '*italic(D)[f]",
  "f0_sd:f0_mean:Df"   = "italic(f)[0]*' SD × '*italic(f)[0]*' mean × '*italic(D)[f]"
)

ggarrange(ggplot(main_power_res, aes(x = n_sample, y = power)) +
            geom_line(linewidth = 0.5) +
            geom_point(aes(color = power > 0.8), size = 2) +  # Color by condition
            geom_hline(yintercept = 0.8, linetype = "dashed", color = "darkred") +
            scale_color_manual(
              values = c("TRUE" = "darkred", "FALSE" = "gray40"),
              labels = c("FALSE" = "≤ 80%", "TRUE" = "> 80%"),
              name = "Power threshold"
            ) +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            labs(
              title = "Power Curves for Main Effects",
              x = "Number of Participants",
              y = "Estimated Power"
            ) +
            theme_tq() +
            facet_wrap(~term, labeller = as_labeller(term_labels, label_parsed)) +
            theme(legend.position = "bottom"),
          ggplot(int_power_res, aes(x = n_sample, y = power)) +
            geom_line(linewidth = 0.5) +
            geom_point(aes(color = power > 0.8), size = 2) +  # Color by condition
            geom_hline(yintercept = 0.8, linetype = "dashed", color = "darkred") +
            scale_color_manual(
              values = c("TRUE" = "darkred", "FALSE" = "gray40"),
              labels = c("FALSE" = "≤ 80%", "TRUE" = "> 80%"),
              name = "Power threshold"
            ) +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            labs(
              title = "Power Curves for Interactions",
              x = "Number of Participants",
              y = "Estimated Power"
            ) +
            theme_tq() +
            facet_wrap(~term, labeller = as_labeller(term_labels, label_parsed)) +
            theme(legend.position = "bottom"),
          labels = "AUTO",
          common.legend = TRUE,
          legend = "bottom"
)
