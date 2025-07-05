library(tidyverse)
library(lmerTest)
library(effectsize)
library(scales)
library(tidyquant)

dat_wide <- data.frame(ID = paste0("P", 1:1000),
                  Global = rnorm(1000, mean = 0.5, sd = 0.15)) |> 
  mutate(Global = Global * 5000,
         Boca_per = rnorm(1000, mean = 0.4, sd = 0.2),
         Boca_menos = Global * Boca_per,
         Cambio_per = rnorm(1000, mean = 0.1, sd = 0.2),
         Boca_mas = Boca_menos + (Cambio_per * Boca_menos)) |> 
  select(ID, Global, Boca_menos, Boca_mas) 

dat <- dat_wide|> 
  pivot_longer(cols = Boca_menos:Boca_mas, names_to = "Condicion", values_to = "Boca")

mod <- lmer(Boca ~ Condicion + (1|ID),
            data = dat)  

bla <- anova(mod)
bla$`F value`
bla$NumDF
bla$DenDF
bla$`Pr(>F)`

ggplot(dat, aes(x = Condicion, y = Boca)) +
  geom_boxplot()

hedges_g(dat_wide$Boca_mas, dat_wide$Boca_menos, paired = TRUE)


# Simulation function
run_simulation <- function(dat, n_sample, n_sim) {
  
  # Perform simulations
  results <- map_dfr(seq_len(n_sim), \(i) {
    
    # Sample n_sample unique IDs
    sampled_ids <- dat |>
      distinct(ID) |>
      slice_sample(n = n_sample) |>
      pull(ID)
    
    # Filter data for sampled IDs
    sampled_data <- dat |>
      filter(ID %in% sampled_ids)
    
    # Fit the linear mixed model
    mod <- lmer(Boca ~ Condicion + (1 | ID), data = sampled_data)
    
    # Extract ANOVA table
    anova_res <- anova(mod)
    
    # Pull stats for Condicion term (assuming it's the first fixed effect)
    tibble(
      sim = i,
      f_value = anova_res$`F value`[1],
      num_df = anova_res$NumDF[1],
      den_df = anova_res$DenDF[1],
      p_value = anova_res$`Pr(>F)`[1]
    )
  })
  
  return(results)
}

alpha_lev = 0.05

# Example usage:
results <- run_simulation(dat, n_sample = 100, n_sim = 1000) |> 
  mutate(signif = ifelse(p_value < alpha_lev, "Significativo", "No significativo"))

power <- results |> 
  filter(signif == "Significativo") |> 
  count()/dim(results)[1]

ggplot(results, aes(x = p_value, fill = signif)) +
  scale_fill_hue(direction = -1) +
  geom_histogram(
    bins = 100, breaks = seq(0, 1, 0.01),
    alpha = 0.8,
    color = "gray30"
  ) +
  labs(
    y = "Count", x = "p value", fill = "Significance",
    title = "Statistical power",
    subtitle = bquote("1 - " * beta * " = " * .(round(
      power$n,
      2
    )))
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_fill_tq() +
  theme_tq() +
  theme(
    legend.position = c(0.98, 0.98), # x, y in NPC (0–1)
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.8)),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  guides(fill = guide_legend(reverse = TRUE))

