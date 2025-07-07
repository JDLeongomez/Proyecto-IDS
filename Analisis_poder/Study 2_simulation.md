# Study 2

## Simulation Strategy

We conducted two power analyses for Study 2 using the TOSTER package. First, we estimated the sample size required to detect a positive correlation of $\rho$ = 0.25 using a one-sided test at $\alpha$ = 0.05 with 80% power. The required sample size was approximately $n$ = 97.

Second, we estimated the sample size needed to conclude statistical equivalence, using a TOST procedure with equivalence bounds of ±0.25 (i.e., testing whether the true effect is smaller than this threshold). With α = 0.05 and desired power = 0.90, the required sample size was approximately n = 169.


## CDI

Según Fenson et al. (2007) y datos de validación (p. ej., valididad concurrente), las puntuaciones de vocabulario expresivo y gramatical al rededor de los 36 meses tienden a distribuirse así:

Vocabulario expresivo (0‑100):

Media aproximada: ~50 – 60

Desviación típica: ~20 – 25 (siguiendo una distribución ligeramente sesgada a la derecha)

📊 2. Media y distribución típica
Según Fenson et al. (2007) y datos de validación (p. ej., valididad concurrente), las puntuaciones de vocabulario expresivo y gramatical al rededor de los 36 meses tienden a distribuirse así:

Vocabulario expresivo (0‑100):

Media aproximada: ~50 – 60

Desviación típica: ~20 – 25 (siguiendo una distribución ligeramente sesgada a la derecha)

Sección gramatical:

Aunque no hay una escala directa comparable a 100, estudios muestran una media cercana a 6.5 (DE ~3.7) en producción de oraciones complejas y 3.2 (DE ~1.7) en longitud de oraciones ―datos del CDI‑WS justo al paso a CDI‑III, útil como referencia https://doi.org/10.1177/0165025416644078, https://doi.org/10.1111/j.1467-8624.2005.00882.x

```{r}
CDI <- rtruncnorm(1000, a = 0, b = 100, mean = 55, sd = 25)
summary(CDI)

```

## CHIMUS

With both in hand, we can now simulate realistic raw CHIMUS scores for 3-year-olds, based on:

The 9-item final CHIMUS scale [from Table 2 in the paper and Supplement S2]

Age-specific descriptives from the 3–6-year-old subgroup

🧒 CHIMUS Items
Each item is scored on a 1–5 Likert scale, where:

Most items use either agreement ("totally disagree" to "totally agree") or frequency ("never" to "always").

Some items are reverse-coded (e.g., difficulty paying attention, reproducing melodies).

CHIMUS includes 3 subscales:

Enthusiasm & Motivation:

"often has the desire to make music"

"has great enthusiasm for music"

"enjoys making music as part of their life"

Music Perception:

"has a good sense of timing and rhythm"

"has a feeling for the beat"

"has good hearing ability, e.g., for melodies and rhythms"

Music Production (reverse-coded):

"shows difficulties in producing/reproducing music"

"pays little attention while making music"

"has issues reproducing melodies"

📊 Approximate Descriptives for Children Aged 3
From the paper:

Mean age for the 3–6 group: ~4.8 years (SD ≈ 1.0)

No exact breakdown by age 3, but younger children scored lower on Music Production and Perception, not necessarily on Motivation

Subscale α (reliability):

Motivation: ~.81

Perception: ~.84

Production: ~.78

✅ Recommended Simulation Values (per subscale)
Each subscale ranges from 3 to 15 (3 items × 1–5). For 3-year-olds:

Subscale	Mean	SD	Notes
Enthusiasm & Motivation	12.0	2.0	Likely high even in younger children
Music Perception	9.5	2.2	Developing, more variable
Music Production	9.0	2.4	Likely lower; reverse-coded items
Total Score (sum)	30.5	5.2	Out of 45

```{r}
n <- 1000  # Number of children

# Simulate subscale scores (bounded between 3 and 15)
motivation <- round(pmin(pmax(rnorm(n, mean = 12, sd = 2), 3), 15))
perception <- round(pmin(pmax(rnorm(n, mean = 9.5, sd = 2.2), 3), 15))
production <- round(pmin(pmax(rnorm(n, mean = 9, sd = 2.4), 3), 15))

# Total score
total <- motivation + perception + production

# Combine into data frame
chimus_data <- data.frame(
  age = 3,
  motivation,
  perception,
  production,
  total
)

# Preview
head(chimus_data)
```

## IDS acoustics

✅ Suggested Values for Simulating IDS (Mothers)
🎵 Mean pitch (f₀ mean) in IDS
Mean: ~280 Hz

SD: ~30–40 Hz

Range (min–max across speakers): 210–350 Hz

These values are based on:

Fernald & Kuhl (1987) found mean f₀ in English-speaking mothers’ IDS around 280 Hz.

Kitamura et al. (2001): Australian English-speaking mothers had average IDS pitch of ~270–290 Hz.

Hartman et al. (2022): IDS mean pitch ranged from 230 to 350 Hz depending on culture.

🔀 Pitch variability (f₀ SD) in IDS
Mean (SD of f₀ within utterances): ~50 Hz

SD: ~15 Hz

Range: 30–80 Hz

Examples:

Trainor et al. (2000) and Fernald (1991) report typical f₀ SDs in IDS between 40–60 Hz, often higher than in ADS.

Broesch & Bryant (2018) report ~43 Hz (SD ~21) in IDS.

### Correlation between mean f₀ and f₀ variability in Infant-Directed Speech (IDS)

1. Kitamura et al. (2001) – Australian mothers
Studied emotional tone in IDS.

Reported a moderate positive correlation between mean f₀ and f₀ range/variability (e.g., happier speech = higher pitch and more pitch modulation).

While exact coefficients weren’t reported, visual data suggest r ≈ .3–.5 in some conditions.

2. Fernald et al. (1989) – Cross-linguistic sample
Found that languages with higher mean f₀ in IDS also had greater pitch range and modulation.

Group-level correlations (across languages) appeared strong, but individual-level correlations were moderate at best, with substantial variability.

3. Broesch & Bryant (2018) – Fathers in small-scale societies
Reported higher mean f₀ accompanied by greater pitch range and variability, especially in rural IDS.

Though no direct correlation coefficient was provided, descriptives show that increases in f₀ mean and SD often co-occur.

4. Xu et al. (2013) – Prosodic functions
In adult emotional speech, which shares some features with IDS, f₀ mean and SD are positively but not perfectly correlated (r ~ .3–.6 depending on emotion).

This suggests a biologically grounded but flexible relationship, likely similar in IDS.

📈 Conclusion: Correlation Between f₀ Mean and SD in IDS
Direction: Positive

Magnitude: Small to moderate (typically r = 0.2–0.5)

Interpretation: Mothers who speak with higher pitch in IDS often (but not always) also show greater pitch variability.

```{r}
library(faux)

set.seed(123)
n <- 1000

# Step 2: Define means, SDs, and correlation matrix
mu <- c(CDI = 55, total = 30.5, f0_sd = 50, f0_mean = 280)
sd <- c(CDI = 25, total = 5.2, f0_sd = 15, f0_mean = 35)

# Define desired correlation matrix
rmat <- matrix(c(
  1.00,  0.40,  0.35,  0.30,   # CDI
  0.40,  1.00,  0.30,  0.25,   # total
  0.35,  0.30,  1.00,  0.40,   # f0_sd
  0.30,  0.25,  0.40,  1.00    # f0_mean
), nrow = 4, byrow = TRUE)

colnames(rmat) <- rownames(rmat) <- c("CDI", "total", "f0_sd", "f0_mean")

# Step 3: Simulate all four variables
sim_data <- rnorm_multi(
  n = n,
  mu = mu,
  sd = sd,
  r = rmat,
  varnames = c("CDI", "total", "f0_sd", "f0_mean"),
  empirical = TRUE  # use TRUE if you want *exact* correlations
)

# Optional: Check correlations
round(cor(sim_data), 2)


```



```{r}
sim_data <- chimus_data |> 
  mutate(CDI = CDI)
```

```{r}
set.seed(456)
n <- 1000

# Define mean and SD for each variable
mu <- c(CDI = 55, CHIMUS = 30.5, f0_sd = 50, f0_mean = 280)
sd <- c(CDI = 25, CHIMUS = 5.2, f0_sd = 15, f0_mean = 35)

# Define the target correlation matrix
R <- matrix(c(
  1.00,  0.00,  0.40,  0.30,
  0.00,  1.00,  0.30,  0.20,
  0.40,  0.30,  1.00,  0.20,
  0.30,  0.20,  0.20,  1.00
), nrow = 4, byrow = TRUE)

colnames(R) <- rownames(R) <- c("CDI", "CHIMUS", "f0_sd", "f0_mean")

# Compute covariance matrix
Sigma <- diag(sd) %*% R %*% diag(sd)

# Simulate multivariate normal data
vars <- MASS::mvrnorm(n, mu = mu, Sigma = Sigma)

# Convert to data frame
sim_data <- as.data.frame(vars)

# Round and bound as needed
sim_data <- sim_data |>
  mutate(
    CDI = round(pmin(pmax(CDI, 0), 100)),
    CHIMUS = round(pmin(pmax(CHIMUS, 15), 45)),  # CHIMUS range from 3*3 to 3*15
    f0_sd = f0_sd,
    f0_mean = f0_mean
  ) |>
  rowwise() |>
  mutate(subscales = list({
    s <- sample(3:(CHIMUS - 2), 1)
    p <- sample(3:(CHIMUS - s - 1), 1)
    m <- CHIMUS - s - p
    tibble(motivation = m, perception = p, production = s)
  })) |>
  ungroup() |>
  unnest_wider(subscales) |>
  mutate(Age = 3) |> 
  dplyr::select(Age, f0_sd, f0_mean, motivation, perception, production, CHIMUS, CDI)

# Final correlation check
round(cor(sim_data[c("CDI", "CHIMUS", "f0_sd", "f0_mean")]), 2)
```
