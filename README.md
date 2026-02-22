# afrobarometer.cleaning <img src="man/figures/logo.png" align="right" height="139" alt="" />

> Clean and harmonise Afrobarometer survey data across rounds 5–9.

## Installation

```r
# Install from local source
devtools::install("path/to/afrobarometer.cleaning")

# Or via pak
pak::pak("path/to/afrobarometer.cleaning")
```

## What it does

| Problem in your script | Package solution |
|---|---|
| Each round uses different column names (`Q13A` vs `Q55A` vs `Q74A`) | `AB_VAR_MAP` maps concepts → raw names per round |
| Codebook values −1, 7, 8, 9, 98, 99 mean "Not Applicable / Missing" | `ab_replace_sentinels()` replaces them all with `NA` |
| 0–4 Likert items need scaling before composites | `ab_scale_item()` normalises to 0–1 |
| Education needs category labels | `ab_recode_education()` returns an ordered factor |
| Want binary 0/1 outcome variable | `ab_binarize()` thresholds any column |
| Cleaning five rounds separately and row-binding | `ab_merge_rounds()` does it all in one call |

---

## Quick start

### Clean and merge all five rounds

```r
library(afrobarometer.cleaning)

sources <- list(
  "5" = "data/tan_r5_data_july_2015.sav",
  "6" = "data/tan_r6_data_eng.sav",
  "7" = "data/tan_r7_data.sav",
  "8" = "data/afrobarometer_release-data_tan_r8_en_2021-06-10.sav",
  "9" = "data/TAN_R9.data_.final_.wtd_release.30May23.sav"
)

# Clean all rounds, binarize the outcome variable using each round's median
ab <- ab_merge_rounds(
  sources,
  binarize_vars = "local_gvt_performance"   # > median → 1, else 0
)
```

### Clean a single round

```r
ab5 <- ab_clean_round(
  data  = "data/tan_r5_data_july_2015.sav",
  round = 5,
  binarize_vars      = "local_gvt_performance",
  binarize_threshold = 2        # use explicit threshold instead of median
)
```

### Manually replace sentinels and binarize

```r
library(haven)
raw <- read_spss("tan_r5_data_july_2015.sav")

# Step 1 – Replace all standard "Not Applicable" codes with NA
clean <- ab_replace_sentinels(raw)

# Step 2 – Binarize a specific column (values > 2 become 1)
clean <- ab_binarize(clean, cols = "Q66A", threshold = 2)
```

### Add round-specific extra variables

```r
ab <- ab_merge_rounds(
  sources,
  extra_vars = c(political_trust = "Q44A")  # same raw name every round
)

# Or per-round if column names differ:
ab <- ab_merge_rounds(
  sources,
  extra_vars = list(
    "5" = c(political_trust = "Q44A"),
    "6" = c(political_trust = "Q44A"),
    "7" = c(political_trust = "Q44A"),
    "8" = c(political_trust = "Q44A"),
    "9" = c(political_trust = "Q44A")
  )
)
```

### Plot media trends

```r
library(ggplot2)

trends <- ab_media_trends(ab)

ggplot(trends, aes(x = round, y = percent, color = source)) +
  geom_line() +
  ylim(0, 100) +
  labs(title = "Media consumption across rounds",
       x = "Round", y = "% weekly usage", color = "Source") +
  theme_bw()
```

---

## Key exported objects

| Object | Type | Description |
|---|---|---|
| `AB_SENTINEL_VALUES` | numeric vector | Codebook NA codes: `c(-1, 7, 8, 9, 98, 99)` |
| `AB_VAR_MAP` | named list | Concept → raw column name per round (5–9) |
| `AB_EDUCATION_LEVELS` | character vector | Ordered education level labels |

## Output columns

After `ab_clean_round()` / `ab_merge_rounds()` every row has:

**Identifiers:** `respondent`, `round`, `region`, `rural`, `education`

**News media (0–1 scale):** `news_radio`, `news_tv`, `news_newspaper`, `news_internet`, `news_sm`, `news_weekly`, `internet_news_weekly`

**Participation:** `discuss_politics`, `raise_issue`, `demonstrated`, `voted`, `attend_rally`, `persuade_others`, `worked_campaign`

**Infrastructure (binary):** `electricity`, `water`, `sewage`, `phone_service`, `school`, `police_station`, `clinic`, `police`, `soldiers`, `roadblocks`, `paved_road`

**Contact with leaders:** `contact_local_gvt`, `contact_mp`, `contact_official`, `contact_traditional`

**Bribes:** `bribe_for_permit`, `bribe_for_clinic`, `bribe_for_police`, `bribe_for_school`

**Difficulty accessing services:** `difficulty_id`, `difficulty_help_police`, `dififculty_schooling`, `difficulty_treatment`

**Composites (row-wise means):** `services`, `facilities`, `security`, `contact`, `bribes`, `difficulty_services`, `civic_participation`

**Performance:** `local_gvt_performance`, `often_use_internet`

---

## Extending the variable map

If you need to add or override a variable mapping:

```r
my_map <- AB_VAR_MAP
my_map$new_variable <- data.frame(
  round   = c(5,      6,      7,      8,      9),
  raw_var = c("Q99A", "Q99A", "Q99B", "Q99B", "Q99C")
)

ab <- ab_merge_rounds(sources, var_map = my_map)
```

---

## Running tests

```r
devtools::test()
```
