library(data.table)
library(car)
library(ggplot2)
library(broom)
library(dplyr)
library(forcats)


# Pull in data
dt <- data.table::fread("./8-2-25Download.csv")

#Center population so the intercept is more accurate
dt[, k_pop_centered := k_pop - mean(k_pop, na.rm = TRUE)]


# 1. Create policy group
dt[, policy_group := fifelse(rel_exempt == 1 & nonmed_nonrel_exempt == 2, "Religious Only",
                             fifelse(rel_exempt == 1 & nonmed_nonrel_exempt == 1, "Both Allowed", NA_character_))]

# 2. Summarize outcomes by group
dt[!is.na(policy_group), .(
        mean_MMR = mean(perc_MMR, na.rm = TRUE),
        mean_exempt = mean(perc_exempt, na.rm = TRUE)
), by = policy_group]

dt[, policy_group := factor(policy_group)]

# Fit multivariate multiple regression
mmod_center <- lm(cbind(perc_MMR, perc_exempt) ~ k_pop_centered + policy_group + nonmed_exempt_score, data = dt)

# View summary for each outcome
summary(mmod_center)

# Optional: Test overall model significance
Anova(mmod_center, type = "II")  # Multivariate test of predictors

# Summarize mean MMR and exemption rate by policy group
dt_summary <- dt[!is.na(policy_group), .(
        mean_MMR = mean(perc_MMR, na.rm = TRUE),
        mean_exempt = mean(perc_exempt, na.rm = TRUE)
), by = policy_group]

# Reshape to long format for plotting
dt_long <- melt(dt_summary, id.vars = "policy_group",
                variable.name = "Outcome", value.name = "Mean")

ggplot(dt_long, aes(x = policy_group, y = Mean, fill = Outcome)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Vaccination and Exemption Rates by Policy Group",
             x = "Policy Group", y = "Mean Percentage",
             fill = "Outcome") +
        theme_minimal() +
        scale_fill_manual(values = c("mean_MMR" = "#1f77b4", "mean_exempt" = "#ff7f0e"),
                          labels = c("MMR Coverage", "Exemption Rate"))

#Extract submodels and tidy the data
mmr_model <- lm(perc_MMR ~ policy_group, data = dt)
exempt_model <- lm(perc_exempt ~ policy_group, data = dt)

tidy_mmr <- tidy(mmr_model, conf.int = TRUE) %>% mutate(outcome = "MMR Coverage")
tidy_exempt <- tidy(exempt_model, conf.int = TRUE) %>% mutate(outcome = "Exemption Rate")

tidy_all <- bind_rows(tidy_mmr, tidy_exempt)

#reorder for clarity
tidy_all <- tidy_all %>%
        filter(term != "(Intercept)") %>%
        mutate(term = fct_reorder(term, estimate))

tidy_all <- tidy_all %>%
        filter(term != "(Intercept)") %>%
        mutate(term = fct_reorder(term, estimate),
               sig_label = case_when(
                       p.value < 0.001 ~ "***",
                       p.value < 0.01  ~ "**",
                       p.value < 0.05  ~ "*",
                       TRUE            ~ ""
               ),
               p_label = ifelse(p.value < 0.1, sprintf("p = %.3f", p.value), ""))

#Create the Coefficient Plot
ggplot(tidy_all, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = outcome)) +
        geom_pointrange(position = position_dodge(width = 0.6)) +
        geom_text(aes(label = p_label), 
                  position = position_dodge(width = 0.6), 
                  hjust = -0.5, size = 5, show.legend = FALSE) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(title = "Predictor Effects on MMR Coverage and Exemption Rate",
             x = "Predictor", y = "Estimated Effect", color = "Outcome") +
        theme_minimal(base_size = 13) +
        scale_color_manual(values = c("MMR Coverage" = "#1f77b4", "Exemption Rate" = "#ff7f0e")) +
        coord_flip()