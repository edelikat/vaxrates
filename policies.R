library(data.table)
library(car)
library(ggplot2)
library(broom)
library(dplyr)
library(forcats)
library(tidyr)
library(dotwhisker)
library(knitr)

# Pull in data
dt <- data.table::fread("./8-2-25Download.csv")


# 1. Create policy group
dt[, policy_group := fifelse(rel_exempt == 1 & nonmed_nonrel_exempt == 2, "Religious Only",
                             fifelse(rel_exempt == 1 & nonmed_nonrel_exempt == 1, "Both Allowed", NA_character_))]

# 2. Summarize outcomes by group
dt[!is.na(policy_group), .(
        mean_MMR = mean(perc_MMR, na.rm = TRUE),
        mean_exempt = mean(perc_exempt, na.rm = TRUE)
), by = policy_group]

dt[, policy_group := factor(policy_group)]

# Ensure factor levels are set
dt[, policy_group := factor(policy_group,
                            levels = c("Both Allowed", "Religious Only"))]

# Filter complete cases
policy_vars <- c("form", "in_personHD", "notarize", "edu")
dt_clean <- dt[complete.cases(dt[, ..policy_vars])]

# Fit adjusted models
mod_mmr_adj <- lm(perc_MMR ~ policy_group + form + in_personHD + notarize + edu, data = dt_clean)
mod_exempt_adj <- lm(perc_exempt ~ policy_group + form + in_personHD + notarize + edu, data = dt_clean)

# Type II ANOVA
Anova(mod_mmr_adj, type = "II")
Anova(mod_exempt_adj, type = "II")


# Get predicted values
pred_mmr <- broom::augment(mod_mmr_adj, newdata = dt_clean)
pred_exempt <- broom::augment(mod_exempt_adj, newdata = dt_clean)

# Add policy group for plotting
pred_mmr$policy_group <- dt_clean$policy_group
pred_exempt$policy_group <- dt_clean$policy_group

# Plot adjusted MMR
ggplot(pred_mmr, aes(x = policy_group, y = .fitted, fill = policy_group)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Adjusted MMR Rates by Policy Group",
             x = "Policy Group",
             y = "Predicted MMR (%)") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set2")

# Plot adjusted exemption
ggplot(pred_exempt, aes(x = policy_group, y = .fitted, fill = policy_group)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Adjusted Exemption Rates by Policy Group",
             x = "Policy Group",
             y = "Predicted Exemption Rate (%)") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set2")

library(dotwhisker)

# Combine models for comparison
dwplot(list(MMR = mod_mmr_adj, Exempt = mod_exempt_adj)) +
        theme_minimal() +
        labs(title = "Effect of Policy Group and Documentation on Outcomes",
             x = "Coefficient Estimate",
             y = "") +
        geom_vline(xintercept = 0, linetype = "dashed")

group_means <- dt[!is.na(policy_group), .(
        mean_MMR = mean(perc_MMR, na.rm = TRUE),
        mean_exempt = mean(perc_exempt, na.rm = TRUE)
), by = policy_group]

group_means_long <- melt(group_means, id.vars = "policy_group")

ggplot(group_means_long, aes(x = policy_group, y = value, fill = variable)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Unadjusted Group Means",
             x = "Policy Group",
             y = "Rate (%)") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set1")

library(broom)
library(dplyr)

# Tidy models
mmr_tidy <- tidy(mod_mmr_adj) %>% mutate(outcome = "MMR")
exempt_tidy <- tidy(mod_exempt_adj) %>% mutate(outcome = "Exempt")

# Combine and filter for documentation variables
doc_vars <- c("form", "in_personHD", "notarize", "edu", "edu_form")

doc_effects <- bind_rows(mmr_tidy, exempt_tidy) %>%
        filter(term %in% doc_vars) %>%
        select(outcome, term, estimate, p.value) %>%
        mutate(
                effect = round(estimate, 2),
                pval = format.pval(p.value, digits = 2, eps = 0.001)
        ) %>%
        select(outcome, term, effect, pval) %>%
        pivot_wider(names_from = outcome, values_from = c(effect, pval))


# Combine and filter for policy_group
coef_plot_dt <- bind_rows(mmr_tidy, exempt_tidy) %>%
        filter(term == "policy_groupReligious Only")

# Plot
ggplot(coef_plot_dt, aes(x = outcome, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
        geom_pointrange() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(x = "Outcome", y = "Adjusted Effect (Religious Only vs Both Allowed)",
             title = "Adjusted Effect of Policy Group on MMR and Exemption Rates") +
        theme_minimal()

kable(doc_effects, caption = "Adjusted Effects of Documentation Policies on MMR and Exemption Rates")

write.csv(doc_effects, "policy_effects_summary.csv", row.names = FALSE)


dt[!is.na(policy_group), .(
        mean_MMR = mean(perc_MMR, na.rm = TRUE),
        mean_exempt = mean(perc_exempt, na.rm = TRUE)
), by = .(policy_group, state)]

ggplot(dt[!is.na(policy_group)], aes(x = reorder(state, perc_MMR), y = perc_MMR, fill = policy_group)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        facet_wrap(~ policy_group) +
        labs(title = "MMR Rates by State and Policy Group", x = "State", y = "MMR Rate (%)") +
        theme_minimal()
