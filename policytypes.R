library(data.table)
library(car)
library(ggplot2)
library(broom)
library(dplyr)
library(forcats)
library(tidyr)


# Pull in data
dt <- data.table::fread("./8-2-25Download.csv")

dt[, policy_group := fifelse(rel_exempt == 1 & nonmed_nonrel_exempt == 2, "Religious Only",
                             fifelse(rel_exempt == 1 & nonmed_nonrel_exempt == 1, "Both Allowed", NA_character_))]

policy_vars <- c("form", "in_personHD", "notarize", "edu", "edu_form")
dt_clean <- dt[complete.cases(dt[, ..policy_vars])]

mmod_policy <- lm(cbind(perc_MMR, perc_exempt) ~ form + in_personHD + notarize + edu + edu_form, data = dt_clean)
summary(mmod_policy)

mod_mmr_adj <- lm(perc_MMR ~ policy_group + form + in_personHD + notarize + edu + edu_form, data = dt_clean)
mod_exempt_adj <- lm(perc_exempt ~ policy_group + form + in_personHD + notarize + edu + edu_form, data = dt_clean)

library(car)
Anova(mmod_policy, type = "II")

summary(mod_mmr_adj)
summary(mod_exempt_adj)

mod_joint_adj <- lm(cbind(perc_MMR, perc_exempt) ~ policy_group + form + in_personHD + notarize + edu + edu_form, data = dt_clean)
Anova(mod_joint_adj, type = "II")



tidy_mmr <- tidy(lm(perc_MMR ~ form + in_personHD + notarize + edu + edu_form, data = dt_clean))
tidy_exempt <- tidy(lm(perc_exempt ~ form + in_personHD + notarize + edu + edu_form, data = dt_clean))

tidy_mmr$outcome <- "MMR Coverage"
tidy_exempt$outcome <- "Exemption Rate"

plot_data <- rbind(tidy_mmr, tidy_exempt)
plot_data <- plot_data[plot_data$term != "(Intercept)", ]

ggplot(plot_data, aes(x = term, y = estimate, fill = outcome)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                      position = position_dodge(width = 0.9), width = 0.2) +
        labs(x = "Policy Variable", y = "Effect Estimate", title = "Policy Effects on Coverage and Exemptions") +
        theme_minimal()

# Reshape data
long_policy <- dt_clean |>
        select(perc_MMR, perc_exempt, form, in_personHD, notarize, edu, edu_form) |>
        pivot_longer(cols = form:edu_form, names_to = "policy", values_to = "present") |>
        group_by(policy, present) |>
        summarise(
                avg_MMR = mean(perc_MMR, na.rm = TRUE),
                avg_exempt = mean(perc_exempt, na.rm = TRUE),
                .groups = "drop"
        )

# Plot
ggplot(long_policy, aes(x = policy, fill = factor(present))) +
        geom_bar(aes(y = avg_MMR), stat = "identity", position = "dodge") +
        geom_bar(aes(y = -avg_exempt), stat = "identity", position = "dodge") +
        labs(x = "Policy", y = "Average Rate",
             title = "MMR Coverage vs. Exemption Rates by Policy Presence",
             fill = "Policy Present") +
        scale_y_continuous(labels = abs, sec.axis = sec_axis(~-., name = "Exemption Rate")) +
        theme_minimal()

#SummaryTable

summary_table <- dt_clean |>
        select(perc_MMR, perc_exempt, form, in_personHD, notarize, edu, edu_form) |>
        pivot_longer(cols = form:edu_form, names_to = "policy", values_to = "present") |>
        group_by(policy, present) |>
        summarise(
                avg_MMR = round(mean(perc_MMR, na.rm = TRUE), 1),
                avg_exempt = round(mean(perc_exempt, na.rm = TRUE), 2),
                n_states = n(),
                .groups = "drop"
        )

write.csv(summary_table, "policy_summary.csv", row.names = FALSE, na = "")