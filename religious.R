library(data.table)
library(car)
library(ggplot2)
library(broom)
library(dplyr)
library(forcats)
library(tidyr)


# Pull in data
dt <- data.table::fread("./8-2-25Download.csv")


mmod_rel <- lm(cbind(perc_MMR, perc_exempt) ~ perc_evangx + perc_mainx +
                             perc_cathx + perc_ldsx + perc_other +
                             perc_athag + perc_nothing_unaff,
                     data = dt)

mod_mmr <- lm(perc_MMR ~ perc_evangx + perc_mainx +
                            perc_cathx + perc_ldsx + perc_other +
                            perc_athag + perc_nothing_unaff,
                    data = dt)

mod_exempt <- lm(perc_exempt ~ perc_evangx + perc_mainx +
                               perc_cathx + perc_ldsx + perc_other +
                               perc_athag + perc_nothing_unaff,
                       data = dt)

library(car)
Anova(mmod_rel, type = "II")

summary(mod_mmr)
summary(mod_exempt)

mod_joint_adj <- lm(cbind(perc_MMR, perc_exempt) ~ perc_evangx + perc_mainx + perc_cathx + perc_ldsx + perc_other, perc_athag, perc_nothing_unaff, data = dt)
Anova(mod_joint_adj, type = "II")


# Convert rel_exempt to a factor for clarity
dt$rel_exempt_factor <- factor(dt$rel_exempt, levels = c(1, 2),
                               labels = c("Allows", "Does_Not_Allow"))

# Fit logistic regression model
mod_policy <- glm(rel_exempt_factor ~ perc_evangx + perc_mainx +
                               perc_cathx + perc_ldsx + perc_other +
                               perc_athag + perc_nothing_unaff,
                       data = dt,
                       family = binomial(link = "logit"))

# View summary
summary(mod_policy)



