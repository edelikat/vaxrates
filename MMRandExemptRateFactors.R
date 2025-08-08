library(data.table)
library(car)
library(ggplot2)
library(broom)
library(dplyr)
library(forcats)


# Pull in data
dt <- data.table::fread("./8-2-25Download.csv")

## Compare states only allowing religious exemptions with those allowing all nonmedical exemptions
#filter out medical only states
dt_filtered <- dt %>%
        filter(rel_exempt == 1)

#Run linear regression
mod_exempttype <- lm(perc_MMR ~ nonmed_nonrel_exempt, data = dt_filtered)
summary(mod_exempttype)

## In states allowing non-medical exemptions, how does requiring a standardized state exemption form correlate to MMR rates and exemption rates
# Filter to states that allow religious exemptions
rel_states <- subset(dt, rel_exempt == 1)

# Summary statistics by form requirement
aggregate(cbind(perc_MMR, perc_exempt) ~ form, data = rel_states, FUN = mean)

# Optional: t-tests to compare means
t.test(perc_MMR ~ form, data = rel_states)
t.test(perc_exempt ~ form, data = rel_states)

# Correlation between form and each outcome
cor.test(rel_states$form, rel_states$perc_MMR)
cor.test(rel_states$form, rel_states$perc_exempt)

## In states allowing non-medical exemptions, how does requiring a health department contact correlate to MMR rates and exemption rates
# Filter to states that allow religious exemptions
rel_states <- subset(dt, rel_exempt == 1)

# Summary statistics by hd contact requirement
aggregate(cbind(perc_MMR, perc_exempt) ~ in_personHD, data = rel_states, FUN = mean)

# Optional: t-tests to compare means
t.test(perc_MMR ~ in_personHD, data = rel_states)
t.test(perc_exempt ~ in_personHD, data = rel_states)

# Correlation between form and each outcome
cor.test(rel_states$in_personHD, rel_states$perc_MMR)
cor.test(rel_states$in_personHD, rel_states$perc_exempt)


## In states allowing non-medical exemptions and requiring forms, how does requiring the form to be notarized correlate to MMR rates and exemption rates
# Filter to states that allow religious exemptions and require forms
rel_states <- subset(dt, rel_exempt == 1 & form == 1)

# Summary statistics by notarization requirement
aggregate(cbind(perc_MMR, perc_exempt) ~ notarize, data = rel_states, FUN = mean)

# Optional: t-tests to compare means
t.test(perc_MMR ~ notarize, data = rel_states)
t.test(perc_exempt ~ notarize, data = rel_states)

# Correlation between form and each outcome
cor.test(rel_states$notarize, rel_states$perc_MMR)
cor.test(rel_states$notarize, rel_states$perc_exempt)


## In states allowing non-medical exemptions, how does requiring proof of education about vaccines correlate to MMR rates and exemption rates
# Filter to states that allow religious exemptions
rel_states <- subset(dt, rel_exempt == 1)

# Summary statistics by form requirement
aggregate(cbind(perc_MMR, perc_exempt) ~ edu_full, data = rel_states, FUN = mean)

# Optional: t-tests to compare means
t.test(perc_MMR ~ edu_full, data = rel_states)
t.test(perc_exempt ~ edu_full, data = rel_states)

# Correlation between form and each outcome
cor.test(rel_states$edu_full, rel_states$perc_MMR)
cor.test(rel_states$edu_full, rel_states$perc_exempt)


## In states allowing non-medical exemptions, how does requiring yearly or bi-yearly renewal correlate to MMR rates and exemption rates
# Filter to states that allow religious exemptions
rel_states <- subset(dt, rel_exempt == 1)

# Summary statistics
aggregate(cbind(perc_MMR, perc_exempt) ~ yearly_renew, data = rel_states, FUN = mean)

# Optional: t-tests to compare means
t.test(perc_MMR ~ yearly_renew, data = rel_states)
t.test(perc_exempt ~ yearly_renew, data = rel_states)

# Correlation for each outcome
cor.test(rel_states$yearly_renew, rel_states$perc_MMR)
cor.test(rel_states$yearly_renew, rel_states$perc_exempt)


##Look at rates by state political leaning
# Convert PVI to a factor with descriptive labels
dt <- dt %>%
        mutate(PVI_Cat = case_when(
                PVI == 1 ~ "Marginally Democrat",
                PVI == 2 ~ "Moderately Democrat",
                PVI == 3 ~ "Strongly Democrat",
                PVI == 4 ~ "Marginally Republican",
                PVI == 5 ~ "Moderately Republican",
                PVI == 6 ~ "Strongly Republican",
                TRUE ~ NA_character_
        )) %>%
        mutate(PVI_Cat = factor(PVI_Cat,
                            levels = c("Marginally Democrat", "Moderately Democrat", "Strongly Democrat",
                                       "Marginally Republican", "Moderately Republican", "Strongly Republican")))

dt %>%
        group_by(PVI_Cat) %>%
        summarise(
                mean_MMR = mean(perc_MMR, na.rm = TRUE),
                sd_MMR = sd(perc_MMR, na.rm = TRUE),
                mean_Exempt = mean(perc_exempt, na.rm = TRUE),
                sd_Exempt = sd(perc_exempt, na.rm = TRUE),
                n = n()
        )

# Model MMR Rate ~ CPVI
lm_mmr <- lm(perc_MMR ~ PVI_Cat, data = dt)
summary(lm_mmr)

# Model Exemption Rate ~ CPVI
lm_exempt <- lm(perc_exempt ~ PVI_Cat, data = dt)
summary(lm_exempt)

dt <- dt %>%
        mutate(PVI_Group = case_when(
                PVI %in% 1:3 ~ "Democrat",
                PVI %in% 4:6 ~ "Republican",
                TRUE ~ NA_character_
        )) %>%
        mutate(PVI_Group = factor(PVI_Group))

lm_mmr_group <- lm(perc_MMR ~ PVI_Group, data = dt)
summary(lm_mmr_group)
lm_exempt_group <- lm(perc_exempt ~ PVI_Group, data = dt)
summary(lm_exempt_group)

##Look at MMR and exemption rates by religious affiliation

mmr_model_relgroups <- lm(perc_MMR ~ perc_evangx + perc_histblckx + perc_mainx +
                                  perc_cathx + perc_ldsx + perc_other + perc_athag + perc_nothing_unaff,
                          data = dt)

exempt_model_relgroups <- lm(perc_exempt ~ perc_evangx + perc_histblckx + perc_mainx +
                                     perc_cathx + perc_ldsx + perc_other + perc_athag + perc_nothing_unaff,
                             data = dt)

summary(mmr_model_relgroups)
summary(exempt_model_relgroups)