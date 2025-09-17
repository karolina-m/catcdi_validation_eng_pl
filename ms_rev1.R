## ----analysis-preferences------------------------------------------------------------------------------------------------------------------------
# Seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed, 
                      cache = TRUE, 
                      echo = FALSE)


## ----setup, include = FALSE----------------------------------------------------------------------------------------------------------------------
library(papaja)
library(wordbankr)
library(tidyverse)
library(ggrepel)
library(mirt) 
library(mirtCAT)
library(here)
library(kableExtra)
library(gridExtra)
library(printr)
library(ggpubr)

# KM: the only change I made here was changing the path of the files (for loading)
source(here("data/english/github-archive/scripts/IRT_helpers.R"))
source(here("data/english/github-archive/scripts/01_load_data.R"))


## ------------------------------------------------------------------------------------------------------------------------------------------------
library(wordbankr)
ws <- wordbankr::get_administration_data(language = "English (American)", form = "WS", include_demographic_info = T)
mod_en_ws <- lm(production ~ age * sex, data = ws) # R^2 = .78 

#load(here("data","production","wordbank_eng_ws_wg_webcdi31-36mos.Rds"))
#mod_en_ws <- lm(production ~ age * sex, data = d_demo_en %>% filter(age < 31, age >= 16)) # R^2 = .84 


## ----count-participants, include=F---------------------------------------------------------------------------------------------------------------
count_na <- function(vec) {
  return(length(which(is.na(vec))))
}

numna_en = apply(d_mat_en, 1, count_na) 
eng_ws_subjs = length(which(numna_en<285)) # 5573
eng_cdi_iii_subjs = length(which(d_demo_en$age>30))
#eng_wg_subjs = nrow(d_mat_en) - eng_ws_subjs - eng_cdi_iii_subjs # 1991 (lost some with missing data?)

numna_sp = apply(d_mat_sp, 1, count_na) 
sp_ws_subjs = length(which(numna_sp<222)) # 1146
#sp_cdi_iii_subjs = length(which(d_demo_sp$age>30)) # only 12 words overlap...
#sp_wg_subjs = nrow(d_mat_sp) - eng_ws_subjs - eng_cdi_iii_subjs


## ----plot-production, echo=F, warning=F, fig.width=6.5, fig.height=5.5, fig.cap="Children's CDI vocabulary plotted by age and sex in each dataset: (A) English comprehension, (B) Spanish comprehension, (C) English production, and (D) Spanish production. Note that we plot fitted quadratics showing extrapolated vocabulary sizes beyond the maximum CDI score, rather than an asymptotic logistic."----

en_prod <- d_demo_en %>% 
  mutate(sex = as.character(sex),
         sex = ifelse(sex=="Other", NA, sex)) %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = age, y = production, group = sex, color = sex)) + 
  geom_jitter(alpha = .2) + theme_bw() + 
  stat_smooth(method = "glm", formula = y ~ 0 + x + I(x^2), size = 1) +
  xlab("Age (months)") + ylab("Words Produced") + xlim(11, 37) + ylim(0, 700) +
  ggtitle("English Production")

sp_prod <- ggplot(d_demo_sp, aes(x = age, y = production, group = sex, color = sex)) + 
  geom_jitter(alpha = .3) + theme_bw() + 
  xlab("Age (months)") + ylab("Words Produced") + xlim(11, 32) + ylim(0, 700) +
  stat_smooth(method = "glm", formula = y ~ 0 + x + I(x^2), size = 1) +
  ggtitle("Spanish Production")

en_comp <- demo_eng_wg %>% filter(!is.na(sex)) %>%
  ggplot(aes(x = age, y = comprehension, group = sex, color = sex)) + 
  geom_jitter(alpha = .4) + theme_bw() + ylim(0, 430) +
  stat_smooth(method = "glm", formula = y ~ 0 + x + I(x^2), size = 1) +
  xlab("Age (months)") + ylab("Words Understood") + ggtitle("English Comprehension")

sp_comp <- ggplot(demo_sp_wg, aes(x = age, y = comprehension, group = sex, color = sex)) + 
  geom_jitter(alpha = .4) + theme_bw() + ylim(0, 430) +
  stat_smooth(method = "glm", formula = y ~ 0 + x + I(x^2), size = 1) +
  xlab("Age (months)") + ylab("Words Understood") + ggtitle("Spanish Comprehension")

ggarrange(en_comp, sp_comp,
          en_prod, sp_prod,
          labels = c("A", "B", "C", "D"), 
          ncol=2, nrow=2, common.legend = T)
#ggsave(here("figs","Fig1_vocab_vs_age_EN_SP_comp_prod.pdf"), width=6.5, height=5.5)


## ----form-overlap--------------------------------------------------------------------------------------------------------------------------------
sp_wg_ws_intersect = length(intersect(sp_wg_items$definition, sp_ws_items$definition))
# 388 / 428 match

sp_wg_not_ws = setdiff(sp_wg_items$definition, sp_ws_items$definition)
# 40 items -- but many of these match, yes? (just plural/parens/etc)

eng_wg_ws_intersect = length(intersect(eng_wg_items$definition, eng_ws_items$definition))
# 394 / 396 match
eng_wg_not_ws = setdiff(eng_wg_items$definition, eng_ws_items$definition)
# WG: "in" / "inside" -> a single WS item ("inside/in")


## ----pruning-eng-comp, echo=F--------------------------------------------------------------------------------------------------------------------
load(here("data/english/github-archive/data/comprehension/eng_wg_2pl_itemfits.Rds"))
load(here("data/english/github-archive/data/comprehension/LD_en.Rds"))
load(here("data/english/github-archive/data/comprehension/eng_wg_mod_2pl.Rds"))

bad_items2pl_x2 = which(itfit2pl_x2$p.X2_star_scaled < .001) # 18
bad_items_engC = subset(itfit2pl_x2, p.X2_star_scaled < .001)$item
hiLDvio = get_LD_violations(res, assoc_str=.5)
hiLDvio_words_engC = coefs_2pl[which(hiLDvio>0),]$definition 

# items showing strong LD and poor fit
bad_ld_fit = intersect(which(hiLDvio > 0), bad_items2pl_x2) # none

rm(res, itfit2pl, itfit2pl_x2)


## ----pruning-sp-comp, echo=F---------------------------------------------------------------------------------------------------------------------
load(here("data/english/github-archive/data/comprehension/sp_wg_2pl_itemfits.Rds"))
load(here("data/english/github-archive/data/comprehension/LD_sp.Rds"))
load(here("data/english/github-archive/data/comprehension/sp_wg_mod_2pl.Rds"))

bad_items2pl_x2 = which(itfit2pl_x2$p.X2_star_scaled < .001) # 20
bad_items_spC = subset(itfit2pl_x2, p.X2_star_scaled < .001)$item
hiLDvio = get_LD_violations(res, assoc_str=.5) # 0
hiLDvio_words_spC = coefs_2pl[which(hiLDvio>0),]$definition 

# items showing strong LD and poor fit
bad_ld_fit = intersect(which(hiLDvio > 0), bad_items2pl_x2) # none

rm(res, itfit2pl, itfit2pl_x2)


## ----pruning-eng-prod, echo=F--------------------------------------------------------------------------------------------------------------------
load(here("data/english/github-archive/data/production/eng_ws_wg_2pl_itemfits.Rds"))
load(here("data/english/github-archive/data/production/LD_eng.Rds"))
load(here("data/english/github-archive/data/production/eng_ws_wg_mod_2pl.Rds"))

#bad_items2pl = which(itfit2pl$p.S_X2 < .01) # 54 with p<.01 
bad_items2pl_x2 = which(itfit2pl_x2$p.X2_star_scaled < .001) # 142 with p<.001
bad_items_engP = subset(itfit2pl_x2, p.X2_star_scaled < .001)$item
med_rmsea = median(itfit2pl_x2$RMSEA.X2_star_scaled, na.rm=T) # .01
# no association = abs(V) < .1 no association, .3 is moderate, and .5+ is strong

hiLDvio = get_LD_violations(res, assoc_str=.5)
hiLDvio_words_en = coefs_2pl[which(hiLDvio>0),]$definition # daddy has a strong violation 

# items showing strong LD and poor fit
bad_ld_fit = intersect(which(hiLDvio > 0), bad_items2pl_x2)

d_mat_en = d_mat_en[,-bad_ld_fit]

rm(res, itfit2pl, itfit2pl_x2)


## ----pruning-sp-prod, echo=F---------------------------------------------------------------------------------------------------------------------
load(here("data/english/github-archive/data/production/sp_ws_wg_2pl_itemfits.Rds"))
load(here("data/english/github-archive/data/production/LD_sp.Rds"))
load(here("data/english/github-archive/data/production/sp_wg_ws_mod_2pl.Rds"))

bad_items2pl_x2 = which(itfit2pl_x2$p.X2_star_scaled < .001) # 38 with p<.001
bad_items_spP = subset(itfit2pl_x2, p.X2_star_scaled < .001)$item
hiLDvio = get_LD_violations(res, assoc_str=.5)
hiLDvio_words_sp = coefs_2pl[which(hiLDvio>0),]$definition 

#save(file="data/ill-fitting-items.Rdata", 
#     bad_items_spP, bad_items_engP, 
#     bad_items_spC, bad_items_engC)

# items showing strong LD and poor fit
bad_ld_fit = intersect(which(hiLDvio > 0), bad_items2pl_x2)

d_mat_sp = d_mat_sp[,-bad_ld_fit]
# check: intersect(c("no"), colnames(d_mat_sp))

rm(res, itfit2pl_x2)


## ----load-pruned-models, echo=F------------------------------------------------------------------------------------------------------------------
# final IRT models
prod_mod <- list()
comp_mod <- list()
# final IRT parameters
prod_pars <- list()
comp_pars <- list()
# final IRT fscores
prod_fs <- list()
comp_fs <- list()

# final pruned English production model
load(here("data/english/github-archive/data/production/eng_ws_wg_mod_2pl_nobad.Rds"))
prod_mod$en = mod_2pl
prod_pars$en = coefs_2pl
d_demo_en <- d_demo_en %>% left_join(fscores_2pl %>% 
                                       mutate(data_id = as.numeric(data_id)), by="data_id")
prod_fs$en = fscores_2pl
# final pruned Spanish production model
load(here("data/english/github-archive/data/production/sp_ws_wg_mod_2pl_nobad.Rds"))
prod_mod$sp = mod_2pl
prod_pars$sp = coefs_2pl
d_demo_sp <- d_demo_sp %>% left_join(fscores_2pl %>% 
                                       mutate(data_id = as.numeric(data_id)), by="data_id")
prod_fs$sp = fscores_2pl
# final unpruned English and Spanish comprehension models
load(here("data/english/github-archive/data/comprehension/eng_wg_mod_2pl.Rds"))
comp_mod$en = mod_2pl
comp_pars$en = coefs_2pl
d_demo_en <- d_demo_en %>% left_join(fscores_2pl %>% rename(comp_ability = ability) %>%
                                       mutate(data_id = as.numeric(data_id)), by="data_id")
comp_fs$en = fscores_2pl
demo_eng_wg <- demo_eng_wg %>% left_join(fscores_2pl %>% 
                                       mutate(data_id = as.numeric(data_id)), by="data_id")

load(here("data/english/github-archive/data/comprehension/sp_wg_mod_2pl.Rds"))
comp_mod$sp = mod_2pl
comp_pars$sp = coefs_2pl
d_demo_sp <- d_demo_sp %>% left_join(fscores_2pl %>% rename(comp_ability = ability) %>%
                                       mutate(data_id = as.numeric(data_id)), by="data_id")
comp_fs$sp = fscores_2pl
demo_sp_wg <- demo_sp_wg %>% left_join(fscores_2pl %>% 
                                       mutate(data_id = as.numeric(data_id)), by="data_id")

en_full_prod_vs_comp_theta = with(d_demo_en, 
                                  cor.test(ability, comp_ability, na.rm=T)) # .65
sp_full_prod_vs_comp_theta = with(d_demo_sp, 
                                  cor.test(ability, comp_ability, na.rm=T)) # .75

en_prod_vs_theta = with(d_demo_en, cor.test(production, ability)) # .93
sp_prod_vs_theta = with(d_demo_sp, cor.test(production, ability)) # .90

#cor(d_demo_en$age, d_demo_en$production) # .81
#cor(d_demo_en$age, d_demo_en$ability) # .81
# Age is correlated equally strongly with production sumscore as with ability.
#cor(d_demo_sp$age, d_demo_sp$production) # .64
#cor(d_demo_sp$age, d_demo_sp$ability) # .63


## ----impute-missing-data, echo=F, warning=F------------------------------------------------------------------------------------------------------
set.seed(123)

d_mat_en = imputeMissing(prod_mod$en, 
                         Theta=fscores(prod_mod$en)) 
d_mat_sp = imputeMissing(prod_mod$sp, 
                         Theta=fscores(prod_mod$sp))
# Imputing too much data can lead to very conservative results. Use with caution.


## ----theta-by-age, include=F, echo=F, fig.width=6, fig.height=3.8, fig.cap="Children's ability vs. age (A) and total CDI score vs. ability (B) for the English production dataset."----
# Overlaid histograms
#ggplot(d_demo_en, aes(x=ability, fill=age_group)) +
#    geom_histogram(binwidth=.5, alpha=.5, position="identity") + theme_classic() +
#  xlab("Ability (Theta) Estimates from 2PL") +
#  labs(fill="Age Group") + ylab("Number of Participants")

p1 <- ggplot(d_demo_en, aes(x=jitter(age), y=ability)) +
  geom_point(alpha=.05) + theme_classic() +
  xlab("Age (mos)") + ylab("Estimated Ability")

p2 <- ggplot(d_demo_en, aes(x=production, y=ability, color=age)) + 
  geom_point(alpha=.05) + theme_classic() +
  xlab("Total CDI Score") + ylab("Estimated Ability")

# Density plots with semi-transparent fill
#ggplot(d_demo, aes(x=ability, fill=age_group)) + geom_density(alpha=.3) + theme_classic()

ggarrange(p1, p2, ncol=2, labels = c("A", "B"))
#ggsave(here("figs","Fig2_ability_vs_age_n_vocab_EN_prod.pdf"), width=7.5, height=4.8)


## ----preferredCAT-tab-en-prod, echo=F------------------------------------------------------------------------------------------------------------
load(here("data/english/github-archive/data/production/preferredCAT_en.Rds"))

get_preferred_cat_table <- function(d_mat, fs) {
  cat_tab_cols = c("Scoring / Start Item", 
                    "Median Items",
                    "Mean Items", 
                    "r with full CDI", 
                    "Mean SE", 
                    "Reliability", 
                    "Unused Items")
  pref_tab = summarize_CAT(min25_max50_ML, d_mat, fs)
  pref_tab = rbind(pref_tab, summarize_CAT(min25_max50_MAP, d_mat, fs))
  pref_tab = rbind(pref_tab, summarize_CAT(min25_max50_ML_age, d_mat, fs))
  pref_tab = rbind(pref_tab, summarize_CAT(min25_max50_MAP_age, d_mat, fs))
  pref_tab = data.frame(pref_tab)
  pref_tab$cond = c("ML / MI", "MAP / MI", "ML / age-based", "MAP / age-based")
  names(pref_tab) = cat_tab_cols
  pref_tab$`Median Items` = NULL
  pref_tab$`Unused Items` = NULL
  return(pref_tab)
}

pref_tab <- get_preferred_cat_table(d_mat_en, prod_fs$en)

apa_table(pref_tab, digits=c(0, 1, 3, 3, 3), caption="English production CAT simulations with preferred settings.") 


## ------------------------------------------------------------------------------------------------------------------------------------------------
load(here("data/english/github-archive/data/production/preferredCAT_sp.Rds"))

pref_tab <- get_preferred_cat_table(d_mat_sp, prod_fs$sp)

apa_table(pref_tab, digits=c(0, 1, 3, 3, 3), caption="Spanish production CAT simulations with preferred settings.")


## ------------------------------------------------------------------------------------------------------------------------------------------------
load(here("data/english/github-archive/data/comprehension/preferredCAT_eng.Rds"))

pref_tab <- get_preferred_cat_table(en_mat_wg, comp_fs$en)

apa_table(pref_tab, digits=c(0, 1, 3, 3, 3), caption="English comprehension CAT simulations with preferred settings.")


## ----preferredCAT-tab-sp-comp, echo=F------------------------------------------------------------------------------------------------------------
load(here("data/english/github-archive/data/comprehension/preferredCAT_sp.Rds"))

pref_tab <- get_preferred_cat_table(sp_mat_wg, comp_fs$sp)

apa_table(pref_tab, digits=c(0, 1, 3, 3, 3), caption="Spanish comprehension CAT simulations with preferred settings.")


## ----validation-participants---------------------------------------------------------------------------------------------------------------------
# all data (without quality exclusions)
load(here("data/english/github-archive/data/validation/processed_data.Rdata"))
# kept_demo / dropped_demo
load(here("data/english/github-archive/data/validation/processed_exclusions_data.Rdata"))

mom_ed_tab <- table(demo$primary_education_cat) # what to report?
mom_ed_mean = mean(demo$primary_education_cat) # 15.85

hisp_tab <- table(demo$child_hispanic_latino_cat) # (22 Hispanic/Latino, 48 not reported)
eth_tab <- table(demo$child_ethnicity_cat) # 203 white, ..B=black? O=other? 

#exclusions criteria
excluded_demo_reason <- dropped_demo %>% 
  mutate(mismatch_sex = ((sex_cat == "M") & (sex_full == "Female")) | ((sex_cat == "F") & (sex_full == "Male")),
         mismatch_age = age_cat != age_full,
         mismatch_birthweight = birth_weight_lb_cat != birth_weight_confirmation_lb_cat |
           (birth_weight_lb_cat<5 | birth_weight_lb_cat>9),
         mismatch_zip = zip_code_cat != zip_code_full,
         primary_yob = primary_yob_cat != primary_yob_confirmation_cat,
         multilingual = (language_days_per_week_cat * language_hours_per_day_cat) >0) %>%
  select(subject_id, mismatch_sex, mismatch_age, mismatch_birthweight, mismatch_zip, primary_yob, multilingual) %>%
  mutate(num_true = rowSums(. == TRUE, na.rm = T))

one_reason <- excluded_demo_reason %>% filter(num_true == 1)
mult_reason <- excluded_demo_reason %>% filter(num_true != 1)

#time between completion of study A and completion of study B
demo <- demo %>% 
  mutate(time_diff_hours = difftime(last_modified_full, last_modified_cat, 
                                    units = "hours"),
         time_diff_days = difftime(last_modified_full, last_modified_cat, 
                                    units = "days"))


excluded_subjs <- unique(dropped_demo$subject_id)

resps <- resps %>% filter(!is.element(subject_id, excluded_subjs))
kept_demo <- demo %>% filter(!is.element(subject_id, excluded_subjs))

# "exclude participants who respond differently on 75% or more of the 25-50 CDI-CAT items, as compared to their responses on the same items on the CDI-WS"

per_subj <- resps %>% filter(response_cat!="no_test") %>%
  mutate(resp_agrees = response_full==response_cat,
         CATproduces = case_when(response_cat == "produces" ~ 1,
                              response_cat == "no_produces" ~ 0,
                              TRUE ~ NA_real_)) %>%
  group_by(subject_id) %>% 
  summarise(agreement = mean(resp_agrees), n=n(), CATpropyes = sum(CATproduces) / n) %>% 
  arrange(agreement)

bad_Ss = per_subj %>% filter(agreement <= .25) # no participants excluded 


## ----administration procedure--------------------------------------------------------------------------------------------------------------------
#administration order
full_first <- filter(kept_demo, order == "full_first")
cat_first <- filter(kept_demo, order == "cat_first")

#proportion administrations with gap
time_gap_one_day <- kept_demo %>% filter((time_diff_hours > 6 & time_diff_hours < 24) | 
                                      (time_diff_hours < -12 & time_diff_hours > -6))

time_gap_multi_day <- kept_demo %>% filter((time_diff_hours > 24) | 
                                        (time_diff_hours < -24))


## ----cat-procedure, echo=FALSE, fig.cap="Screenshots of the English production CDI-CAT user interface.", out.width = '65%'-----------------------
#knitr::include_graphics("CAT-procedure-figure.png")


## ----include=F-----------------------------------------------------------------------------------------------------------------------------------
full_w <- resps %>% arrange(definition) %>%
  select(-response_cat) %>%
  mutate(item = definition, 
         produces = case_when(response_full == "produces" ~ 1,
                              response_full == "no_produces" ~ 0,
                              TRUE ~ NA_real_)) %>% 
  pivot_wider(id_cols = "subject_id", 
              names_from = item, 
              values_from = produces) %>%
  arrange(subject_id)

cat_w <- resps %>% #filter(response_cat!="no_test") %>%
  arrange(definition) %>%
  select(-response_full) %>%
  mutate(item = definition, 
         produces = case_when(response_cat == "produces" ~ 1,
                              response_cat == "no_produces" ~ 0,
                              TRUE ~ NA_real_)) %>%
  pivot_wider(id_cols = "subject_id", 
              names_from = item, 
              values_from = produces,
              values_fill = NA) %>%
  arrange(subject_id)

prod_s <- full_w %>% mutate(production = Reduce("+",.[2:681])) %>%
  select(subject_id, production) %>% 
  left_join(demo %>% select(subject_id, sex_full, age_full, order), by="subject_id")

full_mat <- as.matrix(full_w %>% select(-subject_id))
cat_mat <- as.matrix(cat_w %>% select(-subject_id))

load(here("data/english/github-archive/data/production/eng_ws_wg_mod_2pl.Rds"))

full_thetas <- data.frame(fscores(mod_2pl, method="MAP", response.pattern = full_mat)[,c("F1","SE_F1")])
cat_thetas <- data.frame(fscores(mod_2pl, method="MAP", response.pattern = cat_mat)[,c("F1","SE_F1")])

prod_s$fullTheta <- full_thetas[,1]
prod_s$fullTheta_SE <- full_thetas[,2]
prod_s$catTheta <- cat_thetas[,1]
prod_s$catTheta_SE <- cat_thetas[,2]


## ----echo=F--------------------------------------------------------------------------------------------------------------------------------------
full_vs_cat_theta <- with(prod_s, cor(fullTheta, catTheta)) # .92
prod_vs_cat_theta <- with(prod_s, cor(production, catTheta)) # .86
full_theta_vs_sumscore <- with(prod_s, cor(production, fullTheta)) # .95


## ----echo=F--------------------------------------------------------------------------------------------------------------------------------------
# t-test on ability differences based on test order
prod_s <- prod_s %>% mutate(sq_err = (fullTheta - catTheta)^2,
                            full_cat_diff = fullTheta - catTheta)

sq_err_test <- t.test(subset(prod_s, order=="full_first")$sq_err, 
                      subset(prod_s, order=="cat_first")$sq_err)
# t = 0.46016, df = 185.84, p-value = 0.6459
full_cat_diff_test <- t.test(subset(prod_s, order=="full_first")$full_cat_diff, 
                             subset(prod_s, order=="cat_first")$full_cat_diff)

bad_thresh <- mean(prod_s$sq_err) + 1.5*sd(prod_s$sq_err) # mean=.55, med = .17
bad <- subset(prod_s, sq_err > bad_thresh) # 18
good <- subset(prod_s, sq_err <= bad_thresh) # 186
#t.test(prod_s$full_cat_diff) # CAT on average shows higher ability (mean=.52)
#t.test(good$full_cat_diff) # mean=.40 even without the 18 outliers showing the largest error

#mean(bad$full_cat_diff)
# table(bad$order) # 11 / 18 discrepant participants did the full CDI first


## ----cat-vs-full-cdi, echo=F, fig.width=4.5, fig.height=3.8, fig.cap="Children's estimated ability from the full CDI:WS vs. estimated ability from the CDI-CAT, by sex of child."----
prod_s %>% rename(Sex=sex_full) %>%
  ggplot(aes(x = fullTheta, y = catTheta, color=Sex)) + geom_point(alpha=.7) + 
    geom_errorbar(aes(ymin = catTheta-catTheta_SE, ymax = catTheta+catTheta_SE), alpha=.7) + 
    geom_errorbarh(aes(xmin = fullTheta-fullTheta_SE, xmax = fullTheta+fullTheta_SE), alpha=.7) +
  theme_classic() + xlab("Ability from full CDI") + ylab("Ability from CAT") +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  geom_smooth(method=lm)# + 
  #stat_cor(inherit.aes = F, aes(x=fullTheta, y=catTheta, 
  #             label = paste(..rr.label.., sep = "~`,`~")))
#ggsave(here("figs","Fig3_vocab_vs_age_EN_SP_comp_prod.pdf"), width=5, height=4)


## ----echo=F--------------------------------------------------------------------------------------------------------------------------------------
valid_tab <- prod_s %>% 
  mutate(age_group = cut(age_full, breaks=seq(12,36,3), right=F, include.lowest = T)) %>%
  group_by(age_group) %>%
  summarise(r = cor(fullTheta, catTheta), n=n()) #%>% kableExtra::kable(digits=2)

valid_tab_wide = rbind(round(valid_tab$r, 2), as.character(valid_tab$n))
colnames(valid_tab_wide) = as.character(valid_tab$age_group)
row.names(valid_tab_wide) = c('r CAT vs. full CDI', 'N')

apa_table(valid_tab_wide, caption="Validation study ability correlations (CDI-CAT vs. full CDI) by age group.") 


## ----render-appendix-----------------------------------------------------------------------------------------------------------------------------
#render_appendix("appendix.Rmd")


## ----create_r-references-------------------------------------------------------------------------------------------------------------------------
#r_refs(file = "references.bib")

