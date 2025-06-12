library(here)

## AMERICAN ENGLISH

# Items definitions etc
# source(here("scripts/01_load_data.R"))
# write_csv(eng_ws_items, "eng_ws_items.csv")
eng_ws_items <- read_csv("data/english/eng_ws_items.csv")

# IRT 2PL English production (items common to WG, WS and CDI-III)
# load(here("data/production/eng_ws_wg_mod_2pl.Rds"))
# view(coefs_2pl)
# write_csv(coefs_2pl, "eng_coefs_2pl.csv")
coefs_2pl <- read_csv("data/english/eng_coefs_2pl.csv")


# Validation data
# load(here("data/validation/processed_data.Rdata"))
# write_csv(resps, "eng_resps.csv")
resps <- read_csv("data/english/eng_resps.csv")
# kept_demo / dropped_demo
# load(here("data/validation/processed_exclusions_data.Rdata"))
# view(resps)

# Explore items difficulty and discrimination
# add CDI category to items (wg + ws)
coefs_2pl <- merge(coefs_2pl, eng_ws_items[,c("definition", "uni_lemma", "category")], by = "definition", all.x=T)
items_cat_en <- coefs_2pl
rm(coefs_2pl)
rm(eng_ws_items)

# all data (without quality exclusions)
load(here("data/english/validation/processed_data.Rdata"))
# kept_demo / dropped_demo
load(here("data/english/validation/processed_exclusions_data.Rdata"))

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

load(here("data/english/production/eng_ws_wg_mod_2pl.Rds"))

full_thetas <- data.frame(fscores(mod_2pl, method="MAP", response.pattern = full_mat)[,c("F1","SE_F1")])
cat_thetas <- data.frame(fscores(mod_2pl, method="MAP", response.pattern = cat_mat)[,c("F1","SE_F1")])

prod_s$fullTheta <- full_thetas[,1]
prod_s$fullTheta_SE <- full_thetas[,2]
prod_s$catTheta <- cat_thetas[,1]
prod_s$catTheta_SE <- cat_thetas[,2]

#t.test(prod_s$full_cat_diff) # CAT on average shows higher ability (mean=.52)
#t.test(good$full_cat_diff) # mean=.40 even without the 18 outliers showing the largest error


