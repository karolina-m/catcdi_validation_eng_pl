library(tidyverse)
library(Multilada)
library(ggpubr)
library(ggrepel)
library(gridExtra)
library(mirt)
library(mirtCAT)
library(viridis)

# ----- by-child data -------

## --- WS full ----
# get full cdi data
# cdi_read("cdi", "ws_pl") -> ws_responses
ws_responses <- read_csv2("data/polish/form_ws_pl_202412022243.csv")

# cut off early test submissions:
ws_responses %>% filter(end_date > "2021-07-07") -> ws_responses

# calculate sums:
cdi_count_oneCheckboxGroup(ws_responses, "word") %>%
     select(! type) %>% rename(score_full = n) -> ws_scores

# merge with submissions data (e.g. to get submitter's info to compare later to CAT)
ws_submissions <- cdi_submissions(ws_responses)
str(ws_submissions)
# TODO: śmieci w sex, guardian, guardian.other --> update multilada library?
# For the time being and for the sake of simplicity we get the demographic data (age, sex, person filling in, i.e., submitter) from the adaptive submissions only. This data was provided by the submitter in both full-item and adaptive CDIs. There might be discrepancies between the two sources and  we may want to look at them at some point.

ws_scores <- merge(ws_submissions, ws_scores, by = c("id", "end_date"))
ws_scores <- ws_scores %>% rename(submitter_full = guardian, submitter_full_other = guardian.other) # rename guardian to submitter_full

## --- WS cat ----
# cdi_read("cdi", "ws-cat_pl_adaptive") -> ws_cat_responses
ws_cat_responses <- read_csv2("data/polish/_form_ws_cat_pl_adaptive__202412022301.csv")

# fix a few IDs
ws_cat_responses$idx[ws_cat_responses$idx == "cIhfxbX2KKMJk7LeObAzUkm"] <- "cIhfxbX2KKMJk7LeObAzU" # removing "km" added to the id
ws_cat_responses$idx[ws_cat_responses$idx == "L3p3bjQACfa7DJwkqJTmxxyz"] <- "L3p3bjQACfa7DJwkqJTmx" # removing "xyz" added to the id

# create ws_cat_scores
cdi_adaptive(ws_cat_responses) -> ws_cat_scores
ws_cat_scores <- ws_cat_scores %>% rename(submitter_cat = guardian) # rename guardian to submitter_cat





# join full and CAT data
merge(ws_cat_scores, ws_scores[,c("id", "end_date", "start_date", "submitter_full", "submitter_full_other", "score_full")], by = "id", all.x = T) -> by_child_pl # dublets here, dealt with below in chunk on exclusions

by_child_pl$group <- fct_recode(by_child_pl$group, "Production" = "mowa")

# rename theta --> cat_theta
names(by_child_pl)[names(by_child_pl) == "theta"] <- "cat_theta"
names(by_child_pl)[names(by_child_pl) == "se_theta"] <- "cat_theta_se"
by_child_pl$cat_theta <- as.numeric(by_child_pl$cat_theta)
by_child_pl$se_theta <- as.numeric(by_child_pl$cat_theta_se)


# Data exclusions, part 1
# - exclude test, non-SW ids
# - not the same submitter in full and CAT
# - time between testings longer than 30 days
# (PART 2 is comparing bilinguals to monolinguals)
# Data fixups:
# - double ids (children with multiple CDI of the same type)

# # remove non-SW IDs
# sw <- multilada_connect(database = "starwords", prompt = "Database")
#
# sw_kids <- dbGetQuery(sw,
#                      "SELECT child.family_id, childlanguage.child_id, child.child_hash, user.test, childlanguage.language_id
#                       FROM child, childlanguage, user
#                       WHERE child.pk = childlanguage.child_id AND user.family_id = child.family_id")
# sw_kids <- unique(sw_kids) # doublets: if child hears more than one language, we have two rows, one for each language_id

sw_kids <- read_csv2("data/polish/_SELECT_child_family_id_childlanguage_child_id_child_child_hash__202412022248.csv")

by_child_pl <- subset(by_child_pl, by_child_pl$id %in% sw_kids$child_hash)


# same submitter?
by_child_pl <- by_child_pl %>% relocate(submitter_cat, .after = submitter_full)

# TODO below, there's some mess in the submitters' levels in full ws --> cannot compare them automatically
# str(by_child_pl)
# levels(by_child_pl$submitter_cat)
# levels(by_child_pl$submitter_full)
# by_child_pl$same_submitter <- by_child_pl$submitter_cat == by_child_pl$submitter_full




# time between ws and cat testing
# rename end_date.x and end_date.y
by_child_pl <- by_child_pl %>%
     rename(end_date_cat = end_date.x,
            end_date_stat = end_date.y)
by_child_pl <- by_child_pl %>%
     mutate(days_between = floor(interval(end_date_stat, end_date_cat)/ days(1)))


# if a child has two testings of the same CDI type, keep the one with smaller days_between (testings)
by_child_pl <- by_child_pl %>%
     group_by(id) %>%
     filter(days_between == min(days_between))

# if time between testings is more than 30 days --> excluded
by_child_pl$days_between <- abs(by_child_pl$days_between)
by_child_pl <- by_child_pl %>% filter(days_between < 31)




# dobs consistent across cdi and sw?
by_child_pl <- merge(by_child_pl, sw_kids[,c("child_id", "child_hash")], by.x = "id", by.y = "child_hash", all.x = T)
sw_dobs <- read_csv2("data/polish/dob_df.csv")
by_child_pl <- merge(by_child_pl, sw_dobs[,c("child_id", "dob")], by = "child_id", all.x = T)
by_child_pl <- by_child_pl %>% rename(dob_sw = dob, dob_cdi = birth_date)
by_child_pl <- by_child_pl %>% relocate(dob_sw, .after = dob_cdi)
by_child_pl[by_child_pl$child_id == "6877", "dob_sw"] <- "2021-09-26"



# age at CDI CAT
by_child_pl <- by_child_pl %>%
     mutate(age_CAT = floor(interval(dob_sw, end_date_cat)/ months(1)))

# exclude children with CAT after 36 mo
by_child_pl <- by_child_pl %>% filter(age_CAT < 37)



# update ws_responses and ws_cat_responses to delete excluded children
responses_full_pl <- subset(ws_responses, ws_responses$id %in% by_child_pl$id)
responses_cat_pl <- subset(ws_cat_responses, ws_cat_responses$idx %in% by_child_pl$id)

# double ids in responses_full_pl?
responses_full_pl %>% select(id, end_date) %>% unique() %>% group_by(id) %>% count()
# 2 duplicates:
# YPPJJn25X1pI1nTZlUag2
# O0Aj1GkMheZwClXU_teTv

# if a child has two testings of cdi ws, keep the testing that has been already chosen for by_child_pl
responses_full_pl <- responses_full_pl %>%
     semi_join(by_child_pl, by = c("id" = "id", "end_date" = "end_date_stat"))


# double ids in responses_cat_pl? i.e. a CAT for a child filled in twice
responses_cat_pl %>% select(idx, end_date) %>% unique() %>% group_by(idx) %>% count()
# 3 duplicates
# cIhfxbX2KKMJk7LeObAzU
# L3p3bjQACfa7DJwkqJTmx
# 2CZ3bfbOIudvg6jXRwAJk

# if a child has two testings of CAT, keep the testing that has been already chosen for by_child_pl
responses_cat_pl <- responses_cat_pl %>%
     semi_join(by_child_pl, by = c("idx" = "id", "end_date" = "end_date_cat"))

# add duration in responses_full_pl
responses_full_pl <- responses_full_pl %>% mutate(
     duration_full = lubridate::as.duration(end_date - start_date))

by_child_pl <- merge(by_child_pl, ws_submissions[,c("id", "duration", "end_date")], by.x = c("id", "end_date_stat"), by.y = c("id", "end_date"))






# Children's number of languages from StarWords database

# add new column (language_id_n) for each language of a child
kids_lang <- sw_kids %>%
     select(child_hash, language_id)
kids_lang <- kids_lang %>%
     filter(!is.na(child_hash)) %>%
     group_by(child_hash) %>%
     mutate(language_id = 1:n()) %>%
     ungroup() %>%
     pivot_wider(values_from = language_id,
                 names_from  = language_id,
                 names_prefix = 'language_id_')

# count languages per child
kids_lang <- kids_lang %>%
     mutate(nr_lang =(rowSums(!is.na(kids_lang[,2:8]))))

# add nr_lang to sw_kids data frame and remove double rows
sw_kids <- merge(sw_kids, kids_lang[,c("child_hash", "nr_lang")], by = "child_hash", all.x = T) %>% select(-language_id) %>% unique()

by_child_pl <- merge(by_child_pl, sw_kids, by.x = "id", by.y = "child_hash", all.x = T)
by_child_pl <- by_child_pl %>% filter(is.na(test))

# bi vs. mono
by_child_pl <- by_child_pl %>%
     mutate(lang_group = ifelse(nr_lang == 1, "monolingual", "multilingual"))



# small cleaning
by_child_pl <- by_child_pl %>% mutate(cdi = "WS")
by_child_pl <- by_child_pl %>% rename(child_id = child_id.x) %>% select(-child_id.y)
by_child_pl <- by_child_pl %>% rename(duration_cat = duration.x,
                                            duration_full = duration.y)
by_child_pl <- unique(by_child_pl)

# double ids in by_child_pl? i.e. a CAT for a child filled in twice
by_child_pl %>% select(id) %>% group_by(id) %>% count() # no duplicates

rm(ws_cat_scores, ws_responses, ws_scores, sw_kids, sw_dobs, kids_lang) # removing (original) dfs with more kids than considered here

rm(ws_cat_responses, ws_submissions)


# ---- by-item data ------

## ------- WS cat ------

# Upload item characteristics from the 2PL IRT model
items_cat_pl <- read_csv("data/polish/items_ws_cat.csv")
# add unilemmas
unilemmas <- read_csv("data/polish/uni_lemma_CDI - km_unilemma_for_r.csv")

# merge
items_cat_pl <- merge(items_cat_pl, unilemmas[,c("itemID", "definition", "category", "uni_lemma")],
                      all.x = T,
                      by.x = "item", by.y = "definition")
items_cat_pl <- items_cat_pl %>% select(-c(question, id, group))
items_cat_pl <- items_cat_pl %>% relocate(itemID, .before = item)
rm(unilemmas)

# small cleaning
items_cat_pl <- items_cat_pl %>%
     mutate(itemID = ifelse(is.na(itemID) & item == "[imię zwierzątka]", "item_343", itemID),
            itemID = ifelse(is.na(itemID) & item == "[własne imię]", "item_356", itemID))
items_cat_pl <- items_cat_pl %>%
     mutate(category = ifelse(is.na(category) & item == "[imię zwierzątka]", "people", category),
            category = ifelse(is.na(category) & item == "[własne imię]", "people", category))
items_cat_pl <- items_cat_pl %>%
     mutate(uni_lemma = ifelse(is.na(uni_lemma) & item == "[imię zwierzątka]", "pet's name", uni_lemma),
            uni_lemma = ifelse(is.na(uni_lemma) & item == "[własne imię]", "child's name", uni_lemma))

# ryba d = 1.4985849 to animals, "fish (animal)", item_42
# ryba d = 0.7313158 to food_drink, "fish (food)", item_124
items_cat_pl <- items_cat_pl %>%
     mutate(itemID = ifelse(row_number() == 473, "item_42", itemID),
            itemID = ifelse(row_number() == 474, "item_124", itemID))
items_cat_pl <- items_cat_pl %>%
     mutate(category = ifelse(row_number() == 473, "animals", category),
            category = ifelse(row_number() == 474, "food_drink", category))
items_cat_pl <- items_cat_pl %>%
     mutate(uni_lemma = ifelse(row_number() == 473, "fish (animal)", uni_lemma),
            uni_lemma = ifelse(row_number() == 474, "fish (food)", uni_lemma))

# woda d = 1.517011 to food_drink, "water (beverage)", item_133
# woda d = 1.954704 to outside, "water (not beverage)", item_311
items_cat_pl <- items_cat_pl %>%
     mutate(itemID = ifelse(row_number() == 610, "item_133", itemID),
            itemID = ifelse(row_number() == 611, "item_311", itemID))
items_cat_pl <- items_cat_pl %>%
     mutate(category = ifelse(row_number() == 610, "food_drink", category),
            category = ifelse(row_number() == 611, "outside", category))
items_cat_pl <- items_cat_pl %>%
     mutate(uni_lemma = ifelse(row_number() == 610, "water (beverage)", uni_lemma),
            uni_lemma = ifelse(row_number() == 611, "water (not beverage)", uni_lemma))

# tylko d = -2.34557113030468 to quantifiers, "only", item_591
# tylko d = -2.09252855552428 to connecting_words, "but", item_668
items_cat_pl <- items_cat_pl %>%
     mutate(itemID = ifelse(row_number() == 575, "item_591", itemID),
            itemID = ifelse(row_number() == 574, "item_668", itemID))
items_cat_pl <- items_cat_pl %>%
     mutate(category = ifelse(row_number() == 575, "quantifiers", category),
            category = ifelse(row_number() == 574, "connecting_words", category))
items_cat_pl <- items_cat_pl %>%
     mutate(uni_lemma = ifelse(row_number() == 575, "only", uni_lemma),
            uni_lemma = ifelse(row_number() == 574, "but", uni_lemma))

# ws_cat responses --> which items keep being chosen by the algorithm?
n_cat_ws <- responses_cat_pl %>% select(idx) %>% unique() %>% summarise(n()) %>% unlist() # number of unique kids = test administrations
# it's 115 not 113 not because of duplicates - maybe we had more CAT testings than CDI full testings?

# TUTAJ - chcemy mieć w responses_cat_pl także info o kategorii (dla rozdzielenia wody od wody)
# in how many test administrations did an item appear - n and %
cat_items_ws <- responses_cat_pl %>% select(items) %>%
     group_by(items) %>% summarise(n = n()) %>%
     mutate(items_perc = n/n_cat_ws)


cat_items_ws <- merge(cat_items_ws, items_cat_pl, by.x = "items", by.y = "item", all.x = T)
rm(cat_items_ws)
# needs a fix: some items seem double (e.g. "woda" or "tylko") because they belong to two CDI categories (e.g. "food and drink" and "outside")


## ------ WS full -----

# read in WS static item definitions and individual responses (0/1)
# w pliku items_ws_static.csv były oryginalnie dwie kategorie food_drink (jedna ze spacją na końcu, 3 itemy) i quantifiers jako cała kategoria miała spację na końcu ("quantifiers ")
items_full_pl <- read.csv2("data/polish/items_ws_static.csv")
# ^ tu itemy typu ja/mnie/mi to jeden item (konkretnie item 600) i tak ma być
# bo tak wygląda ten item także w modelu od PK
# cdi_itemise natomiast rozbija ten item na trzy (i inne jemu podobne też rozbija)
# efekcie, po cdi_itemise mamy więcej itemów niż przewiduje matrix PK
# ws_full_matrix_view <- cdi_itemise_oneCheckboxGroup(responses_full_pl, type = "word", items = items_full_pl)

# robię swoje itemise, zachowując temy typu ja/mnie/mi to jeden item (konkretnie item 600)
items_full_pl$category <- as.factor(items_full_pl$category)
items_full_pl$item_id <- as.factor(items_full_pl$item_id)

items_full_pl <- items_full_pl %>%
     filter(type == "word") %>%
     group_by(category)%>%
     mutate(nr_in_category = row_number())

ws_itemized <- responses_full_pl %>%
     filter(type == "word" & answer_type == "oneCheckboxGroup") %>%
     select(id, category, question_id:answer1)

ws_itemized$answer1 <- as.integer(ws_itemized$answer1)
ws_itemized$nr_in_category <- ws_itemized$answer1

# add item_id to ws_itemized
ws_itemized <- merge(ws_itemized,
                     items_full_pl[,c("category", "nr_in_category", "item_id")],
                     all.x = T,
                     by = c("category", "nr_in_category"))


# Create df marking correct items
ws_itemized <- ws_itemized %>%
     mutate(correct = 1) %>%
     select(id, item_id, correct)

# Get all combinations (participants × items)
all_items <- items_full_pl %>%
     select(item_id)

all_participants <- ws_itemized %>%
     select(id) %>%
     distinct()

full_grid <- expand.grid(id = all_participants$id,
                         item_id = all_items$item_id,
                         stringsAsFactors = FALSE)

# Merge with correct responses, fill NAs with 0
ws_itemized <- full_grid %>%
     left_join(ws_itemized, by = c("id", "item_id")) %>%
     mutate(correct = ifelse(is.na(correct), 0, correct)) %>%
     pivot_wider(names_from = item_id, values_from = correct)

rm(full_grid, all_items, all_participants)

# I need a df with 113 ppl (in rows) and 666 items (columns),
# no row for id
# no columns for the following (removed) items: "gorący" 392., "gorąco" 417., "(nie)grzeczny" 398., "o (np. o kotku)" 652.
# as in PK/GK model:
load("data/polish/output_ws.Rds")
mod <- output[[1]]
x <- mod@Data[["data"]]
rm(x, output)


ws_itemized <- ws_itemized %>%
     select(-c(item_392, item_398, item_417, item_652))

# PARENTAL CONSISTENCY

# POLISH
# WS
consistency_pl <- cdi_itemise_oneCheckboxGroup(responses_full_pl, "word", items_full_pl)

responses_cat_pl <- responses_cat_pl %>% rename(
     id = idx, definition = items)

#responses_cat_pl <- responses_cat_pl %>% rename(id = idx, definition = items)
consistency_pl <- merge(consistency_pl, responses_cat_pl[,c("id", "definition", "answers")], by = c("id", "definition"), all.x = T)

consistency_pl <- consistency_pl %>%
     rename(answer_full = response, answer_cat = answers) %>%
     filter(!is.na(answer_full)) %>%
     filter(!is.na(answer_cat))%>%
     relocate(answer_full, .before = answer_cat)

consistency_pl <- consistency_pl %>% group_by(id) %>%
     mutate(consistent = case_when(
          answer_full == answer_cat ~ "consistent",
          .default = "not_consistent"
     ))

consistency_pl_sum <- consistency_pl %>% group_by(id, consistent) %>% tally()
consistency_pl_sum <- consistency_pl_sum %>% pivot_wider(id_cols = id, names_from = consistent, values_from = n)
consistency_pl_sum <- consistency_pl_sum %>% mutate(perc = consistent/sum(consistent, not_consistent))
consistency_pl_sum %>% dplyr::summarise(mean = mean(perc, na.rm = T),
                                    median = median(perc, na.rm = T),
                                    max = max(perc, na.rm = T),
                                    min = min(perc, na.rm = T))

median(consistency_pl_sum$perc, na.rm = T)
mean(consistency_pl_sum$perc, na.rm = T)

consistency_pl_sum <- merge(consistency_pl_sum, by_child_pl[,c("id", "days_between", "score_full", "duration_cat", "duration_full", "lang_group")], by = "id", all.x = T)
consistency_pl_sum <- unique(consistency_pl_sum)


# ---- ability estimate from full ------

# Calculate thetas from the full CDIs in validation

names(ws_itemized) <- gsub("item_", "item", names(ws_itemized)) # names of cols the same as in the model

# Calculate thetas from full CDIs in the validation study
# remove id but then re-attach
id_column <- ws_itemized$id # store ids sep
ws_itemized_noid <- ws_itemized %>% select(-id) # remove ids
theta_est <- fscores(mod, method = "MAP", response.pattern = ws_itemized_noid)
full_thetas_val_pl <- cbind(id = id_column, theta_est)
rm(theta_est)
full_thetas_val_pl <- as.data.frame(full_thetas_val_pl)
full_thetas_val_pl <- full_thetas_val_pl %>% rename(full_theta = F1,
                                                    full_theta_se = SE_F1)

# write.csv(full_thetas_val_pl, "full_thetas_pl_ws_validation.csv")

by_child_pl <- merge(by_child_pl, full_thetas_val_pl, all.x = T, by = "id")
by_child_pl <- by_child_pl %>% relocate(full_theta, .after = se_theta)
by_child_pl <- by_child_pl %>% relocate(full_theta_se, .after = full_theta)
by_child_pl$full_theta <- as.numeric(by_child_pl$full_theta)
by_child_pl$full_theta_se <- as.numeric(by_child_pl$full_theta_se)
by_child_pl$cat_theta <- as.numeric(by_child_pl$cat_theta)
by_child_pl$cat_theta_se <- as.numeric(by_child_pl$cat_theta_se)
by_child_pl$sex <- factor(by_child_pl$sex, levels = c("dziewczynka", "chłopiec"))

rm(full_thetas_val_pl)


# Mark kids (observations) with too high SE of cat_theta
cat_settings_pl <- read_csv2("data/polish/cat_settings.csv")
cat_settings_pl$max_SE <- as.numeric(cat_settings_pl$max_SE)

by_child_pl$cat_theta <- as.numeric(by_child_pl$cat_theta)
by_child_pl <- by_child_pl %>%
     mutate(above_max_se_cat = case_when(
          cdi == "WS" & se_theta > 0.1 ~ "yes",
          .default = "no"
     )) %>%
     mutate(above_max_se_full = case_when(
          cdi == "WS" & full_theta_se > 0.1 ~ "yes",
          .default = "no"
     ))

