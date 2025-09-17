## English Data

# now wordbank WS & WG data have been merged with older WebCDI kids in data-merge-WS-WG.R
#items <- wordbankr::get_item_data(language = "English (American)", form = "WS") %>%
#  filter(type=="word") %>% arrange(definition)
#items <- items %>% select(num_item_id,definition,lexical_class,category,uni_lemma)

# now WS data has been merged WG and older WebCDI kids in data-merge-WS-WG.R
load(here("data/english/github-archive/data/production/wordbank_eng_ws_wg_webcdi31-36mos.Rds"))
eng_ws_items = items

too_young <- which(d_demo$age < 12) # 378 children can't be producing any words yet

d_demo_en = d_demo[-too_young,]
d_mat_en = d_mat[-too_young,]
d_demo_en$production = rowSums(d_mat_en, na.rm=T)

# WG comprehension data -- keep all WG subjects
load(here("data/english/github-archive/data/comprehension/wordbank_eng_wg_webcdi.Rds"))
en_mat_wg = d_mat_wg #[-which(d_demo$age < 12),]

demo_eng_wg = d_demo #subset(d_demo, age>=12)
eng_wg_subjs = nrow(demo_eng_wg)
eng_wg_items = wg_items %>% filter(!is.na(lexical_class))
rm(d_mat_wg, d_demo) # need this for anything?

## Spanish Data

# now wordbank data has been merged in data-merge-Spanish-WS-WG-III.R
load(here("data/english/github-archive/data/production/wordbank_sp_ws_wg_webcdi12-30mos.Rds"))
d_demo_sp = d_demo # already has <12 mos removed
d_mat_sp = d_mat
sp_ws_items = items %>% filter(!is.na(lexical_class))
rm(d_mat, d_demo, items)

# WG comprehension data -- keep all WG subjects
load(here("data/english/github-archive/data/comprehension/wordbank_sp_wg_webcdi.Rds"))
sp_mat_wg = d_mat_wg # [-which(d_demo$age < 12),]

demo_sp_wg = d_demo #subset(d_demo, age>=12)
sp_wg_subjs = nrow(demo_sp_wg)
sp_wg_items = wg_items %>% filter(!is.na(lexical_class)) # 428
rm(d_mat_wg, d_demo)


d_demo_en$age_group = cut(d_demo_en$age, breaks=seq(12,36,3), right=F, include.lowest = T)
n_age_en = table(d_demo_en$age_group)

d_demo_sp$age_group = cut(d_demo_sp$age, breaks=seq(12,30,3), right=F, include.lowest = T)
n_age_sp = table(d_demo_sp$age_group)

demo_eng_wg$age_group = cut(demo_eng_wg$age, breaks=seq(8,18,2), right=F, include.lowest = T)
n_age_enC = table(demo_eng_wg$age_group)

demo_sp_wg$age_group = cut(demo_sp_wg$age, breaks=seq(8,18,2), right=F, include.lowest = T)
n_age_spC = table(demo_sp_wg$age_group)
