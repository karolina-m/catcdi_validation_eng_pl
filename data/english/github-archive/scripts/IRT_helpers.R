html_table_width <- function(kable_output, width){
  width_html <- paste0(paste0('<col width="', width, '">'), collapse = "\n")
  sub("<table>", paste0("<table>\n", width_html), kable_output)
} 

get_anova_table <- function(mod1, mod2, model_names=c("model1","model2")) {
  aa = anova(mod1, mod2)
  tab = data.frame(Model=model_names, AIC=aa$AIC, BIC=aa$BIC, logLik=aa$logLik, df=aa$df)
  return(tab)
}

doCAT <- function(dat, mod, max_items=50, min_SEM=.1) {
  all_items = c() # track freq of all the items that were asked 
  parms = c()
  results <- mirtCAT(mo = mod, criteria = 'MI', start_item = 'MI', method = 'MAP', # cl = cl,
                     local_pattern = dat, design = list(max_items = max_items, min_SEM = min_SEM))
  for(s in 1:nrow(dat)) {
    all_items = c(all_items, results[[s]]$items_answered)
    so <- summary(results[[s]])
    parms = rbind(parms, c(t(so$final_estimates), length(so$items_answered)))
  }
  sum_score = rowSums(dat)
  parms = data.frame(cbind(parms, sum_score))
  names(parms) = c("thetaCAT","CAT_SE","Qs_asked","sum_score")
  want = list(all_items=all_items, parms=parms)
  return(want)
}

doCAT_fixed_length <- function(dat, mod, min_items=50, criteria='MI', start_item='MI') {
  all_items = c() # track freq of all the items that were asked 
  parms = c()
  results <- mirtCAT(mo = mod, criteria = criteria, start_item = start_item, method = 'MAP', cl = cl,
                     local_pattern = dat, design = list(min_items = min_items, max_items=min_items))
  for(s in 1:nrow(dat)) {
    all_items = c(all_items, results[[s]]$items_answered)
    so <- summary(results[[s]])
    parms = rbind(parms, c(t(so$final_estimates), length(results[[s]]$items_answered)))
  }
  sum_score = rowSums(dat)
  parms = data.frame(cbind(parms, sum_score))
  names(parms) = c("thetaCAT","CAT_SE","Qs_asked","sum_score")
  want = list(all_items=all_items, parms=parms)
  return(want)
}

summarize_CAT <- function(catdat, d_mat, fscores_2pl, verbose=F) {
  meanSE = mean(catdat$parms$CAT_SE)
  never_selected = setdiff(1:ncol(d_mat), unique(catdat$all_items))
  num_unused = length(never_selected)
  item_freq = sort(table(catdat$all_items))
  #names(which(item_freq<10))
  mean_Qs_asked = mean(catdat$parms$Qs_asked)
  median_Qs_asked = median(catdat$parms$Qs_asked)
  cond = max(catdat$parms$Qs_asked)
  # correlation with subject's estimated ability on full CDI
  r_cat_full = cor(fscores_2pl$ability, catdat$parms$thetaCAT)
  reliability = 1-mean(catdat$parms$CAT_SE)^2
  # also look at correlation of sum_score?
  #cor(catdat$parms$thetaCAT, catdat$parms$sum_score)
  if(verbose) {
    plot(fscores_2pl$ability, catdat$parms$thetaCAT)
    plot(catdat$parms$thetaCAT, catdat$parms$CAT_SE)
    print("Items that were never selected:")
    print(never_selected)
  }
  return(cbind(cond, median_Qs_asked, mean_Qs_asked, r_cat_full, meanSE, reliability, num_unused))
}

preferredCAT <- function(dat, method='ML', min_SEM=.15, start_item=c()) {
  all_items = c() # track freq of all the items that were asked 
  parms = c()
  if(length(start_item)==0) start_item = 'MI' # otherwise supply age-based
  results <- mirtCAT(mo = mod_2pl, criteria = 'MI', start_item = start_item, 
                     method = method, cl = cl, local_pattern = dat, 
                     design = list(min_items = 25,
                                   max_items = 50, 
                                   min_SEM = min_SEM))
  for(s in 1:nrow(dat)) {
    all_items = c(all_items, results[[s]]$items_answered)
    so <- summary(results[[s]])
    parms = rbind(parms, c(t(so$final_estimates), length(so$items_answered)))
  }
  sum_score = rowSums(dat)
  parms = data.frame(cbind(parms, sum_score))
  names(parms) = c("thetaCAT","CAT_SE","Qs_asked","sum_score")
  want = list(all_items=all_items, parms=parms)
  return(want)
}

get_cor_by_age <- function(d, catdat) {
  d$thetaCAT = catdat$parms$thetaCAT
  cors <- d %>%
    group_by(age_group) %>% 
    summarize(r=cor(ability, thetaCAT))
  return(cors)
}

# accepts residuals(model, type="LD"), returns items with LD strengths at/above assoc_str
# no association = abs(V) < .1 no association, .3 is moderate, and .5+ is strong
get_LD_violations <- function(res, assoc_str = .3) {
  vio = rep(NA, nrow(res))
  for(i in 1:nrow(res)) {
    vio[i] = length(which(abs(res[i,i:ncol(res)])>=assoc_str))
  }
  return(vio)
}

# find item with maximum information at given theta value
get_max_info_item <- function(mod, theta) {
  infos = rep(NA, nrow(coefs_2pl))
  for(i in 1:nrow(coefs_2pl)) {
    infos[i] = iteminfo(extract.item(mod, i), theta)
  }
  return(list(item=which(infos==max(infos)), info=max(infos)))
}


get_item_info_1d <- function(mod, item) {
  ii <- extract.item(mod, item)
  Theta <- matrix(seq(-4,4, by = .1))
  info <- iteminfo(ii, Theta)
  return(sum(info))
}

get_item_info_2d <- function(mod, item) {
  ii <- extract.item(mod, item)
  #Theta <- as.matrix(expand.grid(-4:4, -4:4))
  Theta <- as.matrix(expand.grid(seq(-4,4,by=.5), seq(-4,4,by=.5)))
  info = iteminfo(ii, Theta, degrees=c(45,45)) # equal angle
  info1d = iteminfo(ii, Theta, degrees=c(90,0)) # first dimension only
  info2d = iteminfo(ii, Theta, degrees=c(0,90))
  
  # information matrices
  #iteminfo(ii, Theta, multidim_matrix = TRUE)
  #iteminfo(ii, Theta[1, , drop=FALSE], multidim_matrix = TRUE)
  return(c(sum(info), sum(info1d), sum(info2d)))
}


make_fixed_cat_table <- function(d_mat, fs, has400=T) {
  fcat_tab = summarize_CAT(f25, d_mat, fs)
  fcat_tab = rbind(fcat_tab, summarize_CAT(f50, d_mat, fs))
  fcat_tab = rbind(fcat_tab, summarize_CAT(f75, d_mat, fs))
  fcat_tab = rbind(fcat_tab, summarize_CAT(f100, d_mat, fs))
  fcat_tab = rbind(fcat_tab, summarize_CAT(f200, d_mat, fs))
  fcat_tab = rbind(fcat_tab, summarize_CAT(f300, d_mat, fs))
  if(has400) fcat_tab = rbind(fcat_tab, summarize_CAT(f400, d_mat, fs))
  
  fcat_tab = data.frame(fcat_tab)
  fcat_tab$median_Qs_asked = NULL # constant
  fcat_tab$mean_Qs_asked = NULL # constant
  names(fcat_tab) = c("Test Length", "r vs. full CDI", "Mean SE", 
                      "Reliability", "Unused Items") 
  
  rand_baseline = c(cor(r25$parms$thetaCAT, fs$ability),
                    cor(r50$parms$thetaCAT, fs$ability),
                    cor(r75$parms$thetaCAT, fs$ability),
                    cor(r100$parms$thetaCAT, fs$ability),
                    cor(r200$parms$thetaCAT, fs$ability),
                    cor(r300$parms$thetaCAT, fs$ability))
  
  rand_SE = c(mean(r25$parms$CAT_SE), mean(r50$parms$CAT_SE), mean(r75$parms$CAT_SE),
              mean(r100$parms$CAT_SE), mean(r200$parms$CAT_SE), mean(r300$parms$CAT_SE))
  
  if(has400) {
    rand_baseline = c(rand_baseline, cor(r400$parms$thetaCAT, fs$ability))
    rand_SE = c(rand_SE, mean(r400$parms$CAT_SE))
  }
  
  fcat_tab[,"Unused Items"] = NULL # table is too wide with this
  fcat_tab[,"r Random vs. full CDI"] = rand_baseline
  fcat_tab[,"Random SEM"] = rand_SE
  return(fcat_tab)
}

early_stopping_CAT_sim_table <- function(d_mat, fs, has400 = T) {
  cat_tab = summarize_CAT(t25, d_mat, fs)
  cat_tab = rbind(cat_tab, summarize_CAT(t50, d_mat, fs))
  cat_tab = rbind(cat_tab, summarize_CAT(t75, d_mat, fs))
  cat_tab = rbind(cat_tab, summarize_CAT(t100, d_mat, fs))
  cat_tab = rbind(cat_tab, summarize_CAT(t200, d_mat, fs))
  cat_tab = rbind(cat_tab, summarize_CAT(t300, d_mat, fs))
  # don't have these for comprehension
  if(has400) cat_tab = rbind(cat_tab, summarize_CAT(t400, d_mat, fs))
  cat_tab = data.frame(cat_tab)
  
  names(cat_tab) = c("Max. Items", "Median Items", "Mean Items", "r with full CDI", 
                     "Mean SE", "Reliability", "Unused Items")
  return(cat_tab)
}

get_prefCAT_age_table <- function(prefCATfilepath, d_demo, n_age) {
  load(prefCATfilepath)
  r1 = get_cor_by_age(d_demo, min25_max50_ML)
  r2 = get_cor_by_age(d_demo, min25_max50_MAP)
  r3 = get_cor_by_age(d_demo, min25_max50_ML_age)
  r4 = get_cor_by_age(d_demo, min25_max50_MAP_age)
  
  age_tabp = rbind(c("ML / MI", round(r1$r, 3)), 
                   c("MAP / MI", round(r2$r, 3)), 
                   c("ML / age-based", round(r3$r, 3)), 
                   c("MAP / age-based", round(r4$r, 3)))
  #colnames(age_tabp) = c("Scoring / Start Item", paste(names(n_age),"mos")) # columns too wide
  colnames(age_tabp) = c("Scoring / Start Item", names(n_age))
  return(age_tabp)
}

