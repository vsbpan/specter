library(tidyverse)
vmisc::load_all2("specter")

env <- new.env()

evalq({
  d_pop <- suppressMessages(read_csv("cleaned_data/GPDD_pop_res.csv"))
  d_ricker <- suppressMessages(read_csv("cleaned_data/GPDD_ricker_res.csv"))
  d_temp <- suppressMessages(read_csv("cleaned_data/GPDD_temp_res.csv"))
  d_meta <- suppressMessages(read_csv("cleaned_data/GPDD_meta_cleaned.csv"))
  
  d_temp <- d_temp %>% 
    dplyr::select(ID, part, y_mean, y_inv_mean, whole_mean_freq, mean_freq) %>% 
    rename(
      temp = y_mean,
      inv_temp = y_inv_mean,
      temp_freq = mean_freq
    )
  
  d_ricker <- d_ricker %>% 
    filter(converged) %>% 
    dplyr::select(ID, part, starts_with("se"), starts_with("estimate"))
  
  d_pop <- d_pop %>% 
    filter(!error) %>% 
    mutate(
      p_missing = y_missing / y_n,
      periodic = whole_n_freq > 0
    ) %>% 
    dplyr::select(
      ID, part, x_length, y_mean, y_log_mean, p_missing, periodic, mean_freq, x_median, x_min, x_max
    ) %>% 
    rename(
      pop_freq = mean_freq
    ) %>% 
    filter(
      x_length > 5 & p_missing < 0.2
    ) %>% 
    group_by(ID) %>% 
    filter(
      n() == 2
    ) %>% 
    ungroup()
  
  d_GPDD <<- d_pop %>% 
    left_join(
      d_temp, by = c("ID", "part")
    ) %>% 
    left_join(
      d_ricker, by = c("ID", "part")
    ) %>% 
    inner_join(
      d_meta %>% 
        mutate(ID = mainid),
      by = "ID"
    ) %>% 
    mutate(
      ott_id = as.character(ott_id)
    ) %>% 
    group_by(ID) %>% 
    mutate(
      x_median_mean = mean(x_median),
      x_median_offset = x_median - x_median_mean,
      temp_mean = mean(temp),
      temp_offset = temp - temp_mean,
      inv_temp_mean = mean(inv_temp),
      inv_temp_offset = inv_temp - inv_temp_mean,
      temp_freq_mean = mean(temp_freq),
      temp_freq_offset = temp_freq - temp_freq_mean,
      estimate.r_mean = mean(estimate.r),
      estimate.r_offset = estimate.r - estimate.r_mean
    ) %>% 
    ungroup()
}, envir = env)

rm(env)


env <- new.env()

evalq({
  d_pop <- suppressMessages(read_csv("cleaned_data/mastTree_pop_res.csv"))
  d_temp <- suppressMessages(read_csv("cleaned_data/mastTree_temp_res.csv"))
  d_meta <- suppressMessages(read_csv("cleaned_data/mastTree_meta_cleaned.csv"))
  
  d_temp <- d_temp %>% 
    dplyr::select(ID, part, y_mean, y_inv_mean, whole_mean_freq, mean_freq) %>% 
    rename(
      temp = y_mean,
      inv_temp = y_inv_mean,
      temp_freq = mean_freq
    )
  
  d_pop <- d_pop %>% 
    filter(!error) %>% 
    mutate(
      p_missing = y_missing / y_n,
      periodic = whole_n_freq > 0
    ) %>% 
    dplyr::select(
      ID, part, x_length, y_mean, y_log_mean, p_missing, periodic, mean_freq, x_median, x_min, x_max
    ) %>% 
    rename(
      pop_freq = mean_freq
    ) %>% 
    filter(
      x_length > 5 & p_missing < 0.2
    ) %>% 
    group_by(ID) %>% 
    filter(
      n() == 2
    ) %>% 
    ungroup()
  
  d_mast <<- d_pop %>% 
    left_join(
      d_temp, by = c("ID", "part")
    ) %>% 
    inner_join(
      d_meta %>% 
        mutate(ID = series_id),
      by = "ID"
    ) %>% 
    mutate(
      ott_id = as.character(ott_id)
    ) %>% 
    group_by(ID) %>% 
    mutate(
      x_median_mean = mean(x_median),
      x_median_offset = x_median - x_median_mean,
      temp_mean = mean(temp),
      temp_offset = temp - temp_mean,
      inv_temp_mean = mean(inv_temp),
      inv_temp_offset = inv_temp - inv_temp_mean,
      temp_freq_mean = mean(temp_freq),
      temp_freq_offset = temp_freq - temp_freq_mean
    ) %>% 
    ungroup()
}, envir = env)

rm(env)


tre_mast <- ape::read.tree("cleaned_data/mastTree_tree.tre")
tre_mast$tip.label <- gsub("_", " ", tre_mast$tip.label)
tre_mast <- ape::keep.tip(tre_mast, unique(d_mast$taxonname))
tre_mast$vcv <- phytools::vcvPhylo(tre_mast)


tre_GPDD <- ape::read.tree("cleaned_data/GPDD_tree.tre")
d_GPDD <- d_GPDD %>% 
  filter(ott_id %in% tre_GPDD$tip.label)
tre_GPDD <- ape::keep.tip(tre_GPDD, unique(d_GPDD$ott_id))
tre_GPDD$tip.label <- d_GPDD$name_cleaned[match(tre_GPDD$tip.label, d_GPDD$ott_id)]
tre_GPDD$vcv <- phytools::vcvPhylo(tre_GPDD)

tre_GPDD <- ggtree::groupOTU(tre_GPDD, d_GPDD %>% 
                               dplyr::select(name_cleaned, class) %>% 
                               unique() %>% 
                               group_by(class) %>% 
                               summarise(
                                 tips = setNames(list(name_cleaned), dplyr::cur_group())
                               ) %>% 
                               .$tips %>% 
                               keep_len(10))

levels(attributes(tre_GPDD)$group)[1] <- "other"
attributes(tre_GPDD)$group <- factor(attributes(tre_GPDD)$group, 
                                     levels = c(levels(attributes(tre_GPDD)$group)[-1], "other"))

# ggtree::ggtree(tre_GPDD, aes(color = group), layout = "circular") +
#   ggtree::geom_tiplab(aes(label = label), color = "black", size = 2) +
#   ggtree::geom_tippoint(aes(color = group)) + 
#   scale_color_brewer(type = "qual")

tre_mast <- ggtree::groupOTU(tre_mast, d_mast %>% 
                               dplyr::select(taxonname, order) %>% 
                               unique() %>%
                               group_by(order) %>% 
                               summarise(
                                 tips = setNames(list(taxonname), dplyr::cur_group())
                               ) %>% 
                               .$tips %>% 
                               keep_len(10))
levels(attributes(tre_mast)$group)[2] <- "other"
attributes(tre_mast)$group <- ifelse(attributes(tre_mast)$group == "", 
                                     "other",
                                     as.character(attributes(tre_mast)$group))
attributes(tre_mast)$group <- factor(attributes(tre_mast)$group, 
                                     levels = c(base::setdiff(unique(attributes(tre_mast)$group), "other"), "other"))

# ggtree::ggtree(tre_mast, aes(color = group), layout = "circular") +
#   ggtree::geom_tiplab(aes(label = label), color = "black", size = 2) +
#   ggtree::geom_tippoint(aes(color = group)) +   
#   scale_color_manual(
#     values = unname(pals::alphabet())
#   )











