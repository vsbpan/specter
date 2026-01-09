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
      ID, part, x_length, y_mean, y_log_mean, p_missing, periodic, 
      mean_freq, x_median, x_min, x_max, whole_x_length, whole_x_min, whole_x_max
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
      temp_log_freq = log(temp_freq),
      temp_log_freq_mean = mean(temp_log_freq),
      temp_log_freq_offset = temp_log_freq - temp_log_freq_mean,
      estimate.r_mean = mean(estimate.r),
      estimate.r_offset = estimate.r - estimate.r_mean
    ) %>% 
    ungroup() %>% 
    mutate(
      x_median_mean_scale = as.numeric(scale(x_median_mean)),
      x_median_offset_scale = as.numeric(scale(x_median_offset)), 
      temp_mean_scale = as.numeric(scale(temp_mean)),
      temp_offset_scale = as.numeric(scale(temp_offset)),
      inv_temp_mean_scale = as.numeric(scale(inv_temp_mean)),
      inv_temp_offset_scale = as.numeric(scale(inv_temp_offset)),
      temp_log_freq_mean_scale = as.numeric(scale(temp_log_freq_mean)),
      temp_log_freq_offset_scale = as.numeric(scale(temp_log_freq_offset)),
      estimate.r_mean_scale = as.numeric(scale(estimate.r_mean)),
      estimate.r_offset_scale = as.numeric(scale(estimate.r_offset)),
      x_length_log = log(x_length),
      whole_x_length_log = log(whole_x_length),
      x_length_log_scale = as.numeric(scale(x_length_log)),
      whole_x_length_log_scale = as.numeric(scale(whole_x_length_log))
    )
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
      ID, part, x_length, y_mean, y_log_mean, p_missing, periodic, 
      mean_freq, x_median, x_min, x_max, whole_x_length, whole_x_min, whole_x_max
    ) %>% 
    rename(
      pop_freq = mean_freq
    ) %>% 
    filter(!is.na(pop_freq)) %>% 
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
    group_by(ID) %>% 
    mutate(
      x_median_mean = mean(x_median),
      x_median_offset = x_median - x_median_mean,
      temp_mean = mean(temp),
      temp_offset = temp - temp_mean,
      inv_temp_mean = mean(inv_temp),
      inv_temp_offset = inv_temp - inv_temp_mean,
      temp_log_freq = log(temp_freq),
      temp_log_freq_mean = mean(temp_log_freq),
      temp_log_freq_offset = temp_log_freq - temp_log_freq_mean
    ) %>% 
    ungroup() %>% 
    mutate(
      x_median_mean_scale = as.numeric(scale(x_median_mean)),
      x_median_offset_scale = as.numeric(scale(x_median_offset)), 
      temp_mean_scale = as.numeric(scale(temp_mean)),
      temp_offset_scale = as.numeric(scale(temp_offset)),
      inv_temp_mean_scale = as.numeric(scale(inv_temp_mean)),
      inv_temp_offset_scale = as.numeric(scale(inv_temp_offset)),
      temp_log_freq_mean_scale = as.numeric(scale(temp_log_freq_mean)),
      temp_log_freq_offset_scale = as.numeric(scale(temp_log_freq_offset)),
      x_length_log = log(x_length),
      whole_x_length_log = log(whole_x_length),
      x_length_log_scale = as.numeric(scale(x_length_log)),
      whole_x_length_log_scale = as.numeric(scale(whole_x_length_log))
    )
}, envir = env)

rm(env)


tre_mast <- ape::read.tree("cleaned_data/mastTree_tree.tre")
tre_mast$tip.label <- gsub("_", " ", tre_mast$tip.label)
tre_mast <- ape::keep.tip(tre_mast, unique(d_mast$taxonname))
tre_mast$vcv <- phytools::vcvPhylo(tre_mast)


tre_GPDD <- ape::read.tree("cleaned_data/GPDD_tree.tre")
tre_GPDD <- ape::keep.tip(tre_GPDD, base::intersect(tre_GPDD$tip.label, unique(d_GPDD$ott_id)))
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


d_mast <- d_mast %>% 
  rename(
    species = name_cleaned,
    tree_name = taxonname
  )


d_GPDD <- d_GPDD %>% 
  rename(
    species = name_cleaned
  ) %>% 
  mutate(
    tree_name = species
  )

d_GPDD <- d_GPDD %>% 
  mutate(
    genus = assign_unknown_classification(genus, species),
    family = assign_unknown_classification(family, species),
    order = assign_unknown_classification(order, species),
    class = assign_unknown_classification(class, species),
    phylum = assign_unknown_classification(phylum, species)
  )

# ggtree::ggtree(tre_mast, aes(color = group), layout = "circular") +
#   ggtree::geom_tiplab(aes(label = label), color = "black", size = 2) +
#   ggtree::geom_tippoint(aes(color = group)) +   
#   scale_color_manual(
#     values = unname(pals::alphabet())
#   )


d_GPDD2 <- d_GPDD %>% 
  filter(if_all(c(x_median_offset_scale, 
                  x_median_mean_scale, 
                  x_length_log_scale,
                  inv_temp_offset_scale,
                  inv_temp_mean_scale,
                  estimate.r_offset_scale,
                  estimate.r_mean_scale,
                  temp_log_freq_offset_scale, 
                  temp_log_freq_mean_scale), 
                ~ !is.na(.))) %>% 
  group_by(ID) %>% 
  filter(n() == 2) %>% 
  ungroup()

d_mast2 <- d_mast %>% 
  filter(if_all(c(x_median_offset_scale, 
                  x_median_mean_scale, 
                  x_length_log_scale,
                  inv_temp_offset_scale,
                  inv_temp_mean_scale,
                  temp_log_freq_offset_scale, 
                  temp_log_freq_mean_scale), 
                ~ !is.na(.))) %>% 
  group_by(ID) %>% 
  filter(n() == 2) %>% 
  ungroup()







