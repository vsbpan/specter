source("get_data/data_prep.R")
library(brms)

list.files("rds_files", full.names = TRUE, pattern = "m5") %>% 
  lapply(function(x){
    nm <- tools::file_path_sans_ext(basename(x))
    assign(
      nm, 
      readRDS(x),
      envir = globalenv()
    )
  }) %>% invisible()


# 
# d_draws <- tidybayes::tidy_draws(GPDD_m5) %>% 
#   dplyr::select(starts_with("b_")) %>% 
#   dplyr::select(-ends_with("Intercept"),  -ends_with("x_length_log_scale"), -ends_with("mean_scale"))
# 
# 
# d_draws %>% 
#   gather() %>% 
#   ggplot(aes(x = key, y = value)) + 
#   tidybayes::stat_halfeye() + 
#   coord_flip() 

auto_plot <- function(model, term, response, raw_term = gsub("_scale", "", term), 
                group, df, term_sd_df, x_lab = term, annotate = "foo"){
  df$x__ <- df[[raw_term]]
  df$group__ <- df[[group]]
  df$y__ <- df[[response]]
  response2 <- gsub("[[:punct:]]", "", response)
  term_sd <- sd(term_sd_df[[raw_term]], na.rm = TRUE)
  b_vec <- sample(hypothesis_draws(model,
                                   hypothesis = sprintf("%s_%s = 0", response2, term))$H1, 
                  1000)
  # overlaps_zero() returns FALSE if overlaps zero
  if(overlaps_zero(quantile(b_vec, prob = c(0.025,0.975)))){
    line_col <- "black"
  } else {
    line_col <- "grey"
  }
  
  seq_interval(model$data[[term]], 10) %>% 
    expand.grid(
      "x" = .,
      "b" = b_vec
    ) %>% 
    mutate(
      y = (exp(b * x) - 1) * 100,
      x = x * term_sd
    ) %>% 
    group_by(x) %>% 
    summarise(
      estimate__ = mean(y),
      upper__ = quantile(y, prob = 0.975),
      lower__ = quantile(y, prob = 0.025)
    ) %>% 
    ggplot(aes(x = x, 
               y = estimate__)) + 
    geom_line(
      data = df %>%
        group_by(group__) %>%
        mutate(
          y = (y__ / mean(y__) - 1) * 100
        ),
      aes(x = x__, y = y, group = group__),
      alpha = 0.1,
      color = "steelblue",
      linewidth = 0.5
    ) +
    geom_ribbon(
      aes(ymax = upper__, ymin = lower__),
      alpha = 0.2
    ) + 
    geom_line(
      linewidth = 1, color = line_col
    ) + 
    annotation_custom(grid::textGrob(annotate, 0.025, 0.95, just = "left", 
                                     gp = grid::gpar(fontsize = 8))) +  
    labs(x = x_lab, y = "% change in frequency") + 
    theme_bw(base_size = 9)
}

g0 <- auto_plot(GPDD_m5, 
                term = "y_min_log_offeset_scale", 
                response = "pop_freq", 
                group = "ID",
                df = d_GPDD2, 
                term_sd_df = d_GPDD,
                x_lab = tex("Change in log min. density (ln(dens.))"),
                annotate = "Population density")

g1 <- auto_plot(GPDD_m5, 
                term = "inv_temp_offset_scale", 
                response = "pop_freq", 
                group = "ID",
                df = d_GPDD2, 
                term_sd_df = d_GPDD,
                x_lab = tex("Change in inverse temp. ($K^{-1}$)"),
                annotate = "Population density") + 
  scale_x_continuous(labels = fancy_scientific) +
  annotation_custom(grid::textGrob("warmer", 0.025, 1 - 0.95, just = "left", 
                                   gp = grid::gpar(fontsize = 8, col = "grey"))) +
  annotation_custom(grid::textGrob("cooler", 1 - 0.025, 1 - 0.95, just = "right", 
                                   gp = grid::gpar(fontsize = 8, col = "grey"))); g1

g2 <- auto_plot(GPDD_m5, 
                term = "temp_log_freq_offset_scale", 
                response = "pop_freq", 
                group = "ID",
                df = d_GPDD2, 
                term_sd_df = d_GPDD,
                x_lab = tex("Change in log temp. freq. (ln($year^{-1}$))"),
                annotate = "Population density")

g3 <- auto_plot(GPDD_m5, 
                term = "estimate.r_offset_scale", 
                response = "pop_freq", 
                group = "ID",
                df = d_GPDD2, 
                term_sd_df = d_GPDD,
                x_lab = tex("Change in growth rate ($year^{-1}$)"),
                annotate = "Population density")

g4 <- auto_plot(mast_m5, 
                term = "inv_temp_offset_scale", 
                response = "pop_freq", 
                group = "ID",
                df = d_mast2, 
                term_sd_df = d_mast,
                x_lab = tex("Change in inverse temp. ($K^{-1}$)"),
                annotate = "Masting density") + 
  scale_x_continuous(labels = fancy_scientific) + 
  annotation_custom(grid::textGrob("warmer", 0.025, 1 - 0.95, just = "left", 
                                   gp = grid::gpar(fontsize = 8, col = "grey"))) +
  annotation_custom(grid::textGrob("cooler", 1 - 0.025, 1 - 0.95, just = "right", 
                                   gp = grid::gpar(fontsize = 8, col = "grey")))

g5 <- auto_plot(mast_m5, 
                term = "temp_log_freq_offset_scale", 
                response = "pop_freq", 
                group = "ID",
                df = d_mast2, 
                term_sd_df = d_mast,
                x_lab = tex("Change in log temp. freq. (ln($year^{-1}$))"),
                annotate = "Masting density")




compute_mediation <- function(model, exposure, mediator, response, 
                              raw_exposure = gsub("_scale", "", exposure), exposure_sd_df){
  exposure <- assert_atomic_type(exposure, "character")
  response <- assert_atomic_type(response, "character")
  raw_exposure <- assert_atomic_type(raw_exposure, "character")
  
  response2 <- gsub("[[:punct:]]", "", response)
  mediator2 <- gsub("[[:punct:]]", "", mediator)
  term_sd <- sd(exposure_sd_df[[raw_exposure]], na.rm = TRUE)
  
  
  v <- hypothesis_draws(model,
                   hypothesis = sprintf("%s_%s * %s_%s = 0", 
                                        response2, mediator, 
                                        mediator2, exposure))
  v <- v[, 1:length(mediator) , drop = FALSE]
  names(v) <- mediator
  v <- v / term_sd
  tidyr::gather(v, key = "mediator", value = "beta") %>% 
    dplyr::group_by(mediator) %>% 
    dplyr::mutate(
      sig = overlaps_zero(quantile(beta, prob = c(0.025, 0.975)))
    ) %>% 
    dplyr::ungroup()
}

compute_mediation(GPDD_m5, 
                  exposure = "x_median_offset_scale", 
                  mediator = c("estimate.r_offset_scale", "inv_temp_offset_scale", 
                               "temp_log_freq_offset_scale", "y_min_log_offeset_scale"),
                  response = "pop_freq",
                  exposure_sd_df = d_GPDD) %>% 
  cbind(
    "type" = "Population density"
  ) %>% 
  rbind(
    compute_mediation(mast_m5, 
                      exposure = "x_median_offset_scale", 
                      mediator = c("inv_temp_offset_scale", "temp_log_freq_offset_scale"),
                      response = "pop_freq",
                      exposure_sd_df = d_GPDD) %>% 
      cbind(
        "type" = "Masting density"
      )
  ) -> mediator_df


foo <- function(mediator_df, annotate){
  mediator_df %>% 
    mutate(
      beta = (exp(beta * 10) - 1) * 100,
      mediator = case_when(
        mediator == "inv_temp_offset_scale" ~ "Inv. temp.",
        mediator == "temp_log_freq_offset_scale" ~ "Temp. freq.",
        mediator == "estimate.r_offset_scale" ~ "Growth rate",
        mediator == "y_min_log_offeset_scale" ~ "Log min dens."
      )
    ) %>% 
    ggplot(aes(x = mediator, y = beta)) + 
    geom_hline(aes(yintercept = 0), linewidth = 0.6, linetype = 2, color = "grey") + 
    tidybayes::stat_halfeye(
      aes(fill = sig, color = sig), 
      stroke = 0.3, interval_size_range = c(0.3, 0.8), 
      show.legend = FALSE
    ) +
    scale_fill_manual(values = c("#4682B433","#4682B4CC")) + 
    scale_color_manual(values = c("grey","black")) + 
    coord_flip() + 
    theme_bw(base_size = 9) + 
    #facet_wrap(~type, ncol = 2, scales = "free") + 
    theme(
      axis.text.y = element_text(angle = 45, hjust = 0.5),
      axis.title.y = element_blank(),
      axis.title.x = element_text(hjust = 1), 
      strip.background = element_blank()
    ) + 
    annotation_custom(grid::textGrob(annotate, 0.025, 0.95, just = "left", 
                                     gp = grid::gpar(fontsize = 8))) + 
    labs(x = "Mediator", y = "Mediation (% change in freq. / decade)")
}


foo(mediator_df %>% 
      filter(type == "Population density"), 
    "Population density") -> g6;g6

foo(mediator_df %>% 
      filter(type == "Masting density"),
    "Masting density") -> g7;g7



library(patchwork)

(g0 + 
    labs() + 
    theme()) + 
  (g1 + 
     labs(y = "") + 
     theme(axis.title.y = element_blank(),
           axis.text.y = element_blank())) + 
  (g2 + 
     labs(y = "") + 
     theme(axis.title.y = element_blank(),
           axis.text.y = element_blank())) + 
  (g6) +
  (g3 +
     labs() + 
     theme()) + 
  (g4 + 
     labs(y = "") +
     theme(axis.title.y = element_blank(),
           axis.text.y = element_blank(), 
           axis.title.x = element_text(hjust = 0))) + 
  (g5 + 
     labs(y = "") + 
     theme(axis.title.y = element_blank(), 
           axis.text.y = element_blank())) + 
  (g7) + 
  patchwork::plot_layout(nrow = 2) + 
  patchwork::plot_annotation(tag_levels = "A") & 
  theme(
    plot.tag.position = c(0.035, 1.04)
  ) -> g_final;g_final


ggsave("graphs/mediator_figure.pdf", g_final, dpi = 600, width = 9, height = 4.5, bg = "white")
ggsave("graphs/mediator_figure.png", g_final, dpi = 600, width = 9, height = 4.5, bg = "white")

