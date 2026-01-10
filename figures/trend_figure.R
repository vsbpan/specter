source("get_data/data_prep.R")

library(brms)

list.files("rds_files", full.names = TRUE) %>% 
  lapply(function(x){
    nm <- tools::file_path_sans_ext(basename(x))
    assign(
      nm, 
      readRDS(x),
      envir = globalenv()
    )
  }) %>% invisible()


hypothesis_draws(GPDD_m1, GPDD_m2, GPDD_m3, GPDD_m4, 
                 hypothesis = "x_median_offset_scale = 0") %>% 
  mutate(
    x = case_when(
      model == "GPDD_m1" ~ "Overall",
      model == "GPDD_m2" ~ "Periodic",
      model == "GPDD_m3" ~ "No~missing",
      model == "GPDD_m4" ~ as.character(vmisc::tex("Length $\\geq 20$"))
    )
  ) %>% 
  ggplot(aes(x = forcats::fct_reorder(x, order(model, decreasing = T)), 
             y = (exp(
               H1 / sd(d_GPDD$x_median_offset) * 10
             ) - 1) * 100
               )) +
  geom_hline(aes(yintercept = 0), linewidth = 0.6, linetype = 2, color = "grey") + 
  tidybayes::stat_halfeye(
    fill = "steelblue", alpha = 0.8, stroke = 0.3, interval_size_range = c(0.3, 0.8)
  ) +
  scale_x_discrete(label = scales::label_parse()) + 
  scale_y_continuous(sec.axis = sec_axis(breaks = 5, transform = identity, labels = "# of series           ")) + 
  geom_text(
    data = data.frame(
      "x" = c("Overall",
              "Periodic",
              "No~missing",
              as.character(vmisc::tex("Length $\\geq 20$"))), 
      "y" = 5,
      "label" = c(nobs(GPDD_m1)/2, nobs(GPDD_m2)/2, nobs(GPDD_m3)/2, nobs(GPDD_m4)/2),
      model = "foo"
    ),
    aes(x = x, y = y, label = label), size = 2.5, hjust = 1
  ) + 
  coord_flip() + 
  labs(x = "", y = "% change in frequency / decade") + 
  theme_bw(base_size = 10) + 
  theme(
    axis.text.y = element_text(angle = 45, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_text(hjust = 1)
  ) -> g1; g1


prepare_newdata(GPDD_m1, "x_median_offset_scale", n = 10)$x_median_offset_scale %>% 
  expand.grid(
    "x" = .,
     "b" = sample(hypothesis_draws(GPDD_m1,hypothesis = "x_median_offset_scale = 0")$H1, 1000)
    ) %>% 
  mutate(
    y = (exp(b * x) - 1) * 100,
    x = x * sd(d_GPDD$x_median_offset)
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
    data = d_GPDD %>%
      group_by(ID) %>%
      mutate(
        y = (pop_freq / mean(pop_freq) - 1) * 100
      ),
    aes(x = x_median_offset, y = y, group = ID),
    alpha = 0.1,
    color = "steelblue",
    linewidth = 0.5
  ) +
  geom_ribbon(
    aes(ymax = upper__, ymin = lower__),
    alpha = 0.2
  ) + 
  geom_line(
    linewidth = 1
  ) + 
  annotation_custom(grid::textGrob("Population density", 0.025, 0.95, just = "left", 
                                   gp = grid::gpar(fontsize = 8))) +  
  labs(x = "Years from series center", y = "% change in frequency") + 
  theme_bw(base_size = 10) -> g0; g0



hypothesis_draws(mast_m1, mast_m2, mast_m3, mast_m4, 
                 hypothesis = "x_median_offset_scale = 0") %>% 
  mutate(
    x = case_when(
      model == "mast_m1" ~ "Overall",
      model == "mast_m2" ~ "Periodic",
      model == "mast_m3" ~ "No~missing",
      model == "mast_m4" ~ as.character(vmisc::tex("Length $\\geq 20$"))
    )
  ) %>% 
  ggplot(aes(x = forcats::fct_reorder(x, order(model, decreasing = T)), 
             y = (exp(
               H1 / sd(d_mast$x_median_offset) * 10
             ) - 1) * 100
  )) +
  geom_hline(aes(yintercept = 0), linewidth = 0.6, linetype = 2, color = "grey") + 
  tidybayes::stat_halfeye(
    aes(fill = x == "Periodic", color = x == "Periodic"), 
    stroke = 0.3, 
    interval_size_range = c(0.3, 0.8), 
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("#4682B4CC", "#4682B433")) + 
  scale_color_manual(values = c("black", "grey")) + 
  scale_x_discrete(label = scales::label_parse()) + 
  coord_flip() + 
  labs(x = "", y = "% change in frequency / decade") + 
  theme_bw(base_size = 10) + 
  geom_text(
    data = data.frame(
      "x" = c("Overall",
              "Periodic",
              "No~missing",
              as.character(vmisc::tex("Length $\\geq 20$"))), 
      "y" = 6,
      "label" = c(nobs(mast_m1)/2, nobs(mast_m2)/2, nobs(mast_m3)/2, nobs(mast_m4)/2),
      model = "foo"
    ),
    aes(x = x, y = y, label = label), size = 2.5, hjust = 1
  ) + 
  theme(
    axis.text.y = element_text(angle = 45, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_text(hjust = 1)
  ) -> g2; g2


prepare_newdata(mast_m1, "x_median_offset_scale", n = 10)$x_median_offset_scale %>% 
  expand.grid(
    "x" = .,
    "b" = sample(hypothesis_draws(mast_m1,hypothesis = "x_median_offset_scale = 0")$H1, 1000)
  ) %>% 
  mutate(
    y = (exp(b * x) - 1) * 100,
    x = x * sd(d_mast$x_median_offset)
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
    data = d_mast %>%
      group_by(ID) %>%
      mutate(
        y = (pop_freq / mean(pop_freq) - 1) * 100
      ),
    aes(x = x_median_offset, y = y, group = ID),
    alpha = 0.1,
    color = "steelblue",
    linewidth = 0.5
  ) +
  geom_ribbon(
    aes(ymax = upper__, ymin = lower__),
    alpha = 0.2
  ) + 
  geom_line(
    linewidth = 1
  ) + 
  annotation_custom(grid::textGrob("Masting density", 0.025, 0.95, just = "left", 
                                   gp = grid::gpar(fontsize = 8))) +  
  labs(x = "Years from series center", y = "% change in frequency") + 
  theme_bw(base_size = 10) -> g3; g3

library(patchwork)


g0 + theme(axis.title.x = element_blank()) + 
  g1 + theme(axis.title.x = element_blank()) + 
  g3 + g2 + 
  patchwork::plot_layout(
    design = "
    AAAABB
    AAAABB
    CCCCDD
    CCCCDD
    "
  ) + 
  plot_annotation(tag_levels = 'A') & 
  theme(
    plot.tag.position = c(0.09, 0.98)
  ) -> g_final; g_final


ggsave("graphs/trend_figure.pdf", g_final, dpi = 600, width = 5, height = 4, bg = "white")
ggsave("graphs/trend_figure.png", g_final, dpi = 600, width = 5, height = 4, bg = "white")