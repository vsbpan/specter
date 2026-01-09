source("get_data/data_prep.R")

d_GPDD %>% 
  dplyr::select(ID, lat, lon) %>% 
  unique() %>% 
  filter(!(lat == 0 & lon == 0)) %>% 
  mutate(
    lat = bin(lat, 2),
    lon = bin(lon, 2)
  ) %>% 
  ggplot() + 
  geom_polygon(data = map_data("world"), 
             aes(x=long, y = lat, group = group), 
             alpha = 1,
             linewidth = 0.1,
             color = "grey85",
             fill = "grey90") +
  scale_size_continuous(range = c(1,3)) + 
  #coord_fixed(1.3) + 
  geom_count(aes(y = lat, x = lon), 
             stroke = 0,
             color = "steelblue",
             alpha = 0.7, show.legend = TRUE) +
  theme_light(base_size = 10) + 
  labs(
    x = "Longitude (degree)",
    y = "Latitude (degree)",
    size = "Sample size",
    tag = "A"
  ) + 
  theme(
    legend.title.position = "left",
    legend.justification = "left", 
    legend.box.just = "left", 
    legend.margin = margin(l = -0.05, b = -0.1, unit = "npc"),
    legend.position = "top",
    legend.background = element_blank(), 
    legend.key = element_blank(),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.03, "npc"),
    legend.key.width = unit(0.02, "npc"),
    legend.key.height = unit(0.03, "npc")
  ) + 
  annotation_custom(grid::textGrob("GPDD", 0.025, 0.1, just = "left",gp = grid::gpar(fontsize = 8))) + 
  scale_x_continuous(breaks = seq(-180,180, by = 60)) + 
  scale_y_continuous(breaks = seq(-90,90, by = 45)) -> g1;g1

d_mast %>% 
  dplyr::select(ID, lat, lon) %>% 
  unique() %>% 
  mutate(
    lat = bin(lat, 2),
    lon = bin(lon, 2)
  ) %>% 
  ggplot() + 
  geom_polygon(data = map_data("world"), 
               aes(x=long, y = lat, group = group), 
               alpha = 1,
               linewidth = 0.1,
               color = "grey85",
               fill = "grey90") +
  scale_size_continuous(range = c(1,3)) + 
  #coord_fixed(1.3) + 
  geom_count(aes(y = lat, x = lon), 
             stroke = 0,
             color = "steelblue",
             alpha = 0.7, show.legend = TRUE) +
  theme_light(base_size = 10) + 
  labs(
    x = "Longitude (degree)",
    y = "Latitude (degree)",
    size = "Sample size",
    tag = "D"
  ) + 
  theme(
    legend.title.position = "left",
    legend.justification = "left", 
    legend.box.just = "left", 
    legend.margin = margin(l = -0.05, b = -0.1, unit = "npc"),
    legend.position = "top",
    legend.background = element_blank(), 
    legend.key = element_blank(),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.03, "npc"),
    legend.key.width = unit(0.02, "npc"),
    legend.key.height = unit(0.03, "npc")
  ) + 
  annotation_custom(grid::textGrob("MASTREE+", 0.025, 0.1, just = "left", gp = grid::gpar(fontsize = 8))) + 
  scale_x_continuous(breaks = seq(-180,180, by = 60)) + 
  scale_y_continuous(breaks = seq(-90,90, by = 45))  -> g2;g2

g1.1 <- d_GPDD %>% 
  dplyr::select(ID,whole_x_min, whole_x_max) %>% 
  mutate(
    foo = rnorm(nrow(.))
  ) %>% 
  unique() %>% 
  ggplot(aes(x = forcats::fct_reorder2(as.factor(ID), -whole_x_max, whole_x_min, .desc = TRUE))) + 
  geom_segment(
    aes(y = whole_x_min, yend = whole_x_max, group = ID),
    color = "steelblue",
    linewidth = 0.1
  ) + 
  scale_x_discrete(breaks = NULL) + 
  coord_flip() + 
  annotation_custom(grid::textGrob("GPDD", 0.025, 0.1, just = "left",gp = grid::gpar(fontsize = 8))) + 
  labs(y = "Year", tag = "B") + 
  theme_bw(base_size = 10) + 
  theme(
    axis.title.y = element_blank()
  ) ; g1.1

g2.1 <- d_mast %>% 
  dplyr::select(ID,whole_x_min, whole_x_max) %>% 
  mutate(
    foo = rnorm(nrow(.))
  ) %>% 
  unique() %>% 
  ggplot(aes(x = forcats::fct_reorder2(as.factor(ID), -whole_x_max, whole_x_min, .desc = TRUE))) + 
  geom_segment(
    aes(y = whole_x_min, yend = whole_x_max, group = ID),
    color = "steelblue",
    linewidth = 0.1
  ) + 
  scale_x_discrete(breaks = NULL) + 
  coord_flip() + 
  annotation_custom(grid::textGrob("MASTREE+", 0.025, 0.1, just = "left",gp = grid::gpar(fontsize = 8))) + 
  labs(y = "Year", tag = "E") + 
  theme_bw(base_size = 10) + 
  theme(
    axis.title.y = element_blank()
  ) ; g2.1


ggtree::ggtree(tre_GPDD, aes(color = group, size = 1), layout = "circular") +
  scale_size_continuous(range = c(0.2, 0.2), guide = FALSE) + 
  ggtree::geom_tippoint(aes(color = group), size = 0.2) +
  scale_color_brewer(type = "qual") + 
  theme(
    plot.margin = margin(t = -0.1, r = -0.05, b = -0.1, l = -0.2, unit = "npc"),
    legend.position = c(1.03,0.5),
    legend.background = element_blank()
  ) + 
  theme_bw(base_size = 9) + 
  scale_x_continuous(expand = c(0,0)) +
  guides(color = guide_legend(override.aes = list(linewidth = 1, size = 2))) + 
  theme(
    #plot.margin = margin(t = -0.1, r = -0.1, b = -0.1, l = -1, unit = "npc"),
    legend.background = element_blank(),
    legend.key.spacing = unit(0.001,"npc"),
    legend.margin = margin(),
    legend.key = element_blank(),
    legend.box.background = element_blank(),
    legend.frame = element_blank(),
    axis.line.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_blank(),
    panel.border = element_blank(), 
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(), 
    legend.position = c(1.18,0.5),
    legend.key.height = unit(0.015, "npc")
  ) + 
  labs(color = "Class", tag = "C") -> g1.2;g1.2

ggtree::ggtree(tre_mast, aes(color = group, size = 1), layout = "circular") +
  scale_size_continuous(range = c(0.2, 0.2), guide = FALSE) + 
  ggtree::geom_tippoint(aes(color = group), size = 0.2) +
  scale_color_manual(
    values = unname(pals::alphabet())
  ) + 
  theme_bw(base_size = 9) + 
  scale_x_continuous(expand = c(0, 0)) + 
  guides(color = guide_legend(override.aes = list(linewidth = 1, size = 2))) + 
  theme(
    #plot.margin = margin(t = -0.1, r = -0.1, b = -0.1, l = -1, unit = "npc"),
    legend.background = element_blank(),
    legend.key.spacing = unit(0.001,"npc"),
    legend.margin = margin(),
    legend.key = element_blank(),
    legend.box.background = element_blank(),
    legend.frame = element_blank(),
    axis.line.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.background = element_blank(),
    panel.border = element_blank(), 
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(), 
    legend.key.height = unit(0.015, "npc"),
    plot.tag = element_text(size = 14),
    legend.position = c(1.1,0.5)
  ) + 
  labs(color = "Order", tag = "F") -> g2.2;g2.2


library(patchwork)

g1 + theme(axis.title.x = element_blank(), 
           legend.margin = margin(l = 0, b = -0.01, t = 0, unit = "npc")) + 
  g1.1 + theme(axis.title.x = element_blank(),
               legend.margin = margin(l = 0, b = -0.01, t = 0, r = -0.1, unit = "npc")) + 
  g2 + theme(legend.margin = margin(l = 0, b = -0.01, t = 0, unit = "npc")) + 
  g2.1 + theme(legend.margin = margin(l = 0, b = -0.01, t = 0, r = -0.1, unit = "npc")) + 
  patchwork::plot_layout(
    design = "
    AAABB
    AAABB
    CCCDD
    CCCDD
    "
  ) &
  theme(
    plot.tag.position = c(0.03, 0.96)
  ) -> g_final; g_final

ggpubr::ggarrange(
  g_final, 
  ((g1.2 + theme(plot.tag.position = c(0.05, 0.9))) / 
     (g2.2 + theme(plot.tag.position = c(0.05, 0.96)))) & theme(
    plot.margin = margin(t = 0, r = -0.1, b = 0, l = -0.41, unit = "npc"),
    panel.border = element_blank(),
    plot.tag = element_text(size = 14)
  ),
  widths = c(3, 2.1)
) -> g_final2;g_final2



ggsave(
  "graphs/sampling_figure.pdf", g_final2, dpi = 600, width = 8, height = 4.4, bg = "white"
)
ggsave(
  "graphs/sampling_figure.png", g_final2, dpi = 600, width = 8, height = 4.4, bg = "white"
)

