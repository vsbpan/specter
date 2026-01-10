
# Rick Karban's LTREB caterpillar data
d <- data.frame(year = 1986:2019, 
                y = c(1.28, 0.04, 0.39, 0.53, 0.78, 0.19, 1.87, 0.17, 0.28, 0.8, 2.72, 0.1, 0.65, 0.76, 0.21, 1.35, 0.32, 1.37, 0.04, 0.004, 0.01, 1.91, 1.44, 0.13, 0.06, 1.08, 1.82, 0.05, 0.23, 0.11, 0.53, 0.48, 0.85, 5.62))

z <- with(d, {
  series_make(year, y, "Karban_LTREB")
}) %>% 
  series_split() %>% 
  lapply(power_spec) %>% 
  lapply(function(x){
    bind_attributes(x, with(x, {find_spectrum_indices(freq, power, y)})) %>% 
      bind_attributes(with(x, {find_years_vars(x)}))
  })

d2 <- cbind(
  do.call("data.frame",z$p1[c("power", "freq")]),
  "part" = "part 1"
) %>% 
  mutate(
    power = power / var(z$p1$y) / 2 
  ) %>% 
  rbind(
    cbind(
      do.call("data.frame",z$p2[c("power", "freq")]),
      "part" = "part 2"
    ) %>% 
      mutate(
        power = power / var(z$p2$y) / 2 
      )
  )


g1 <- d %>% 
  mutate(
    year = year - 1985,
    part = !year <= 34/2
  ) %>% 
  ggplot(aes(x = year, y = y)) + 
  geom_line(
    linewidth = 1,
    show.legend = FALSE
  ) + 
  geom_point(aes(color = part), 
             show.legend = F, 
             size = 2) + 
  geom_vline(
    aes(xintercept = median(year)),
    linetype = 2,
    linewidth = 0.8, 
    color = "grey"
  ) + 
  theme_bw(base_size = 8) + 
  scale_y_log10() + 
  scale_x_continuous(
    breaks = c(1, 8.5, 17,18,34/4*3, 34),
    label = c(
      "1","0.5N", "N", "N+1","1.5N", "2N"
    )
  ) + 
  scale_color_discrete(type = c("steelblue", "violetred")) + 
  labs(x = "Series index (years since beginning)", y = "Population density"); g1

g2 <- d2 %>% 
  ggplot(aes(x = freq, y = power)) + 
  geom_line(
    linewidth = 1,
    aes(color = part),
    show.legend = FALSE
  ) + 
  theme_bw(base_size = 8) + 
  scale_x_continuous(
    breaks = c(
      1/length(z$p1$x), 0.5
    ),
    label = c(
      vmisc::tex("$1/N$"), "0.5"
    ),
    limits = c(1/length(z$p1$x), 0.5)
  ) + 
  scale_color_discrete(type = c("steelblue", "violetred")) + 
  scale_y_log10() + 
  facet_wrap(~part) + 
  theme(strip.background = element_blank(),
        strip.text = element_blank()) + 
  labs(x = tex("Frequency, $\\textit{f}$"), y = tex("Power, $\\textit{\\hat{S}(f)}$")); g2

library(patchwork)

d3 <- lapply(z, collect_attributes) %>% 
  do.call("rbind", .)

d3 %>% 
  mutate(
    x_median_mean = mean(x_median),
    x_median_offset = x_median - x_median_mean
  ) %>% 
  ggplot(aes(x = x_median_offset, y = mean_freq)) + 
  geom_line(
    linewidth = 1
  ) + 
  geom_point(size = 2, aes(color = factor(part)), show.legend = FALSE) +
  scale_color_discrete(type = c("steelblue", "violetred")) + 
  scale_x_continuous(
    breaks = c(-z$p1$attributes$x_length/2,
               0,
               z$p1$attributes$x_length/2),
    labels = c("-0.5N","0","0.5N")
  ) + 
  scale_y_log10(
    breaks = c(1/z$p1$attributes$x_length,0.1, 0.3,0.5),
    labels = c(
      "1/N",0.1, 0.3,0.5
    ),
    limit = c(1/z$p1$attributes$x_length,0.5)
  ) + 
  labs(x = "Years from series center", y = tex("\\overset{Mean frequency, $\\textit{\\langle f \\rangle}$}{($cycles / year$)}")) + 
  theme_bw(base_size = 8) -> g4; g4

g_final <- g1 / g2 / (patchwork::plot_spacer() + g4)
g_final

ggsave("graphs/concept_figure.png",g_final, dpi = 600, bg = "white", width = 5.5, height = 4)

