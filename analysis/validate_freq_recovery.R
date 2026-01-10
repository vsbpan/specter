library(tidyverse)
vmisc::load_all2("specter")

k <- 10
N <- 100
A <- 1
list(
  "x" = seq(1, N, by = 1), 
  "y" = sin(seq(1, N, by = 1) / k) * A
) %>% 
  series_rand_drop(p = 0.1, set_as_NA = FALSE) %>% 
  power_spec() %>% 
  with(
    {
      plot(y = power, x = freq)
    }
  ); segments(x0 = 1/(2 * pi * k), x1 = 1/(2 * pi * k), 
              y0 = 0, y1 = A^2 * N / 2)



simulate_one_instance <- function(k, N, p, method = c("lomb", "ndft")){
  if((N - ceiling(N * p)) < 4){
    return(NA_real_)
  }
  
  list(
    "x" = seq(1, N, by = 1), 
    "y" = sin(seq(1, N, by = 1) / k)
  ) %>% 
    series_rand_drop(p = p, set_as_NA = FALSE) %>% 
    power_spec(method = method) %>%
    with(
      pred_error_score(freq, power, 1/(2 * pi * k))
    )
}


d_grid <- expand.grid(
  "k" = 10^seq(-0.5, log10(50 / (2 * pi)), by = 0.05),
  "N" = seq(4,50), 
  "p" = seq(0, 0.3, by = 0.1),
  "rep" = 1:100
)

d_grid %>%
  assign_chunk(n_chunks = 320) %>% 
  group_by(chunk_id) %>% 
  dplyr::group_split() %>% 
  vmisc::pb_par_lapply(
    function(x){
      res <- apply(x, 1, function(v){
        simulate_one_instance(
          k = as.numeric(v["k"]),
          N = as.numeric(v["N"]),
          p = as.numeric(v["p"]), 
          method = "ndft"
        )
      }, simplify = FALSE) %>% 
        do.call("c", .) %>% 
        unname()
      
      cbind(x, "score" = res)
    }, 
    cores = 8, 
    inorder = FALSE
  ) %>% 
  do.call("rbind", .) -> d_res2


# write_csv(d_res2, "cleaned_data/ndft_recovery_simulation.csv")

d_grid %>%
  assign_chunk(n_chunks = 320) %>% 
  group_by(chunk_id) %>% 
  dplyr::group_split() %>% 
  vmisc::pb_par_lapply(
    function(x){
      res <- apply(x, 1, function(v){
        simulate_one_instance(
          k = as.numeric(v["k"]),
          N = as.numeric(v["N"]),
          p = as.numeric(v["p"]), 
          method = "lomb"
        )
      }, simplify = FALSE) %>% 
        do.call("c", .) %>% 
        unname()
      
      cbind(x, "score" = res)
    }, 
    cores = 8, 
    inorder = FALSE
  ) %>% 
  do.call("rbind", .) -> d_res3


# write_csv(d_res3, "cleaned_data/lomb_recovery_simulation.csv")



d_res2 <- read_csv("cleaned_data/ndft_recovery_simulation.csv")
d_res3 <- read_csv("cleaned_data/lomb_recovery_simulation.csv")




d_res2 %>% 
  rename(score_1 = score) %>% 
  dplyr::select(-chunk_id) %>% 
  left_join(
    d_res3 %>% 
      rename(score_2 = score) %>% 
      dplyr::select(-chunk_id)
  ) %>% 
  mutate(
    score = abs(score_1) - abs(score_2) # The higher the score, the worse NDFT is over Lomb
  ) -> d_res4

d_res3 %>% 
  group_by(k, N, p) %>% 
  summarise(
    score = mean(score, na.rm = TRUE)
  ) %>% 
  mutate(
    freq = 1/(2 * pi * k),
    include = ifelse(is.na(score), "No", ifelse(abs(score) < 0.1, "Yes", "No"))
  ) %>% 
  filter(
    k < 50 / (2 * pi)
  ) %>% 
  mutate(
    p2 = sprintf("Missing data: %s%s", p * 100, "%")
  ) %>% 
  ggplot(aes(x = N, y = freq, fill = score)) + 
  scale_y_log10() + 
  geom_tile() + 
  geom_tile(
    aes(color = include),
    show.legend = FALSE
  ) + 
  # geom_line(
  #   aes(
  #     x = N, y = 1 / N
  #   )
  # ) + 
  geom_line(
    aes(
      x = N, y = pmin(0.5, 1 / floor(N * (1 - p)))
    ),
    color = "black"
  ) + 
  geom_line(
    aes(x = N, y = 1/2)
  ) + 
  facet_wrap(~p2) + 
  scale_color_discrete(
    type = c("#00000000","grey50")
  ) + 
  scale_fill_gradient2(low = "#B2182B", 
                       mid = "grey80", 
                       high = "#2166AC", 
                       midpoint = 0) + 
  theme_minimal() + 
  theme(
    strip.background = element_blank()
  )  + 
  labs(x = "Series length", y = "True frequency", 
       fill = "Log ratio")




# Resolution
# min: 1/N
# max: 1/2








