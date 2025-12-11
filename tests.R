k <- 1
list(
  "x" = seq(1, 1000, by = 1), 
  "y" = sin(seq(1, 1000, by = 1) / k)
) %>% 
  rand_drop(p = 0.1) %>% 
  power_spec()%>% 
  with(
    {
      plot(y = power, x = freq)
    }
  ); lines(x = rep(1/(2 * pi * k), 2), y = c(0, 1000000) )