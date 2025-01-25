# comparison of icp and hh xrf
# the icp consistently underestimates
# because aqua regia digests didn't work on the silicates

full_join(
  hhxrf %>%
    select(top, bot, any_of(elementsList)) %>%
    pivot_longer(!c("top", "bot")) %>%
    mutate(method = "hhxrf"),
  icp %>%
    select(top, bot, any_of(elementsList)) %>%
    pivot_longer(!c("top", "bot")) %>%
    mutate(method = "icp")
  ) %>%
  filter(name == "Ca") %>%
  select(-name) %>%

  pivot_wider(id_cols = c(top, bot),
              names_from = method,
              values_from = value) %>%
  
  glimpse() %>%
  
  ggplot(aes(x = hhxrf, y = icp)) + 
  geom_point() + 
  geom_abline (slope=1, linetype = "dashed", color="Red") +
  coord_fixed()

# comparison of hhxrf and benchtop xrf
# the benchtop xrf has some weirdly low results, probably due to detector fail
# in general it has higher results
full_join(
  hhxrf %>%
    select(top, bot, any_of(elementsList)) %>%
    pivot_longer(!c("top", "bot")) %>%
    mutate(method = "hh"),
  benchxrf %>%
    select(top, bot, any_of(elementsList)) %>%
    pivot_longer(!c("top", "bot")) %>%
    mutate(method = "bench")
) %>%
  filter(name == "Ca") %>%
  select(-name) %>%
  
  pivot_wider(id_cols = c(top, bot),
              names_from = method,
              values_from = value) %>%
  
  glimpse() %>%
  
  ggplot(aes(x = hh, y = bench)) + 
  geom_point() + 
  geom_abline (slope=0.0001, linetype = "dashed", color="Red") 
