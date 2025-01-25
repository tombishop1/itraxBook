benchxrf <- read.csv("calibration_samples/CD166_19_benchtop.csv") %>%
  mutate(across(everything(), ~na_if(., "ND"))) %>%
  mutate(`X.1` = lubridate::mdy_hm(`X.1`)) %>%
  mutate(top = str_split_i(X, pattern = "-", 1) %>% as.numeric() * 10,
         bot = str_split_i(X, pattern = "-", 2) %>% as.numeric() * 10
         ) %>%
  select(-X) %>%
  rename(date = X.1) %>%
  select(top, bot, date, everything()) %>%
  janitor::remove_empty(which = "cols", cutoff = 0.5) %>%
  filter(!is.na(top)) %>%
  mutate(across(any_of(elementsList), as.numeric)) %>%
  arrange(top)

# three samples are missing. 
length(hhxrf$bot[!hhxrf$bot %in% benchXRF$bot])

# correct for water content
waterContent <- read_csv("calibration_samples/calibration_data.csv") %>%
  mutate(water_content = (((wet-tare)-(dry-tare))/(wet-tare))*100) %>%
  select(top, bot, water_content) %>%
  mutate(top = as.numeric(top)) %>%
  drop_na()

benchXRF <- left_join(benchXRF, waterContent, by = c("top", "bot")) %>%
  mutate(across(any_of(elementsList), function(x){x*(1-water_content/100)})) %>%
  select(-water_content)

rm(waterContent)
