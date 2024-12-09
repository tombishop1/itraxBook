openxlsx::read.xlsx("calibration_samples/CD166_hh.xls.xlsx") %>%
  select(SAMPLE, any_of(elementsList)) %>%
  mutate(across(where(is.character), ~na_if(., "<LOD"))) %>%
  mutate(across(any_of(elementsList), ~as.numeric(.))) %>%
  glimpse() -> values

openxlsx::read.xlsx("calibration_samples/CD166_hh.xls.xlsx") %>%
  select(SAMPLE, contains("Error")) %>%
  rename_with(~ str_replace(string = .x, pattern = ".Error", replacement = "")) %>%
  select(SAMPLE, any_of(elementsList)) %>%
  glimpse() -> errors

for(i in colnames(values)[-1]){
  values[i] <- 
    set_errors(eval(parse(text = paste0("values$",i))), 
               value = eval(parse(text = paste0("errors$",i)))
    )
}

hhxrf <- values
rm(errors)
rm(i)
rm(values)

hhxrf <- hhxrf %>%
  mutate(top = as.numeric(str_replace(SAMPLE, "cd166-", ""))) %>%
  mutate(top = top * 10) %>%
  mutate(bot = top + 10) %>%
  rename(SampleID = SAMPLE) %>%
  select(top, bot, SampleID, everything()) 

hhxrf <- hhxrf %>%
  janitor::remove_empty(which = "cols")

waterContent <- read_csv("calibration_samples/calibration_data.csv") %>%
  mutate(water_content = (((wet-tare)-(dry-tare))/(wet-tare))*100) %>%
  select(top, bot, water_content) %>%
  mutate(top = as.numeric(top)) %>%
  drop_na()

hhxrf <- left_join(hhxrf, waterContent, by = c("top", "bot"))

rm(waterContent)

hhxrf <- hhxrf %>%
  mutate(across(any_of(elementsList), function(x){x*(1-water_content/100)})) %>%
  select(-water_content)

save(hhxrf, file = "calibration_samples/CD166_hhxrf.RData")



full_join(
  hhxrf %>%
    select(any_of(c(elementsList, "top"))) %>%
    rename(depth = top) %>%
    pivot_longer(any_of(elementsList),
                 values_to = "icp",
                 names_to = "element"),
  
  xrf %>% 
    select(any_of(c(elementsList, "depth"))) %>%
    mutate(depth = plyr::round_any(depth, 10, f = floor)) %>%
    pivot_longer(any_of(elementsList),
                 values_to = "xrf",
                 names_to = "element"),
  
  by = c("depth", "element")
) %>%
  
  #filter(element %in% myElements) %>%
  drop_na() %>%
  
  ggplot(aes(x = icp, y = xrf)) +
  geom_point() +
  ggpmisc::stat_poly_line() +
  ggpmisc::stat_poly_eq() +
  facet_wrap(vars(element), 
             scales = "free") +
  xlab("Handheld XRF [ppm]") + 
  ylab("Itrax ED-XRF [peak area]")
