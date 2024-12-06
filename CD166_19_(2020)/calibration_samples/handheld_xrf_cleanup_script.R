openxlsx::read.xlsx("calibration_samples/CD166_hh.xls.xlsx") %>%
  select(SAMPLE, any_of(elementsList)) %>%
  glimpse()

openxlsx::read.xlsx("calibration_samples/CD166_hh.xls.xlsx") %>%
  select(SAMPLE, contains("Error")) %>%
  rename_with(~ str_replace(string = .x, pattern = ".Error", replacement = "")) %>%
  glimpse()
