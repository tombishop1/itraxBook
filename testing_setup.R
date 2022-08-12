# this setups up the workspace as it is laid out in the book... I think!

# load things ----
library(itraxR)
library(tidypaleo)
library(ordr)
library(kableExtra)
library(DiagrammeR)
library(tidyverse)
library(PeriodicTable)
library(egg)
library(errors)
library(forecast)
library(slider)
library(compositions)

# set wording directory ----
setwd("C:/Github/itraxBook/CD166_19_(2020)/")

# load utility data ----
data(periodicTable)
elementsList <- periodicTable$symb
rm(periodicTable)

# load itrax data ----
CD166_19_S1 <- list(metadata   = itrax_meta("CD166_19_S1/CD166_19_S1/document.txt"),
                    xrf        = itrax_import("CD166_19_S1/CD166_19_S1/Results.txt", 
                                              depth = 0, 
                                              parameters = "all"),
                    image      = itrax_image(file = "CD166_19_S1/CD166_19_S1/optical.tif",
                                             meta = "CD166_19_S1/CD166_19_S1/document.txt"),
                    radiograph = itrax_radiograph(file = "CD166_19_S1/CD166_19_S1_RAD/radiograph0.tif",
                                                  meta = "CD166_19_S1/CD166_19_S1_RAD/document.txt",
                                                  trim = as.numeric(itrax_meta("CD166_19_S1/CD166_19_S1/document.txt")[6:7,2])))

CD166_19_S2 <- list(metadata   = itrax_meta("CD166_19_S2/CD166_19_S2/document.txt"),
                    xrf        = itrax_import("CD166_19_S2/CD166_19_S2/Results.txt", 
                                              depth = max(CD166_19_S1$xrf$depth), 
                                              parameters = "all"),
                    image      = itrax_image(file = "CD166_19_S2/CD166_19_S2/optical.tif",
                                             meta = "CD166_19_S2/CD166_19_S2/document.txt"),
                    radiograph = itrax_radiograph(file = "CD166_19_S2/CD166_19_S2_RAD/radiograph0.tif",
                                                  meta = "CD166_19_S2/CD166_19_S2_RAD/document.txt",
                                                  trim = as.numeric(itrax_meta("CD166_19_S2/CD166_19_S2/document.txt")[6:7,2])))

CD166_19_S3 <- list(metadata   = itrax_meta("CD166_19_S3/CD166_19_S3/document.txt"),
                    xrf        = itrax_import("CD166_19_S3/CD166_19_S3/Results.txt", 
                                              depth = max(CD166_19_S2$xrf$depth), 
                                              parameters = "all"),
                    image      = itrax_image(file = "CD166_19_S3/CD166_19_S3/optical.tif",
                                             meta = "CD166_19_S3/CD166_19_S3/document.txt"),
                    radiograph = itrax_radiograph(file = "CD166_19_S3/CD166_19_S3_RAD/radiograph0.tif",
                                                  meta = "CD166_19_S3/CD166_19_S3_RAD/document.txt",
                                                  trim = as.numeric(itrax_meta("CD166_19_S3/CD166_19_S3/document.txt")[6:7,2])))

CD166_19_xrf <- itrax_join(list(S1 = CD166_19_S1$xrf, S2 = CD166_19_S2$xrf, S3 = CD166_19_S3$xrf))

# do qc ----
CD166_19_xrf <- CD166_19_xrf %>%
  mutate(slope = `sample surface` - dplyr::lag(`sample surface`)) %>%
  mutate(in_slope_tolerance = ifelse(slope <=-0.1 | slope >=0.1 | is.na(slope) == TRUE, FALSE, TRUE)) %>%
  select(-slope) %>%
  mutate(in_cps_tolerance = ifelse(cps <=30000 | cps >=60000 | is.na(cps) == TRUE, FALSE, TRUE)) %>%
  mutate(in_mse_tolerance = ifelse(MSE <=2, TRUE, FALSE)) %>%
  rowwise() %>%
  mutate(qc = !any(c(validity, in_slope_tolerance, in_cps_tolerance, in_mse_tolerance) == FALSE)) %>%
  ungroup() %>%
  select(-c(in_slope_tolerance, in_cps_tolerance, in_mse_tolerance)) 

# do element selection ----
myElements <- apply(CD166_19_xrf %>% select(any_of(elementsList)), 2, FUN = function(x){round(Acf(x, plot = F)$acf, 3)}) %>%
  as_tibble(rownames = "lag") %>%
  pivot_longer(!c("lag"), names_to = "elements", values_to = "value") %>%
  mutate(lag = as.numeric(lag),
         elements = factor(elements, levels = filter(., lag == 5) %>% arrange(desc(value)) %>% pull(elements))) %>%
  group_by(elements) %>%
  filter(lag == 5) %>%
  filter(value >= 0.7) %>%
  pull(elements) %>% 
  ordered()


# set uid
CD166_19_xrf <- CD166_19_xrf %>%
  mutate(uid = paste0(label, "_", depth))

# do compositional transforms
CD166_19_xrf_acomp <- CD166_19_xrf %>%
  filter(qc == TRUE) %>%
  select(any_of(c(elementsList, "uid"))) %>%
  column_to_rownames("uid") %>%
  mutate(across(everything(), function(x){ifelse(x == 0, -1, x)})) %>%
  acomp()

CD166_19_xrf_acomp_meta <- full_join(CD166_19_xrf_acomp %>% 
                                       as.data.frame() %>%
                                       rownames_to_column("uid"),
                                     CD166_19_xrf %>%
                                       select(-any_of(elementsList)),
                                     by = "uid"
                                     ) %>%
  arrange(depth, label) 

# do calibration ----
# make icp dataset
icp <- read_csv("calibration_samples/calibration_data.csv") %>%
  #select(-`Sample Id`) %>%
  mutate(water_content = (((wet-tare)-(dry-tare))/(wet-tare))*100) %>%
  select(-c("wet", "tare", "dry")) %>%
  # remove negative values
  mutate(across(any_of(elementsList), function(x){replace(x, which(x<0), NA)})) %>%
  # adjust for dilution
  mutate(across(any_of(elementsList), function(x){(x*`Dilution`)/`weight`})) %>%
  # adjust for water content
  mutate(across(any_of(elementsList), function(x){x*(1-icp$water_content/100)})) %>%
  # remove Inf or NaN values
  mutate(across(any_of(elementsList), function(x){replace(x, is.infinite(x) | is.nan(x), NA)})) %>%
  select(-c("weight", "Dilution")) %>%
  filter(!top %in% c("BLANK", "MESS-4")) %>%
  mutate(top = as.numeric(top), bot = as.numeric(bot))

# make obs dataset
xrf <- CD166_19_xrf %>%
  filter(qc == TRUE) %>%
  select(-c(label, filename, uid)) %>%
  itrax_reduce(names = icp$SampleID,
               breaks_lower = icp$top,
               breaks_upper = icp$bot) %>%
  rename(SampleID = resample_names) %>%
  mutate(top = icp$top, 
         bot = icp$bot)

# do comparisons
full_join(
  icp %>%
    select(any_of(c(elementsList, "SampleID"))) %>%
    pivot_longer(any_of(elementsList),
                 values_to = "icp",
                 names_to = "element"),
  
  xrf %>% 
    select(any_of(c(elementsList, "SampleID"))) %>%
    pivot_longer(any_of(elementsList),
                 values_to = "xrf",
                 names_to = "element"),
  
  by = c("SampleID", "element")
  ) %>%

  filter(element %in% myElements) %>%
  
  ggplot(aes(x = icp, y = xrf)) +
  geom_point() +
  #geom_smooth(method='lm') +
  ggpmisc::stat_poly_line() +
  ggpmisc::stat_poly_eq() +
  facet_wrap(vars(element), 
             scales = "free")
