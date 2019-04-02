# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md", figure)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./manipulation/stitched-output/0-greeter.md", )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(dplyr) # disable when temp lines are removed
library(ggplot2)
library(ggpubr)
library(readxl)
# ---- declare-globals ---------------------------------------------------------
# path_file_input_data       <- "./data-unshared/raw/2016-03-25 - AllSentences_small.xlsx"
# path_file_input_data       <- "./data-unshared/raw/2017-10-04  AllSentences_updated_small.xlsx"
path_file_input_data       <- "./data-unshared/raw/2017-10-04  AllSentences_updated.xlsx"
# path_file_input_parameters <- "./data-unshared/derived/saveParams_SYTYCG_Season1.csv"
# ---- load-data ---------------------------------------------------------------
ds      <- readxl::read_excel(path_file_input_data,sheet = "AllSentences" )
meta      <- readxl::read_excel(path_file_input_data,sheet = "Codebook" )

ds %>% dplyr::glimpse(80)

# ---- tweak-data ---------------------------------------------------------------
names(ds)
# d1 <- ds %>% 
#   dplyr::select(.dots = c(
#     "Idnumbercomb"
#     ,"Offense Begin Date"
#     ,"Begin Date"
#     ,"Offense Arrest CD"
#     ,"Offense Arrest"
#   ))
d1 <- ds[c(
  "Idnumbercomb"
  # ,"Offense Begin Date"
  ,"Begin Date"
  ,"Offense Arrest CD" # code
  ,"Offense Arrest"    # description
)]

names(d1) <- c(
  "person_id"
  # , "offense_begin_date"
  , "begin_date"
  , "offense_arrest_cd"
  , "offense_arrest"
)

d1 %>% glimpse()

d2 <- d1 %>% 
  dplyr::mutate(
    conviction_id = paste0(person_id,"-",offense_arrest_cd)
    ,year = lubridate::year(begin_date)
    # ,offense_group = gsub("^(\\w{1}(\\d{2}))$","\\1", offense_arrest_cd)
    ,offense_group = substr(offense_arrest_cd,1,1)
  )

d2 %>% glimpse()
# ---- basic-table ----------------------------

# type of offense total
d2 %>% 
  dplyr::filter(year > 1974) %>% 
  dplyr::group_by(offense_group) %>% 
  dplyr::count() %>% 
  ggplot2::ggplot(aes(x = offense_group, y = n ))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_minimal()

# type of offsen by year
d2 %>% 
  dplyr::filter(year > 1974) %>% 
  dplyr::group_by(offense_group, year) %>% 
  dplyr::count() %>% 
  ggplot2::ggplot(aes(x = year, y = offense_group, fill = n ))+
  geom_raster()+
  # coord_flip()+
  theme_minimal()


# type of drug offense by year
d2 %>% 
  dplyr::filter(year > 1974) %>% 
  dplyr::filter(offense_group == "C") %>% 
  dplyr::group_by(offense_arrest, year) %>% 
  dplyr::count() %>% 
  ggplot2::ggplot(aes(x = year, y = offense_arrest, fill = n ))+
  geom_raster()+
  # coord_flip()+
  theme_minimal()

# see our research jounal https://docs.google.com/document/d/1_EhkXgkBZTJ8nc02rr8Z4wrbzSbvvT6VZoQJi6DAhNQ/edit?usp=sharing



# ---- basic-graph -------------------------------------------------------

  
# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/0-greeter/0-greeter.Rmd"
  ,output_format = c(
    "html_document" 
    # "pdf_document"
    ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
  )

