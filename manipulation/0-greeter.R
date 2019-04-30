# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md")
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./manipulation/stitched-output/0-greeter.md", )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  
base::source("./scripts/common-functions.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(dplyr) # disable when temp lines are removed
library(ggplot2)
library(ggpubr)
library(readxl)
# ---- declare-globals ---------------------------------------------------------
# path_file_input_data       <- "./data-unshared/raw/2017-10-04  AllSentences_updated_small.xlsx"
# path_file_input_inmateDB   <- "./data-unshared/raw/inmateDB_small.xlsx"
path_file_input_data       <- "./data-unshared/raw/2017-10-04  AllSentences_updated.xlsx"
path_file_input_inmateDB   <- "./data-unshared/raw/inmateDB.xlsx"
# ---- load-data ---------------------------------------------------------------
# source 1 : Data from Nebraska dept of Corrections
ds_sentence <- readxl::read_excel(path_file_input_data,sheet = "AllSentences" )
ds_meta     <- readxl::read_excel(path_file_input_data,sheet = "Codebook",n_max = 50) 

# source 2 : Public Inmate database 
ds_inmate   <- readxl::read_excel(path_file_input_inmateDB) 

# ---- tweak-data ---------------------------------------------------------------
ds_sentence %>% pryr::object_size(); ds_sentence %>% dim()
ds_meta     %>% pryr::object_size(); ds_meta%>% dim()
ds_inmate   %>% pryr::object_size(); ds_inmate%>% dim()

# ---- tweak-1 -----------------------------------------------------
# Adjust SENTENCES
ds_sentence %>% dplyr::glimpse(100)
colnames(ds_sentence) <- gsub(" " ,"_",colnames(ds_sentence)) %>% tolower()
colnames(ds_sentence) <- gsub("__","_",colnames(ds_sentence)) # remove doubles

ds_sentence <- ds_sentence %>% 
  # create inambiguous label for unique individual identifier
  dplyr::rename_(
    "person_id" = "idnumbercomb" # manually checked for doublicates
  ) %>% 
  dplyr::mutate(
    conviction_id  = paste0(person_id,"-",offense_arrest_cd) # to discern multiple convictions on the same date
    ,year          = lubridate::year(begin_date) # 
    ,month         = lubridate::month(begin_date) # 
    ,offense_group = substr(offense_arrest_cd, 1, 1)
  )
ds_sentence %>% dplyr::glimpse(100)


# ---- tweak-2 -----------------------------------------------------
# Adjust CODEBOOK for sentences
ds_meta %>% glimpse(80)
colnames(ds_meta) <- gsub(" " ,"_",colnames(ds_meta)) %>% tolower()
colnames(ds_meta) <- gsub("__","_",colnames(ds_meta)) # remove doubles
ds_meta <- ds_meta %>% 
  dplyr::mutate(
    field_name  = tolower( gsub(" " ,"_",field_name) ) # to be consistent with `ds_sentence`
    ,field_name =          gsub("__","_",field_name)   # to be consistent with `ds_sentence`
  ) 
ds_meta %>% glimpse(80)

# ---- tweak-3 -----------------------------------------------------
# Adjust INMATE DATABASE 
ds_inmate %>% glimpse(80)
colnames(ds_inmate) <- gsub(" " ,"_",colnames(ds_inmate)) %>% tolower() # remove spaces between words
colnames(ds_inmate) <- gsub("__","_",colnames(ds_inmate))               # remove doubles

colnames(ds_inmate) <- gsub("['(']\\month&year[')']","",colnames(ds_inmate))            # remove special characters
colnames(ds_inmate) <- gsub("/","_",colnames(ds_inmate))          # remove special characters
colnames(ds_inmate) <- gsub("..\\d{1}$","",colnames(ds_inmate))         # remove debree from auto-rename
# colnames(ds_inmate) <- gsub(" $","",colnames(ds_inmate))                # remove trailing space
ds_inmate %>% glimpse(80)

# ---- basic-graph -------------------------------------------------------


# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------
dto <- list(
  "sentence"  = ds_sentence # each row is a conviction, multiple rows per person
  ,"codebook" = ds_meta     # each row is a variable in `ds_sentence`
  ,"inmate"   = ds_inmate   # 
)

dto %>% pryr::object_size()
dto %>% saveRDS("./data-unshared/derived/0-dto.rds")
ds_meta %>% readr::write_csv("./data-public/derived/all-sentences-codebook.csv") # for read-only inspection

# ---- initial-explorations-0 ------------------------
# focus on a few variables
ds <- dto$sentence

d1 <- ds %>%
  dplyr::mutate(
    conviction_id  = paste0(person_id,"-",offense_arrest_cd)
    ,year          = lubridate::year(begin_date)
    ,offense_group = substr(offense_arrest_cd,1,1)
    # ,offense_group = gsub("^(\\w{1})(\\d{2}))$","\\1", offense_arrest_cd)
  ) %>% 
  dplyr::select_(.dots = c(
    "person_id"
    ,"begin_date"
    ,"offense_arrest_cd"
    ,"offense_arrest"
    ,"conviction_id"
    ,"year"
    ,"offense_group"
    )
  )
d1 %>% glimpse(100)


d2 <- d1 %>% 
  dplyr::mutate(
    conviction_id = paste0(person_id,"-",offense_arrest_cd)
    ,year         = lubridate::year(begin_date)
    # ,offense_group = gsub("^(\\w{1})(\\d{2}))$","\\1", offense_arrest_cd)
    ,offense_group = substr(offense_arrest_cd,1,1)
  )

d2 %>% glimpse(100)

d2 %>% group_by(offense_group) %>% count() %>% neat() # rows
d2 %>% group_by(offense_group) %>% summarize(n=length(unique(person_id))) %>% neat() # persons


# ---- initial-explorations-1 ------------------------

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
  # geom_raster()+
  geom_tile(color = "grey80")+
  scale_fill_gradient2( high = "red")+
  theme_minimal()

# ---- initial-explorations-2 ------------------------

# type of drug offense by year
d2 %>% 
  dplyr::filter(year > 1974) %>% 
  dplyr::filter(offense_group == "C") %>% 
  dplyr::group_by(offense_arrest, year) %>% 
  dplyr::count() %>% 
  ggplot2::ggplot(aes(x = year, y = offense_arrest, fill = n ))+
  geom_tile(color = "grey80")+
  scale_fill_gradient2( high = "blue")+
  # scale_fill_gradient2( high = "red")+
  theme_minimal()

# see our research jounal https://docs.google.com/document/d/1_EhkXgkBZTJ8nc02rr8Z4wrbzSbvvT6VZoQJi6DAhNQ/edit?usp=sharing

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/0-greeter/0-greeter.Rmd"
  ,output_format = c(
    "html_document" 
    # ,"pdf_document"
    # ,"md_document"
    # "word_document" 
  )
  ,clean=TRUE
)



