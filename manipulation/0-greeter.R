# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md")
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./manipulation/stitched-output/0-greeter.md", )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# import custom functions and scripts
base::source("./scripts/common-functions.R") # generic toolkit
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
library(magrittr)          # pipes
library(ggplot2)           # graphing

# other packages used in the script
requireNamespace("dplyr")    # data wrangling
requireNamespace("readxl")   # import from Excel
# ---- declare-globals ---------------------------------------------------------
# paths to the data sources
# path_file_input_sentences  <- "./data-unshared/raw/2017-10-04  AllSentences_updated_small.xlsx" # for testing
# path_file_input_inmateDB   <- "./data-unshared/raw/inmateDB_small.xlsx"                         # for testing
path_file_input_sentences       <- "./data-unshared/raw/2017-10-04  AllSentences_updated.xlsx"
path_file_input_inmateDB   <- "./data-unshared/raw/inmateDB.xlsx"
# ---- load-data ---------------------------------------------------------------
# source 1 : Data from Nebraska Dept of Corrections
ds_sentence <- readxl::read_excel(path_file_input_sentences, sheet = "AllSentences")
ds_codebook     <- readxl::read_excel(path_file_input_sentences, sheet = "Codebook", n_max = 50) 

# source 2 : Public Inmate database 
ds_inmate   <- readxl::read_excel(path_file_input_inmateDB) 

# ---- inspect-data ---------------------------------------------------------------
# what are the basic specs (size, nrow, ncol) of these objects?
ds_sentence %>% pryr::object_size(); ds_sentence %>% dim()
ds_codebook     %>% pryr::object_size(); ds_codebook%>% dim()
ds_inmate   %>% pryr::object_size(); ds_inmate%>% dim()

# ---- tweak-1 -----------------------------------------------------
# Tweak SENTENCES
ds_sentence %>% dplyr::glimpse(100)
# replace code values for date variables
ds_sentence[ds_sentence == "1800-01-01"] <- NA # '1800-01-01' indicates conversion error

# remove illegal characters in variable names
colnames(ds_sentence) <- gsub(" " ,"_",colnames(ds_sentence)) %>% tolower()
colnames(ds_sentence) <- gsub("__","_",colnames(ds_sentence)) # remove doubles

ds_sentence <- ds_sentence %>% 
  # create inambiguous label for unique individual identifier
  dplyr::rename_(
    "person_id" = "idnumbercomb" # manually checked for douplicates
  ) %>% 
  dplyr::mutate(
    conviction_id  = paste0(person_id,"-",offense_arrest_cd) # to discern multiple convictions on the same date
    ,year          = lubridate::year(begin_date)     # for aggregation and graphing
    ,month         = lubridate::month(begin_date)    # for aggregation and graphing 
    ,offense_group = substr(offense_arrest_cd, 1, 1) # for aggregation and graphing
  )
ds_sentence %>% dplyr::glimpse(100)


# ---- tweak-2 -----------------------------------------------------
# Tweak CODEBOOK for sentences
ds_codebook %>% glimpse(80)
# remove illegal characters in variable names
colnames(ds_codebook) <- gsub(" " ,"_",colnames(ds_codebook)) %>% tolower()
colnames(ds_codebook) <- gsub("__","_",colnames(ds_codebook)) # remove doubles
ds_codebook <- ds_codebook %>% 
  dplyr::mutate(
    field_name  = tolower( gsub(" " ,"_",field_name) ) # to be consistent with `ds_sentence`
    ,field_name =          gsub("__","_",field_name)   # to be consistent with `ds_sentence`
  ) 
ds_codebook %>% glimpse(80)

ds_codebook %>% neat()

# ---- tweak-3 -----------------------------------------------------
# Tweak INMATE DATABASE 
ds_inmate %>% glimpse(80)
# remove illegal characters in variable names
colnames(ds_inmate) <- gsub(" " ,"_",colnames(ds_inmate)) %>% tolower()      # remove spaces between words
colnames(ds_inmate) <- gsub("__","_",colnames(ds_inmate))                    # remove doubles

colnames(ds_inmate) <- gsub("['(']\\month&year[')']","",colnames(ds_inmate)) # remove special characters
colnames(ds_inmate) <- gsub("/","_",colnames(ds_inmate))                     # remove special characters
colnames(ds_inmate) <- gsub("...\\d{1}$","",colnames(ds_inmate))              # remove debree from auto-rename

# colnames(ds_inmate) <- gsub(" $","",colnames(ds_inmate))                   # remove trailing space
ds_inmate %>% glimpse(80)

# ---- basic-graph -------------------------------------------------------


# ---- define-utility-functions ---------------

# ---- basic-pivot-prep ----------------------------

a <- ds_sentence %>% 
  dplyr::group_by(person_id) %>% # for each person
  dplyr::summarize(
    n_convictions = length(unique(conviction_id)) # count how many convictions s/he has
  ) %>% 
  dplyr::ungroup() 

b <- a %>% 
  dplyr::group_by(n_convictions) %>% # for each number of convictions
  dplyr::summarize(
    n_people = length(unique(person_id)) # count how many people had this many convictions
  )

sample_from_n_conviction <- function(
  d
  ,sample_size
  ,n_conv
){
  
  a <- d %>% 
    dplyr::group_by(person_id) %>% # for each person
    dplyr::summarize(
      n_convictions = length(unique(conviction_id)) # count how many convictions s/he has
    ) %>% 
    dplyr::ungroup() 
  
  b <- a %>% 
    dplyr::group_by(n_convictions) %>% # for each number of convictions
    dplyr::summarize(
      n_people = length(unique(person_id)) # count how many people had this many convictions
    )
  
  persons_with_n_convictions <- a %>% 
    filter(n_convictions == n_conv) %>% 
    dplyr::distinct(person_id) %>% na.omit() %>% 
    as.list() %>% unlist() %>% as.vector() 
  
  sample_of_persons_with_n_convictions <- persons_with_n_convictions %>% 
    sample(size = sample_size, replace = FALSE )
}

set.seed(42)
sample_id_2 <- ds_sentence %>% sample_from_n_conviction(sample_size = 6,n_conv = 2)
sample_id_3 <- ds_sentence %>% sample_from_n_conviction(sample_size = 3,n_conv = 3)
sample_id_4 <- ds_sentence %>% sample_from_n_conviction(sample_size = 4,n_conv = 4)
sample_id_6 <- ds_sentence %>% sample_from_n_conviction(sample_size = 1,n_conv = 6)
sample_id_7 <- ds_sentence %>% sample_from_n_conviction(sample_size = 1,n_conv = 6)

ds_sentence_sample <- ds_sentence %>% 
  dplyr::filter(person_id %in% c(
     sample_id_2
    ,sample_id_3
    ,sample_id_4
    ,sample_id_6
    ,sample_id_7
  ))

get_a_sample <- function(
  d,
  varname            # unique of these
  ,sample_size
  ,show_all = FALSE
){
  # varname = "offense_arrest_cd"
  sample_pool <- d %>% 
    dplyr::distinct_(.dots = varname) %>% na.omit() %>% 
    as.list() %>% unlist() %>% as.vector() 
  if(show_all){ sample_size = length(sample_pool)}
  selected_sample <- sample_pool %>% sample(size = sample_size, replace = FALSE )
  
  return(selected_sample)
}  

ds_inmate_sample <- ds_inmate %>% get_a_sample("id_number", sample_size = 20)
# ---- basic-pivot-print ----------------------------

# print the pivots
ds_sentence_sample %>% neat_DT(caption = "Sample records from `Sentences` data set")
ds_codebook %>% neat_DT(caption = "Codebook for `Sentences` data set" )
ds_inmate_sample %>% neat_DT(caption = "Sample records from `Inmate` data set")

# ---- save-to-disk ----------------------------
dto <- list(
  "sentence"  = ds_sentence # each row is a conviction, multiple rows per person
  ,"codebook" = ds_codebook     # each row is a variable in `ds_sentence`
  ,"inmate"   = ds_inmate   # 
)

dto %>% pryr::object_size()
dto %>% saveRDS("./data-unshared/derived/0-dto.rds") # for piping further
ds_codebook %>% readr::write_csv("./data-public/derived/all-sentences-codebook.csv") # for read-only inspection

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



