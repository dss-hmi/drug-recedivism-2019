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
ds_codebook %>% pryr::object_size(); ds_codebook%>% dim()
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
  ) %>% 
  dplyr::arrange(person_id, begin_date, offense_arrest_cd)
ds_sentence %>% dplyr::glimpse(100)


# ---- tweak-2 -----------------------------------------------------
# Tweak CODEBOOK for sentences
ds_codebook %>% dplyr::glimpse(80)
# remove illegal characters in variable names
colnames(ds_codebook) <- gsub(" " ,"_",colnames(ds_codebook)) %>% tolower()
colnames(ds_codebook) <- gsub("__","_",colnames(ds_codebook)) # remove doubles
ds_codebook <- ds_codebook %>% 
  dplyr::mutate(
    field_name  = tolower( gsub(" " ,"_",field_name) ) # to be consistent with `ds_sentence`
    ,field_name =          gsub("__","_",field_name)   # to be consistent with `ds_sentence`
  ) 
ds_codebook %>% dplyr::glimpse(80)

# ---- tweak-3 -----------------------------------------------------
# Tweak INMATE DATABASE 
ds_inmate %>% dplyr::glimpse(80)
# remove illegal characters in variable names
colnames(ds_inmate) <- gsub(" " ,"_",colnames(ds_inmate)) %>% tolower()      # remove spaces between words
colnames(ds_inmate) <- gsub("__","_",colnames(ds_inmate))                    # remove doubles

colnames(ds_inmate) <- gsub("['(']\\month&year[')']","",colnames(ds_inmate)) # remove special characters
colnames(ds_inmate) <- gsub("/","_",colnames(ds_inmate))                     # remove special characters
colnames(ds_inmate) <- gsub("(...)(\\d{1})$","_\\2",colnames(ds_inmate))              # remove debree from auto-rename

# colnames(ds_inmate) <- gsub(" $","",colnames(ds_inmate))                   # remove trailing space
ds_inmate %>% dplyr::glimpse(80)

# ---- basic-graph -------------------------------------------------------


# ---- define-utility-functions ---------------

# ---- inspect-1 ----------------------------
ds_sentence %>% 
  dplyr::filter(person_id %in% c(46222,65392, 50495) ) %>% # for testing
  dplyr::select(c(
        "person_id"          # idnumbercomb, manually checked to represent a unique person 
        ,"inmate_name"
        ,"begin_date"        # date the person began serving the aggregate sentence
        ,"offense_arrest_cd" # code for the offense committed   
        ,"offense_count"     # count of offenses in inmate's sentence
        ,"offense_arrest"    # standardized description of the offense committed
      )) %>% 
  neat(caption = "Sample records from `Sentences` data set")

# ---- inspect-2 ----------------------------
ds_codebook %>% neat(caption = "Codebook for `Sentences` data set" )

# ---- inspect-3 ----------------------------
ds_inmate %>% 
  dplyr::filter(id_number %in% c(46222,65392, 50495) ) %>% 
  dplyr::select(c(
    "id_number"
    ,"committed_last_name"
    ,"date_of_birth"
    ,"race_desc"
    ,"gender"
    ,"sentence_begin_date"
  )) %>% 
  neat(caption = "Sample records from `Inmate` data set")


# ---- save-to-disk ----------------------------
dto <- list(
  "sentence"  = ds_sentence # each row is a conviction, multiple rows per person
  ,"codebook" = ds_codebook     # each row is a variable in `ds_sentence`
  ,"inmate"   = ds_inmate   # 
)

dto %>% pryr::object_size()
dto %>% saveRDS("./data-unshared/derived/0-dto.rds") # for piping further
ds_codebook %>% readr::write_csv("./data-public/derived/all-sentences-codebook.csv") # for read-only inspection

# see our research jounal at
# https://docs.google.com/document/d/1_EhkXgkBZTJ8nc02rr8Z4wrbzSbvvT6VZoQJi6DAhNQ/edit?usp=sharing

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



