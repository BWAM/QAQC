#Extract -----------------------------------------------------------------

  sections.path <- "sections"
  extracted.path <- c("sections")
  
  extract_code <- function(rmd.path, extracted.path) {
    r.files.vec <- list.files(rmd.path)
    r.files.vec <- r.files.vec[grepl(".Rmd", r.files.vec)]
    
    purrr::map(r.files.vec, function(file.i) {
      file.name <- gsub(".Rmd", "", file.i)
      extracted.file <- paste0(file.name, ".R")
      knitr::purl(file.path(rmd.path, file.i),
                  file.path(extracted.path, extracted.file))
    })
    
  }
  
extract_code(sections.path, extracted.path)

# knitr::purl("QAQC.Rmd",
#             file.path(extracted.path, "QAQC.R"))
  
# Run ---------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(purrr)
#first load the data file

data<-read.csv("sections/data/projectData/wallkill2018/Wallkill_2018_chem.csv")
# data<-read.csv("sections/data/projectData/Streams/2018_Ramapo/2018-Ramapo_EDD-merge-bind.csv")
# data<-read.csv("sections/data/projectData/Streams/2018_Ramapo/2018-Ramapo_EDD-merge-bind_2-11-19.csv")

# data<-read.csv("sections/data/projectData/Streams/2017_minnewaska/Minnewaska_chem_2017_raw.csv")

#This file is a list of lab errors extracted from the ALS PDF reports on the first page of "Narrative Documents". See General Chemistry and Metals (not always present)
errors<-read.csv("sections/data/projectData/wallkill2018/laberrors.csv")
# errors<-read.csv("sections/data/projectData/Streams/2018_Ramapo/laberrors.csv")
# errors<-read.csv("sections/data/projectData/Streams/2017_minnewaska/laberrors.csv")

#truncate the input file to only the necessary fields
#this shortened file is saved as Wallkill.short.csv
# May want to add a SiteID field to the input data for carrying through. Would this be applicable to lakes data?

data<-unique(data[c('sys_sample_code','chemical_name','cas_rn','fraction','lab_qualifiers','lab_sdg','sample_date',
                    'result_value','result_unit','qc_original_conc','qc_spike_added','qc_spike_measured',
                    'method_detection_limit','detection_limit_unit','quantitation_limit','sample_source','sample_type_code',
                    'DEC_sample_type','analysis_date')])



  extracted.path <- c("sections")
  source.vec <- c(
    "prep.R",
    "Lab.R",
    "Accuracy.R",
    "Precision.R"#,
    # "Equipment.Blanks.R"#,
    # "Parameter.pairs.R",
    # "HT.R",
    # "Conclusions.R"
  
  )
  
  purrr::map(source.vec, function(source.i) {
    source(file.path(extracted.path, source.i))
  })
  