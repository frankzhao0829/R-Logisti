# Scripts to generate exploration reports
# Need the describe2html node.js project

rm(list = ls())
library(dplyr)
library(foreach)
library(doParallel)

source('R/DeepDescribe.R')

RAW.DATA.DIR <- 'data/raw/'
REGION <- 'AMS'
#JSON.OUTPUS.DIR <- paste0('outputs/json/', REGION, '/')
JSON.OUTPUS.DIR <- ''

csv.file.list <- list.files(RAW.DATA.DIR, pattern = paste0(REGION, '.*csv$'), recursive = T)
txt.file.list <- list.files(RAW.DATA.DIR, pattern = paste0(REGION, '.*txt$'), recursive = T)


registerDoParallel(cores = 4)

foreach(f = txt.file.list) %dopar% {
  txt.data <- data.table::fread(paste0(RAW.DATA.DIR, f), sep = '\t', quote = '', fill = T, integer64 = 'character')
  GenerateDescribeJSON(txt.data, stringr::str_match(f, '.*(Week\\d{2})\\.\\w{3}')[2], out.folder = JSON.OUTPUS.DIR)
}

foreach(f = csv.file.list) %dopar% {
  csv.data <- data.table::fread(paste0(RAW.DATA.DIR, f), sep = ',', quote = '', fill = T, integer64 = 'character')
  GenerateDescribeJSON(csv.data, stringr::str_match(f, '.*(Week\\d{2})\\.\\w{3}')[2], out.folder = JSON.OUTPUS.DIR)
}
