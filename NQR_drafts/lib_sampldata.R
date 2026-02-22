
library(usethis)
library(devtools)
library(roxygen2)
library(tidyverse)
library(ggplot2)
#library()



ligase_ela <- read_tsv("/mnt/nanopore/raw_data/libraries_tests/250320_ligaseEla_SuperScript/20250320_1344_MN39398_FBA13144_c7450e82/sequencing_summary_FBA13144_c7450e82_7c2ba15e.txt")

ligase_induro <- read_tsv("/mnt/nanopore/raw_data/libraries_tests/250320_ligaseT4_Induro/20250320_1337_MN40838_FBA13273_4849cee3/sequencing_summary_FBA13273_4849cee3_5ab4411b.txt")

seqsum_database = list(ligase_ela, ligase_induro)
