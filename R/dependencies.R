## dependencies

# install if necessary

list.of.packages <- c("tidyquant", "bizdays", "tidyverse", "readxl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# libraries

library(tidyquant)
library(bizdays)
library(tidyverse)
library(readxl)

## explore

tq_index_options()
tq_get_options()
