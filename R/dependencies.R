### dependencies

#--------------------------------------------------------------------

# install if necessary

list_of_packages <- c("tidyquant", "bizdays", "tidyverse", "readxl")
new_packages <- list_of_packages[!(list_of_packages %in%
    installed.packages() [, "Package"])]
if (length(new_packages)) install.packages(new_packages)


# libraries

library(tidyquant)
library(tidyverse)
library(readxl)
#library(knitr)
#library(rmarkdown)
#library(bizdays)

# functions

"%!in%" <- function(x,y)!('%in%'(x,y))


## explore

tq_index_options()
tq_get_options()
tq_transmute_fun_options()
