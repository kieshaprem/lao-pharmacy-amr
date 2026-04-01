# this is the master script
options(scipen=999)
COLS = c('#364F6B','#3FC1C9','#FC5185')
require(tidyverse)
require(readxl)
require(grid)
require(table1)

# load data into r
source('scripts/1_loaddata.r')


# clean the data
source('scripts/2_cleandata_gpp.r')
source('scripts/2_cleandata_abx.r')
source('scripts/2_cleandata_api.r')

# analyse the data

# make tables
source('scripts/3_tables1and2.r')
source('scripts/4_table2_abxaware.r')

# plots
source('scripts/5a_plotAbxpharmcoclass.r')
source('scripts/5b_plotAbxBoxplots.r')
source('scripts/5c_boxplot_gpp.r')
source('scripts/5d_plotGPP.r')
source('scripts/5e_plotAPI.r')

# models
source('scripts/6a_modelUnregistered.r')

