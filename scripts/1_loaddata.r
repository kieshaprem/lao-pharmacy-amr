
gpp = read_xlsx(paste0('data/gpp.xlsx'), sheet = 'GPP score')
gpp_dict = read_xlsx(paste0('data/gpp.xlsx'), sheet = 'data_dictionary')


abx = read_xlsx(paste0('data/gpp.xlsx'), sheet = 'Antibiotics at pharmacy')
api = read.csv(paste0('data/api.csv'), as.is = TRUE) 

pharma = read_xlsx(paste0('data/kap.xlsx'), sheet = 'Interview information')

