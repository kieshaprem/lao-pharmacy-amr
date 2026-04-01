
head(abx)
table(abx$`Unregistered =1`)


abx$`Trade name`
table(tolower(abx$`Antibiotics available in pharmacy`), useNA = 'ifany')

abx$abx = tolower(abx$`Antibiotics available in pharmacy`)

# abx[689,] is it clarithromycin? 
unique(sort(abx$abx))
abx$aware =NA

abx$aware[abx$abx %in% 'acetyl spiramycin'] = 'access' # CHECK is acetyl spiramycin a spiramycin? 
abx$aware[abx$abx %in% 'amoxicillin'] = 'access'
abx$aware[abx$abx %in% 'amoxicillin+clavulanate'] = 'access'
abx$aware[abx$abx %in% 'ampicillin'] = 'access'
abx$aware[abx$abx %in% 'azithromycin'] = 'watch'
abx$aware[abx$abx %in% 'cefixime'] = 'watch'
abx$aware[abx$abx %in% 'cefixime + lactic acid'] = 'watch'
abx$aware[abx$abx %in% 'cefixime + lactobacillus'] = 'watch'
abx$aware[abx$abx %in% 'cefotaxime'] = 'watch'
abx$aware[abx$abx %in% 'cefotaxime sodium'] = 'watch'
abx$aware[abx$abx %in% 'ceftriaxone'] = 'watch'
abx$aware[abx$abx %in% 'cephalexin'] = 'access' # CHECK what is cephalexin?
abx$aware[abx$abx %in% 'cephamycin'] = NA # CHECK what is cephamycin?
abx$aware[abx$abx %in% 'chloramphenicol'] = 'access' 
abx$aware[abx$abx %in% 'ciprofloxacin'] = 'watch' 
abx$aware[abx$abx %in% 'clarithromycin'] = 'watch' 
abx$aware[abx$abx %in% 'clotrimazole'] = NA # CHECK is clotrimazole an antibiotics it is an antifungal?
abx$aware[abx$abx %in% 'cloxacillin'] = 'access' 
abx$aware[abx$abx %in% 'dicloxacillin'] = 'access'
abx$aware[abx$abx %in% 'doxycycline'] = 'access'
abx$aware[abx$abx %in% 'erythromycin'] = 'watch'
abx$aware[abx$abx %in% 'fluoroquinolones'] = 'watch' 
abx$aware[abx$abx %in% 'gentamicin sulfate'] = 'watch' 
abx$aware[abx$abx %in% 'heromycin'] = 'access'
abx$aware[abx$abx %in% 'kanamycin'] = 'watch' 
abx$aware[abx$abx %in% 'leomycin'] = 'watch' # CHECK Neomycin?
abx$aware[abx$abx %in% 'levofloxacin'] = 'watch' 
abx$aware[abx$abx %in% 'lincomycin'] = 'watch'
abx$aware[abx$abx %in% 'metronidazole'] = 'access' 
abx$aware[abx$abx %in% 'neomycin'] = 'watch'
abx$aware[abx$abx %in% 'nifuroxazide'] = 'watch' # CHECK
abx$aware[abx$abx %in% 'norfloxacin'] = 'watch'
abx$aware[abx$abx %in% 'ofloxacin'] = 'watch' 
abx$aware[abx$abx %in% 'oxytetracycline'] = 'watch' 
abx$aware[abx$abx %in% 'penicillin'] = 'access' 
abx$aware[abx$abx %in% 'rifampicin'] = 'watch'
abx$aware[abx$abx %in% 'roxithromycin'] = 'watch' 
abx$aware[abx$abx %in% 'sulfadiazine'] = 'access'
abx$aware[abx$abx %in% 'sulfamethoxazole+trimethoprim'] = 'access' 
abx$aware[abx$abx %in% 'tetracycline'] = 'access'
abx$aware[abx$abx %in% 'tobramycin'] = 'watch'

table(abx$aware, useNA = 'ifany')
table(abx$abx,abx$aware, useNA = 'ifany')

table(abx$`Unregistered =1`,abx$aware, useNA = 'ifany')

table(abx$`Unregistered =1`,abx$Provinces, useNA = 'ifany')
table(abx$aware,abx$Provinces, useNA = 'ifany')
table(abx$aware,abx$Class, useNA = 'ifany')


abx$abx_pharmcoclass = NA
unique(abx$`Antibiotics available in pharmacy`)



abx$abx_pharmcoclass[abx$abx %in% c("amoxicillin","amoxicillin+clavulanate","ampicillin",
                                    "cloxacillin","dicloxacillin","penicillin" )] = 'beta_lactams_penicillins'

abx$abx_pharmcoclass[abx$abx %in% c("cephalexin","cefixime","cefixime + lactic acid",
                                    "cefixime + lactobacillus","cephamycin ","cephamycin", "cephamycin ",
                                    "cefotaxime", "cefotaxime sodium", "ceftriaxone" )] = 'beta_lactams_cephalosporins'

abx$abx_pharmcoclass[abx$abx %in% c("acetyl spiramycin",'azithromycin','clarithromycin',"clarithromycin ","clarithromycin " ,
                                    "erythromycin",'roxithromycin')] = 'macrolides'


abx$abx_pharmcoclass[abx$abx %in% c('ciprofloxacin','levofloxacin','norfloxacin',
                                    'ofloxacin','fluoroquinolones')] = 'fluoroquinolones'

abx$abx_pharmcoclass[abx$abx %in% c('tetracycline','doxycycline','heromycin',
                                    'oxytetracycline')] = 'tetracyclines'

abx$abx_pharmcoclass[abx$abx %in% c('sulfadiazine','sulfamethoxazole+trimethoprim')] = 'sulfonamides_trimethoprim'

abx$abx_pharmcoclass[abx$abx %in% c("gentamicin sulfate","kanamycin",'leomycin','neomycin',
                                    'tobramycin')] = 'aminoglycosides'

abx$abx_pharmcoclass[abx$abx %in% c("lincomycin","chloramphenicol","metronidazole","nifuroxazide",'rifampicin')] = 'others'

abx$abx_pharmcoclass[abx$abx %in% c("clotrimazole")] = NA

abx_pharmcoclass_label = c('Beta-Lactams (Penicillins)','Beta-Lactams (Cephalosporins)','Macrolides','Fluoroquinolones (Quinolones)','Tetracyclines','Aminoglycosides','Sulfonamides & Trimethoprim','Others')

rev(sort(table(abx$abx_pharmcoclass,useNA = 'ifany')))

(table(abx$abx_pharmcoclass,abx$Provinces,useNA = 'ifany'))
(table(abx$abx_pharmcoclass,abx$aware,useNA = 'ifany'))





