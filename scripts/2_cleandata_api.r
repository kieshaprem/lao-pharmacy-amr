api$Test.result.0.Substadard.1.Meet.standard


api$Test.result.0.Substadard.1.Meet.standard
api$Test.code

api$results = as.numeric(gsub(pattern = '%',replacement = '',x = api$API.test.result))

table(tolower(api$Generic.name))
unique(tolower(api$Generic.name))
api$aware = NA

api[api$Generic.name %in% 'Heromycin',]
api$Generic.name[api$Generic.name %in% 'Heromycin'] = 'tetracycline'
api$abx = tolower(api$Generic.name)

table(api$abx, useNA = 'ifany')

unique(sort(api$abx))
api$aware[api$abx %in% 'amoxicillin'] = 'access'
api$aware[api$abx %in% 'ampicillin'] = 'access'
api$aware[api$abx %in% 'azithromycin'] = 'watch'
api$aware[api$abx %in% 'cefixime'] = 'watch'
api$aware[api$abx %in% 'chloramphenicol'] = 'access' 
api$aware[api$abx %in% 'doxycycline'] = 'access'
api$aware[api$abx %in% 'erythromycin'] = 'access'
api$aware[api$abx %in% 'metronidazole'] = 'access'
api$aware[api$abx %in% 'penicillin'] = 'access'
api$aware[api$abx %in% 'tetracycline'] = 'access'

table(api$aware, useNA = 'ifany')



api$acceptedrange_min = (sapply(strsplit(x = api$Accepted.range, split = ' '), "[[", 1))
api$acceptedrange_min = as.numeric(gsub(pattern = '-',replacement = '',api$acceptedrange_min))

api$acceptedrange_max = sapply(strsplit(x = api$Accepted.range, split = ' '), tail, n = 1)
api$acceptedrange_max = ((gsub(pattern = '-',replacement = '',api$acceptedrange_max)))
api$acceptedrange_max = as.numeric((gsub(pattern = '%',replacement = '',api$acceptedrange_max)))

api$Generic.name

capitalise_first <- function(s) {
  # Capitalizes the first letter and leaves the rest of the string as is
  paste0(toupper(substring(s, 1, 1)), substring(s, 2, nchar(s)))
}


api$Generic.name = capitalise_first(tolower(api$Generic.name))
sort(unique(api$Generic.name))
abx_names = sort(unique(api$abx))
abx_label = sort(unique(api$Generic.name))

api[order(api$Generic.name),]
api[order(api$aware),]




hist(api$results[api$abx %in% abx_names[1]])
hist(api$results[api$abx %in% abx_names[2]])
hist(api$results[api$abx %in% abx_names[3]])
hist(api$results[api$abx %in% abx_names[4]])
hist(api$results[api$abx %in% abx_names[5]])
hist(api$results[api$abx %in% abx_names[6]])
hist(api$results[api$abx %in% abx_names[7]])
hist(api$results[api$abx %in% abx_names[8]])
hist(api$results[api$abx %in% abx_names[9]])
hist(api$results[api$abx %in% abx_names[10]])

table(api$abx,useNA = 'ifany')
aggregate(api$results,list(abx = api$abx, aware = api$aware),median)
aggregate(api$results,list(abx = api$abx, aware = api$aware),mean)


api$results < api$acceptedrange_min
api$results > api$acceptedrange_max

