

table(pharma$Provinces)
table(pharma$Class)
table(pharma$Class,pharma$Provinces)

# pharma$Urban
# pharma$`Peri-urban`
table1(~ factor(Urban) | Provinces, data=pharma, overall=c(left="Total"))
table1(~ factor(`Licensed Pharmacist`) | Provinces, data=pharma, overall=c(left="Total"))

table1(~ factor(Rural) | Provinces, data=pharma, overall=c(left="Total"))
pharma$`Licensed Pharmacist`

median(pharma$`Distance to nearest hospital (KM)`)
min(pharma$`Distance to nearest hospital (KM)`)
max(pharma$`Distance to nearest hospital (KM)`)

aggregate(x = pharma$`Distance to nearest hospital (KM)`,by = list(pharma$Provinces),median)
# aggregate(x = pharma$`Distance to nearest hospital (KM)`,by = list(pharma$Provinces),mean)
aggregate(x = pharma$`Distance to nearest hospital (KM)`,by = list(pharma$Provinces),min)
aggregate(x = pharma$`Distance to nearest hospital (KM)`,by = list(pharma$Provinces),max)

median(pharma$`Years of working experience`,na.rm = TRUE)
min(pharma$`Years of working experience`,na.rm = TRUE)
max(pharma$`Years of working experience`,na.rm = TRUE)

aggregate(x = pharma$`Years of working experience`,by = list(pharma$Provinces),function(x){median(x,na.rm = TRUE)})
aggregate(x = pharma$`Years of working experience`,by = list(pharma$Provinces),function(x){min(x,na.rm = TRUE)})
aggregate(x = pharma$`Years of working experience`,by = list(pharma$Provinces),function(x){max(x,na.rm = TRUE)})

mean(pharma$`Years of working experience`,na.rm = TRUE)
sd(pharma$`Years of working experience`,na.rm = TRUE)

aggregate(x = pharma$`Years of working experience`,by = list(pharma$Provinces),function(x){mean(x,na.rm = TRUE)})
aggregate(x = pharma$`Years of working experience`,by = list(pharma$Provinces),function(x){sd(x,na.rm = TRUE)})

# fisher.test(table(pharma$Provinces,pharma$Urban))
# wilcox.test(table(pharma$Provinces,pharma$`Years of working experience`))

##### GPP
mean(pharma$gppscore,na.rm = TRUE)
sd(pharma$gppscore,na.rm = TRUE)

aggregate(x = pharma$gppscore,by = list(pharma$Provinces),function(x){mean(x,na.rm = TRUE)})
aggregate(x = pharma$gppscore,by = list(pharma$Provinces),function(x){sd(x,na.rm = TRUE)})


##### Antibiotic stock in the pharmacies


table(abx$pharmacy_id,abx$Provinces, useNA = 'ifany')
pharma1 = data.frame(table(abx$pharmacy_id, useNA = 'ifany'))
colnames(pharma1) = c('pharmacy_id','abx_n_total')
pharma = merge(pharma,pharma1,by = 'pharmacy_id')
rm(pharma1)
pharma = pharma[order(pharma$Item),]

table(abx$abx_pharmcoclass,useNA = 'ifany')
unique(abx$abx_pharmcoclass)

pharma$unregistered = NA
pharma$abx_watch = NA
pharma$beta_lactams_penicillins = NA
pharma$beta_lactams_cephalosporins = NA
pharma$macrolides = NA
pharma$fluoroquinolones = NA     
pharma$tetracyclines = NA           
pharma$sulfonamides_trimethoprim = NA 
pharma$aminoglycosides = NA
pharma$others = NA


for(i in 1:nrow(pharma))
{
  pharma$unregistered[i] = sum(abx$`Unregistered =1`[abx$pharmacy_id %in% pharma$pharmacy_id[i]])
  pharma$abx_watch[i] = sum(abx$aware[abx$pharmacy_id %in% pharma$pharmacy_id[i]] %in% 'watch')
  pharma$beta_lactams_penicillins[i] = sum(abx$abx_pharmcoclass[abx$pharmacy_id %in% pharma$pharmacy_id[i]] %in% 'beta_lactams_penicillins')
  pharma$beta_lactams_cephalosporins[i] = sum(abx$abx_pharmcoclass[abx$pharmacy_id %in% pharma$pharmacy_id[i]] %in% 'beta_lactams_cephalosporins')
  pharma$macrolides[i] = sum(abx$abx_pharmcoclass[abx$pharmacy_id %in% pharma$pharmacy_id[i]] %in% 'macrolides')
  pharma$fluoroquinolones[i] = sum(abx$abx_pharmcoclass[abx$pharmacy_id %in% pharma$pharmacy_id[i]] %in% 'fluoroquinolones')
  pharma$tetracyclines[i] = sum(abx$abx_pharmcoclass[abx$pharmacy_id %in% pharma$pharmacy_id[i]] %in% 'tetracyclines')
  pharma$sulfonamides_trimethoprim[i] = sum(abx$abx_pharmcoclass[abx$pharmacy_id %in% pharma$pharmacy_id[i]] %in% 'sulfonamides_trimethoprim')
  pharma$aminoglycosides[i] = sum(abx$abx_pharmcoclass[abx$pharmacy_id %in% pharma$pharmacy_id[i]] %in% 'aminoglycosides')
  pharma$others[i] = sum(abx$abx_pharmcoclass[abx$pharmacy_id %in% pharma$pharmacy_id[i]] %in% 'others')
}
rm(i)

# View(pharma)





# Total antibiotics audited (n)
aggregate(pharma$abx_n_total, by = list(pharma$Provinces),sum)

# mean(pharma$abx_n_total)
# sd(pharma$abx_n_total)

# aggregate(pharma$abx_n_total, by = list(pharma$Provinces),mean)
# aggregate(pharma$abx_n_total, by = list(pharma$Provinces),sd)
# 
# aggregate(pharma$abx_n_total, by = list(pharma$Provinces),median)





## Antibiotics audited per shop, mean (SD)
quantile(pharma$abx_n_total)
aggregate(pharma$abx_n_total, by = list(pharma$Provinces),quantile)

pharma$abx_n_total[pharma$Provinces %in% 'Champasack']
pharma$abx_n_total[pharma$Provinces %in% 'Luangprabang']
pharma$abx_n_total[pharma$Provinces %in% 'Vientiane Capital']

wilcox.test(pharma$abx_n_total[pharma$Provinces %in% 'Champasack'],pharma$abx_n_total[pharma$Provinces %in% 'Luangprabang'])
wilcox.test(pharma$abx_n_total[pharma$Provinces %in% 'Champasack'],pharma$abx_n_total[pharma$Provinces %in% 'Vientiane Capital'])
wilcox.test(pharma$abx_n_total[pharma$Provinces %in% 'Luangprabang'],pharma$abx_n_total[pharma$Provinces %in% 'Vientiane Capital'])




# Unregistered antibiotics per shop, (mean % of total antibiotics audited)
pharma$unregistered_prop = (pharma$unregistered/pharma$abx_n_total)
mean(pharma$unregistered_prop)
sd(pharma$unregistered_prop)
aggregate(pharma$unregistered_prop, by = list(pharma$Provinces),mean)
aggregate(pharma$unregistered_prop, by = list(pharma$Provinces),sd)

# Antibiotics with AWaRe 'Watch' group products per shop, (mean % of total antibiotics audited)
pharma$abx_watch_prop = (pharma$abx_watch/pharma$abx_n_total)
mean(pharma$abx_watch_prop)
sd(pharma$abx_watch_prop)
aggregate(pharma$abx_watch_prop, by = list(pharma$Provinces),mean)
aggregate(pharma$abx_watch_prop, by = list(pharma$Provinces),sd)



