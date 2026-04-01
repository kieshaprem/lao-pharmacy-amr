
gpp$pharmacy_id
gpp$Provinces


table(abx$Country_manufactured)
abx$Country_manufactured = tolower(abx$Country_manufactured)
table(abx$Country_manufactured)
# plots an ugly historgram of the GPP scores
hist(gpp$`Total score`)


table(abx$pharmacy_id)
abx$`Unregistered =1`

aggregate(abx$`Unregistered =1`,by = list(abx$pharmacy_id),sum)
head(abx)
aggregate(abx$`Unregistered =1`,by = list(abx$Provinces),sum)

table(abx$`Unregistered =1`,abx$Country_manufactured)

round((table(abx$`Unregistered =1`,abx$Country_manufactured))/colSums(table(abx$`Unregistered =1`,abx$Country_manufactured)),2)*100

tab1 = aggregate(abx$`Unregistered =1`,by = list(abx$Country_manufactured),function(x) round(length(x)))
tab2 = aggregate(abx$`Unregistered =1`,by = list(abx$Country_manufactured),function(x) round(sum(x)))
tab3 = aggregate(abx$`Unregistered =1`,by = list(abx$Country_manufactured),function(x) round(sum(x)/length(x)*100,2))
unregabx = data.frame(country_manufactured = tab1[,1],
           n_abx = tab1[,2],
           n_unregabx = tab2[,2],
           p_unregabx = tab3[,2])
rm(tab1,tab2,tab3)
# Here we save the file as an excel file- Kiesha please look
write.csv(x = unregabx,file = 'output/unregabx.csv',row.names = FALSE)


abx$pharmacy_id %in% gpp$pharmacy_id
gpp$pharmacy_id %in% abx$pharmacy_id

tab1 = aggregate(abx$`Unregistered =1`,by = list(abx$pharmacy_id),function(x) round(length(x)))
tab2 = aggregate(abx$`Unregistered =1`,by = list(abx$pharmacy_id),function(x) round(sum(x)))
tab3 = aggregate(abx$`Unregistered =1`,by = list(abx$pharmacy_id),function(x) round(sum(x)/length(x)*100,2))
unregabx_pharmacy_id = data.frame(pharmacy_id = tab1[,1],
                      n_abx = tab1[,2],
                      n_unregabx = tab2[,2],
                      p_unregabx = tab3[,2])


unregabx_pharmacy_id$gppscore = gpp$`Total score`[match(x = unregabx_pharmacy_id$pharmacy_id, table = gpp$pharmacy_id)]
unregabx_pharmacy_id$pharmacy_id[match(x = pharma$pharmacy_id,table = unregabx_pharmacy_id$pharmacy_id)]
pharma$gppscore = unregabx_pharmacy_id$gppscore[match(x = pharma$pharmacy_id,table = unregabx_pharmacy_id$pharmacy_id)]

unregabx_pharmacy_id

plot(unregabx_pharmacy_id$p_unregabx,unregabx_pharmacy_id$gppscore)
cor.test(unregabx_pharmacy_id$p_unregabx,unregabx_pharmacy_id$gppscore)

plot(unregabx_pharmacy_id$n_abx,unregabx_pharmacy_id$gppscore)
cor.test(unregabx_pharmacy_id$n_abx,unregabx_pharmacy_id$gppscore)


gpp$`Total score`

rm(tab1,tab2,tab3)
