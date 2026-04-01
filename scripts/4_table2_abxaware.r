library(table1)

head(abx)
table1(~ factor(Class) + factor(aware) | Provinces, data=abx, overall=c(left="Total"))

# table1(~ factor(aware) | Provinces*Class, data=abx, overall=c(left="Total"))

colnames(abx)

length(unique(abx$pharmacy_id))

aggregate(abx$`Unregistered =1`,list(pharmacy_id = abx$pharmacy_id, Provinces = abx$Provinces),sum)
tab_abxunregistered = aggregate(abx$`Unregistered =1`,list(pharmacy_id = abx$pharmacy_id, Provinces = abx$Provinces),mean)
tab_abxwatch = aggregate(1*(abx$aware %in% 'watch'),list(pharmacy_id = abx$pharmacy_id, Provinces = abx$Provinces),mean)

identical(tab_abxwatch$pharmacy_id, tab_abxunregistered$pharmacy_id)

plot(tab_abxwatch$x,tab_abxunregistered$x, xlim = c(0,1), ylim = c(0,1), pch = 16)

table(tab_abxwatch$x,tab_abxunregistered$x)
cor.test(tab_abxwatch$x,tab_abxunregistered$x)
plot(lm(tab_abxwatch$x~tab_abxunregistered$x))

sort(unique(abx$abx))

table((abx$abx))

abx_names

abx$`Unregistered =1`

table((abx$abx[abx$aware %in% 'watch']))

table((abx$abx[abx$`Unregistered =1` %in% 1]))


table(abx$abx_pharmcoclass,useNA = 'ifany')
unique(abx$abx_pharmcoclass)



chisq.test(table(abx$aware,abx$`Unregistered =1`))
(table(abx$aware,abx$`Unregistered =1`)[1,1] * table(abx$aware,abx$`Unregistered =1`)[2,2])/(table(abx$aware,abx$`Unregistered =1`)[1,2] * table(abx$aware,abx$`Unregistered =1`)[2,1])

head(api)
(length(api$results) - (sum(api$results >api$acceptedrange_max)+sum(api$results <api$acceptedrange_min)))/length(api$results)