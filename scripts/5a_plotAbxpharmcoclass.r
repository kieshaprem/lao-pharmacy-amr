
hist(pharma$beta_lactams_penicillins)
shapiro.test(pharma$beta_lactams_penicillins)
abx_pharmcoclass_label
abx_pharmcoclass = vector(mode = 'list', length = 8)
names(abx_pharmcoclass) = unique(abx$abx_pharmcoclass)[1:8]
names(abx_pharmcoclass) = names(abx_pharmcoclass)[c(1:5,8,7,6)] 
names(abx_pharmcoclass)

for(i in 1:length(abx_pharmcoclass))
{
  abx_pharmcoclass[[i]] = aggregate(pharma[,names(abx_pharmcoclass)[i]],by = list(pharma$Provinces), quantile)
}

median_abx_pharmcoclass = array(NA, 8)
for(i in 1:length(abx_pharmcoclass)) median_abx_pharmcoclass[i] = (max(abx_pharmcoclass[[i]]$x[,3]))
median_abx_pharmcoclass
rm(median_abx_pharmcoclass)

q3_abx_pharmcoclass = array(NA, 8)
for(i in 1:length(abx_pharmcoclass)) q3_abx_pharmcoclass[i] = (max(abx_pharmcoclass[[i]]$x[,4]))
q3_abx_pharmcoclass
rm(q3_abx_pharmcoclass)


# median_abx_pharmcoclass[rev(order(median_abx_pharmcoclass))]
# abx_pharmcoclass[rev(order(median_abx_pharmcoclass))]

XRANGE = c(0,14)
YRANGE = c(0.5,8.5)

png('output/abx.png',width=15,height=10,units='cm',res=700)
if(1){
  grid.newpage()
  
  pushViewport(plotViewport(c(3,10,1,1),xscale=XRANGE,yscale=YRANGE))
  # grid.rect(gp=gpar(fill=NA))

   grid.text(label = 'Antibiotics audited per shop',y = unit(-2.5,'lines'),rot = 0,gp = gpar(fontsize = 9,fontface = 'plain'))
  
  for(j in 1:length(abx_pharmcoclass_label))
  {
     
    grid.polygon(y = 9-c(j-0.35,j-0.35,j-0.15,j-0.15),
                 x = as.numeric(c(0,abx_pharmcoclass[[j]]$x[1,3],abx_pharmcoclass[[j]]$x[1,3],0)),
                 default.units = 'native',gp = gpar(fill = COLS[1],col=NA,alpha = 1))
    
    grid.polygon(y = 9-c(j-0.1,j-0.1,j+0.1,j+0.1),
                 x = as.numeric(c(0,abx_pharmcoclass[[j]]$x[2,3],abx_pharmcoclass[[j]]$x[2,3],0)),
                 default.units = 'native',gp = gpar(fill = COLS[2],col=NA,alpha = 1))
    
    grid.polygon(y = 9-c(j+0.35,j+0.35,j+0.15,j+0.15),
                 x = as.numeric(c(0,abx_pharmcoclass[[j]]$x[3,3],abx_pharmcoclass[[j]]$x[3,3],0)),
                 default.units = 'native',gp = gpar(fill = COLS[3],col=NA,alpha = 1))
    

    

    grid.lines(y = 9-c(j-0.25,j-0.25),
               x = as.numeric(c(abx_pharmcoclass[[j]]$x[1,2],abx_pharmcoclass[[j]]$x[1,3])),
               default.units = 'native',gp = gpar(col = 'white',lwd = 0.75))
    grid.lines(y = 9-c(j-0,j-0),
               x = as.numeric(c(abx_pharmcoclass[[j]]$x[2,2],abx_pharmcoclass[[j]]$x[2,3])),
               default.units = 'native',gp = gpar(col = 'white',lwd = 0.75))
    grid.lines(y = 9-c(j+0.25,j+0.25),
               x = as.numeric(c(abx_pharmcoclass[[j]]$x[3,2],abx_pharmcoclass[[j]]$x[3,3])),
               default.units = 'native',gp = gpar(col = 'white',lwd = 0.75))
    grid.lines(y = 9-c(j-0.25,j-0.25),
               x = as.numeric(c(abx_pharmcoclass[[j]]$x[1,4],abx_pharmcoclass[[j]]$x[1,3])),
               default.units = 'native',gp = gpar(col = COLS[1],lwd = 0.75))
    grid.lines(y = 9-c(j-0,j-0),
               x = as.numeric(c(abx_pharmcoclass[[j]]$x[2,4],abx_pharmcoclass[[j]]$x[2,3])),
               default.units = 'native',gp = gpar(col = COLS[2],lwd = 0.75))
    grid.lines(y = 9-c(j+0.25,j+0.25),
               x = as.numeric(c(abx_pharmcoclass[[j]]$x[3,4],abx_pharmcoclass[[j]]$x[3,3])),
               default.units = 'native',gp = gpar(col = COLS[3],lwd = 0.75))
    
  }
   
   grid.lines(y = YRANGE,x=c(0,0),default.units = 'native') 
   grid.yaxis(at = 8:1,label = abx_pharmcoclass_label,gp = gpar(fontface = 'plain', fontsize = 9))
   grid.xaxis(gp=gpar(fontsize = 9))
   
  # grid.rect(gp=gpar(fill=NA))
}
dev.off()



