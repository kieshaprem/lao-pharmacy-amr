

library(grid)


boxplot(gpp$`Total score`)

png('output/boxplot_gpp.png',width=14,height=17,units='cm',res=700)
grid.newpage()

pushViewport(plotViewport(c(3,4.5,1,1),xscale=c(0.25,4.75),yscale=c(0,100)))
# grid.rect(gp=gpar(fill=NA))
grid.yaxis(gp=gpar(fontsize = 12))
grid.lines(x = c(0.25,4.75), y = c(85,85), default.units = 'native',
           gp=gpar(lwd=1, lty = 'dashed',col = 'grey'))
if(1)
{
  grid.lines(x = c(1,1), y = c(max(gpp$`Total score`),min(gpp$`Total score`)), default.units = 'native',
             gp=gpar(lwd=2))
  grid.polygon(x = c(0.6,1.4,1.4,0.6), y = c(quantile(gpp$`Total score`,0.75),quantile(gpp$`Total score`,0.75),
                                             quantile(gpp$`Total score`,0.25),quantile(gpp$`Total score`,0.25)), 
               default.units = 'native',gp=gpar(lwd=1))
  grid.lines(x = c(0.6,1.4), y = c(median(gpp$`Total score`),median(gpp$`Total score`)), default.units = 'native',
             gp=gpar(lwd=2,col = '#1d7874'))
  
  grid.lines(x = c(0.6,1.4), y = c(mean(gpp$`Total score`),mean(gpp$`Total score`)), default.units = 'native',
             gp=gpar(lwd=1, lty = 'dotted',col = '#ee2e31'))
  
  grid.text(label = 'Median', x = unit(1,'native'),y = unit(median(gpp$`Total score`),'native')+unit(-0.5,'lines'),
            gp=gpar(fontsize=9,col = '#1d7874') )
  grid.text(label = 'Mean', x = unit(1,'native'),y = unit(mean(gpp$`Total score`),'native')+unit(-0.5,'lines'),
            gp=gpar(fontsize=9,col = '#ee2e31') )

}

pro = sort(unique(gpp$Provinces))

for(i in 1:3)
{
  grid.lines(x = c(i+1,i+1), y = c(max(gpp$`Total score`[gpp$Provinces %in% pro[i]]),min(gpp$`Total score`[gpp$Provinces %in% pro[i]])), 
             default.units = 'native',gp=gpar(lwd=2))
  grid.polygon(x = i+c(0.6,1.4,1.4,0.6), y = c(quantile(gpp$`Total score`[gpp$Provinces %in% pro[i]],0.75),
                                             quantile(gpp$`Total score`[gpp$Provinces %in% pro[i]],0.75),
                                             quantile(gpp$`Total score`[gpp$Provinces %in% pro[i]],0.25),
                                             quantile(gpp$`Total score`[gpp$Provinces %in% pro[i]],0.25)), 
               default.units = 'native',gp=gpar(lwd=1))
  grid.lines(x = c(i+0.6,i+1.4), y = c(median(gpp$`Total score`[gpp$Provinces %in% pro[i]]),
                                   median(gpp$`Total score`[gpp$Provinces %in% pro[i]])), 
             default.units = 'native',gp=gpar(lwd=2,col = '#1d7874'))
  
  grid.lines(x = c(i+0.6,i+1.4), y = c(mean(gpp$`Total score`[gpp$Provinces %in% pro[i]]),
                                   mean(gpp$`Total score`[gpp$Provinces %in% pro[i]])), 
             default.units = 'native',gp=gpar(lwd=1, lty = 'dotted',col = '#ee2e31'))
}
npro = c(paste0('n = ' ,nrow(gpp)))
for(i in 1:3) npro = c(npro,paste0('n = ' , sum(gpp$Provinces %in% pro[i])))

grid.xaxis(at = 1:4,label = c('All','Champasak','Luang Prabang','Vientiane Capital'),gp=gpar(fontsize = 9))
grid.lines(y=0)
# grid.text(x = unit(1:4,'native'),y = unit(-3.5,'lines'),
          # label = npro,gp = gpar(fontsize = 8))
grid.text('GPP Score', x = unit(-3,'lines'),rot=90,gp = gpar(fontsize = 12))

dev.off()


boxplot(gpp$`dispensing_31 questionnaire`)
pro = sort(unique(gpp$Provinces))



