
range(api$results)
XRANGE = c(0,130)
YRANGE = c(0.5,10.5)

png('output/api_assay.png',width=15,height=10,units='cm',res=700)
if(1){
  grid.newpage()

pushViewport(plotViewport(c(3,6,1,1),xscale=XRANGE,yscale=YRANGE))
grid.rect(gp=gpar(fill=NA))

abx_fontface = array('plain',length(abx_label))
abx_fontface[3] = 'bold'
abx_fontface[4] = 'bold'
abx_col = array('black',length(abx_label))
abx_col[3] = 'darkorange'
abx_col[4] = 'darkorange'
grid.yaxis(at = 10:1,label = abx_label,gp = gpar(fontface = abx_fontface, col = abx_col, fontsize = 9))
grid.xaxis(gp=gpar(fontsize = 9))
grid.text(label = 'Assay (%)',y = unit(-2.5,'lines'),rot = 0,gp = gpar(fontsize = 9,fontface = 'plain'))
grid.text(label = 'Acceptable range',x = unit(100,'native'),y = unit(1,'npc')+unit(0.6,'lines'),rot = 0,gp = gpar(fontsize = 7,fontface = 'bold',col ='darkgreen',alpha = 0.5))

for(i in seq(2,max(XRANGE),2)) grid.lines(x = c(i,i),y = c(min(YRANGE),max(YRANGE)),default.units = 'native',gp = gpar(lwd=0.5, col = 'grey'))

j=1
dens_abx_multi = array(3, length(abx_names))
dens_abx_multi[3] = 0.5
dens_abx_multi[5] = 1.25
dens_abx_multi[1] = 1.5
dens_abx_multi[2] = 1.25

for(j in 1:length(abx_names))
{
  grid.polygon(y = 11-c(j-0.5,j-0.5,j+0.5,j+0.5),
               x = c(api$acceptedrange_min[api$abx %in% abx_names[j]][1],api$acceptedrange_max[api$abx %in% abx_names[j]][1],
                     api$acceptedrange_max[api$abx %in% abx_names[j]][1],api$acceptedrange_min[api$abx %in% abx_names[j]][1]),
               default.units = 'native',gp = gpar(fill = 'darkgreen',col=NA,alpha = 0.3)
               )
  # xval_table = table(round(sort(api$results[api$abx %in% abx_names[j]])))
  
  xval = sort(api$results[api$abx %in% abx_names[j]])
  grid.points(x = xval,y = jitter(rep(11-j,length(xval))), default.units = 'native',pch = 16,
              gp = gpar(col = ifelse(xval<api$acceptedrange_max[api$abx %in% abx_names[j]][1]& 
                                       xval > api$acceptedrange_min[api$abx %in% abx_names[j]][1],'darkgreen','darkorange'), cex=0.5,
                        alpha =ifelse(xval<api$acceptedrange_max[api$abx %in% abx_names[j]][1]& 
                                        xval > api$acceptedrange_min[api$abx %in% abx_names[j]][1],0.7,1)))
  
  # dens = (density(xval))
  # grid.lines(x = dens$x, y = 11-(j+(dens_abx_multi[j]*dens$y)), default.units = 'native',gp=gpar(lwd=0.75,col=COLS[1]))
  # grid.lines(x = dens$x, y = 11-(j-(dens_abx_multi[j]*dens$y)), default.units = 'native',gp=gpar(lwd=0.75,col=COLS[1]))
  
  }
  
for(i in 1:10) grid.lines(y = c(i-0.5,i-0.5),x = c(min(XRANGE),max(XRANGE)),default.units = 'native',gp = gpar(lwd=1, col = 'black'))
grid.rect(gp=gpar(fill=NA))
}
dev.off()