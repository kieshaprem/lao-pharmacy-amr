
hist(pharma$unregistered_prop)
abx_unregistered = vector(mode = 'list', length = 3)

abx_unregistered[[1]] = as.numeric(100*quantile(pharma$unregistered_prop[pharma$Provinces %in% "Champasack" ]))
abx_unregistered[[2]] = as.numeric(100*quantile(pharma$unregistered_prop[pharma$Provinces %in% "Luangprabang" ]))
abx_unregistered[[3]] = as.numeric(100*quantile(pharma$unregistered_prop[pharma$Provinces %in% "Vientiane Capital" ]))

abx_watch = vector(mode = 'list', length = 3)

abx_watch[[1]] = as.numeric(100*quantile(pharma$abx_watch_prop[pharma$Provinces %in% "Champasack" ]))
abx_watch[[2]] = as.numeric(100*quantile(pharma$abx_watch_prop[pharma$Provinces %in% "Luangprabang" ]))
abx_watch[[3]] = as.numeric(100*quantile(pharma$abx_watch_prop[pharma$Provinces %in% "Vientiane Capital" ]))

abx_watch

XRANGE = c(0.5,2.5)
YRANGE = c(0,55)


png('output/abx_boxplots.png',width=7,height=10,units='cm',res=700)
if(1){
  grid.newpage()
  
  pushViewport(plotViewport(c(3,3,1,1),xscale=XRANGE,yscale=YRANGE))
  grid.yaxis(gp = gpar(fontsize = 9))
  grid.lines(y=c(0,0), x = XRANGE,default.units = 'native')
  grid.xaxis(at = c(1,2), label = c('Unregistered\nantibiotics', 'Watch list\nantibiotics'), gp=gpar(fontsize = 9))
  grid.text(label = '% antibiotics audited per shop',x = unit(-3,'lines'),rot = 90,gp = gpar(fontsize = 9,fontface = 'plain'))

  grid.lines(x = c(0.75,0.75), y = abx_unregistered[[1]][c(1,5)],default.units = 'native', gp = gpar(lwd = 1.5,col = COLS[1]))
  grid.lines(x = c(1,1), y = abx_unregistered[[2]][c(1,5)],default.units = 'native', gp = gpar(lwd = 1.5,col = COLS[2]))
  grid.lines(x = c(1.25,1.25), y = abx_unregistered[[3]][c(1,5)],default.units = 'native', gp = gpar(lwd = 1.5,col = COLS[3]))
  
  grid.polygon(x = c(0.65,0.85,0.85,0.65),
               y = abx_unregistered[[1]][c(2,2,4,4)],
               default.units = 'native',gp = gpar(fill = COLS[1],col=NA,alpha = 1))
  grid.polygon(x = c(0.9,1.1,1.1,0.9),
               y = abx_unregistered[[2]][c(2,2,4,4)],
               default.units = 'native',gp = gpar(fill = COLS[2],col=NA,alpha = 1))
  grid.polygon(x = c(1.15,1.35,1.35,1.15),
               y = abx_unregistered[[3]][c(2,2,4,4)],
               default.units = 'native',gp = gpar(fill = COLS[3],col=NA,alpha = 1))
  
  grid.lines(x = c(0.65,0.85), y = abx_unregistered[[1]][c(3,3)],default.units = 'native', gp = gpar(lwd = 1.5,col = 'white'))
  grid.lines(x = c(0.9,1.1), y = abx_unregistered[[2]][c(3,3)],default.units = 'native', gp = gpar(lwd = 1.5,col = 'white'))
  grid.lines(x = c(1.15,1.35), y = abx_unregistered[[3]][c(3,3)],default.units = 'native', gp = gpar(lwd = 1.5,col = 'white'))
  
  
  
  grid.lines(x = c(1.75,1.75), y = abx_watch[[1]][c(1,5)],default.units = 'native', gp = gpar(lwd = 1.5,col = COLS[1]))
  grid.lines(x = c(2,2), y = abx_watch[[2]][c(1,5)],default.units = 'native', gp = gpar(lwd = 1.5,col = COLS[2]))
  grid.lines(x = c(2.25,2.25), y = abx_watch[[3]][c(1,5)],default.units = 'native', gp = gpar(lwd = 1.5,col = COLS[3]))
  
  grid.polygon(x = c(1.65,1.85,1.85,1.65),
               y = abx_watch[[1]][c(2,2,4,4)],
               default.units = 'native',gp = gpar(fill = COLS[1],col=NA,alpha = 1))
  grid.polygon(x = c(1.9,2.1,2.1,1.9),
               y = abx_watch[[2]][c(2,2,4,4)],
               default.units = 'native',gp = gpar(fill = COLS[2],col=NA,alpha = 1))
  grid.polygon(x = c(2.15,2.35,2.35,2.15),
               y = abx_watch[[3]][c(2,2,4,4)],
               default.units = 'native',gp = gpar(fill = COLS[3],col=NA,alpha = 1))
  
  grid.lines(x = c(1.65,1.85), y = abx_watch[[1]][c(3,3)],default.units = 'native', gp = gpar(lwd = 1.5,col = 'white'))
  grid.lines(x = c(1.9,2.1), y = abx_watch[[2]][c(3,3)],default.units = 'native', gp = gpar(lwd = 1.5,col = 'white'))
  grid.lines(x = c(2.15,2.35), y = abx_watch[[3]][c(3,3)],default.units = 'native', gp = gpar(lwd = 1.5,col = 'white'))
  
  
    
  }
  # grid.rect(gp=gpar(fill=NA))
dev.off()


