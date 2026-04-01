
# View(gpp_dict)
gpp_dict$Description
gpp_dict$`Variable name`  
PRO =sort(unique(gpp$Provinces))
INDICATOR =c ("1: The condition of the pharmacy building",
              "2: Pharmacy data system",
              "3: Quality of medicine, drug arrangement and storage in pharmacy",
              "4: Quality of service" )

unique(gpp_dict$Parameters)
OFFSET = c(-0.15,0,0.15)
COLS = c('#364F6B','#3FC1C9','#FC5185')
bootstrap_ci = function(data)
{
  set.seed(123)        # For reproducibility
  n_resamples <- 1000  # Number of bootstrap resamples
  
  # Bootstrap resampling
  bootstrap_means <- replicate(n_resamples, {
    resample <- sample(data, replace = TRUE)
    mean(resample)
  })
  
  # Calculate 95% Confidence Interval
  ci_lower <- as.numeric(quantile(bootstrap_means, 0.025))
  ci_upper <- as.numeric(quantile(bootstrap_means, 0.975))
  
  ci = c(ci_lower,median(bootstrap_means),ci_upper)
  return(ci)
}





library(grid)

XRANGE = c(0.5,12.5)
YRANGE = c(0,2.4)
XVAR = gpp_dict$`Variable name`[5:16]
XLAB = gpp_dict$`Variable name`[5:16]
XLAB = gpp_dict$Description[5:16]
MAXSCORE = rep(2,length(XLAB))
k=1
png('output/gpp_indicator1.png',width=18,height=10,units='cm',res=700)
grid.newpage()

pushViewport(plotViewport(c(10,4.5,1,1),xscale=XRANGE,yscale=YRANGE))
grid.rect(gp=gpar(fill=NA))
grid.yaxis(gp = gpar(fontsize = 9))
grid.xaxis(1:12,label = FALSE)
grid.text(x = unit(1:12,'native'), y = unit(-1,'lines'), label = XLAB, rot = 45,just = 'right', gp = gpar(fontsize = 7) )
grid.text(label = INDICATOR[k],y = unit(1,'npc')-unit(0.5,'lines'),gp = gpar(fontsize = 10,fontface = 'bold'))
grid.text(label = 'Itemized score',x = unit(-3.5,'lines'),rot = 90,gp = gpar(fontsize = 9,fontface = 'plain'))
i=1
j = 1

  for(j in 1:length(XLAB))
  {
    grid.lines(x = c(j-0.5,j+0.5),y = MAXSCORE,default.units = 'native',gp = gpar(col = 'grey', lwd=1))
    for(i in 1:length(PRO))
    {
    ci = bootstrap_ci(data = as.integer(gpp[gpp$Provinces %in% PRO[i],XVAR[j]][[1]]))

    grid.lines(x = c(j,j)+OFFSET[i],y = ci[c(1,3)],default.units = 'native',gp = gpar(col = COLS[i], lwd=2,alpha = 0.5))
    grid.points(x = c(j)+OFFSET[i],y = ci[c(3)],default.units = 'native',pch = 15+i,gp = gpar(col = COLS[i], cex=0.7))
    grid.points(x = c(j)+OFFSET[i],y = mean(as.integer(gpp[gpp$Provinces %in% PRO[i],XVAR[j]][[1]])),default.units = 'native',pch = 1,gp = gpar(col = COLS[i], cex=0.7))
    
  }
}
dev.off()
# gpp[gpp$Provinces %in% PRO[i],XVAR[j]]

k=2
XVAR = gpp_dict$`Variable name`[17:19]
XLAB = gpp_dict$`Variable name`[17:19]
XLAB = gpp_dict$Description[17:19]
XLAB[1] = "Computerized system and\ncompleted data collection"
YRANGE = c(0,11)
XRANGE = c(0.5,3.5)
MAXSCORE = c(10,5,5)
png('output/gpp_indicator2.png',width=18,height=10,units='cm',res=700)
grid.newpage()

pushViewport(plotViewport(c(10,4.5,1,1),xscale=XRANGE,yscale=YRANGE))
grid.rect(gp=gpar(fill=NA))
grid.yaxis(gp = gpar(fontsize = 9))
grid.xaxis(1:(max(XRANGE)-0.5),label = FALSE)
grid.text(x = unit(1:(max(XRANGE)-0.5),'native'), y = unit(-1,'lines'), label = XLAB, rot = 45,just = 'right', gp = gpar(fontsize = 7) )
grid.text(label = INDICATOR[k],y = unit(1,'npc')-unit(0.5,'lines'),gp = gpar(fontsize = 10,fontface = 'bold'))
grid.text(label = 'Itemized score',x = unit(-3.5,'lines'),rot = 90,gp = gpar(fontsize = 9,fontface = 'plain'))
i=1
j = 1

for(j in 1:length(XLAB))
{
  grid.lines(x = c(j-0.5,j+0.5),y = c(MAXSCORE[j],MAXSCORE[j]),default.units = 'native',gp = gpar(col = 'grey', lwd=1))
  for(i in 1:length(PRO))
  {
    ci = bootstrap_ci(data = as.integer(gpp[gpp$Provinces %in% PRO[i],XVAR[j]][[1]]))
    
    grid.lines(x = c(j,j)+OFFSET[i],y = ci[c(1,3)],default.units = 'native',gp = gpar(col = COLS[i], lwd=2,alpha = 0.5))
    grid.points(x = c(j)+OFFSET[i],y = ci[c(3)],default.units = 'native',pch = 15+i,gp = gpar(col = COLS[i], cex=0.7))
    grid.points(x = c(j)+OFFSET[i],y = mean(as.integer(gpp[gpp$Provinces %in% PRO[i],XVAR[j]][[1]])),default.units = 'native',pch = 1,gp = gpar(col = COLS[i], cex=0.7))
  }
}
dev.off()



k=3
XVAR = gpp_dict$`Variable name`[20:26]
XLAB = gpp_dict$`Variable name`[20:26]
XLAB = gpp_dict$Description[20:26]
XLAB
XLAB[2]= "Medicines should not be exposed to\nsunlight,moisture and heat" 
XLAB[4]= "Expired medications are segregated\nfor proper handling or destruction" 
YRANGE = c(0,3.7)
XRANGE = c(0.5,7.5)
MAXSCORE = c(3,1,1,1,3,3,3)
png('output/gpp_indicator3.png',width=18,height=10,units='cm',res=700)
grid.newpage()

pushViewport(plotViewport(c(10,4.5,1,1),xscale=XRANGE,yscale=YRANGE))
grid.rect(gp=gpar(fill=NA))
grid.yaxis(gp = gpar(fontsize = 9))
grid.xaxis(1:(max(XRANGE)-0.5),label = FALSE)
grid.text(x = unit(1:(max(XRANGE)-0.5),'native'), y = unit(-1,'lines'), label = XLAB, rot = 45,just = 'right', gp = gpar(fontsize = 7) )
grid.text(label = INDICATOR[k],y = unit(1,'npc')-unit(0.5,'lines'),gp = gpar(fontsize = 10,fontface = 'bold'))
grid.text(label = 'Itemized score',x = unit(-3.5,'lines'),rot = 90,gp = gpar(fontsize = 9,fontface = 'plain'))
i=1
j = 1

for(j in 1:length(XLAB))
{
  grid.lines(x = c(j-0.5,j+0.5),y = c(MAXSCORE[j],MAXSCORE[j]),default.units = 'native',gp = gpar(col = 'grey', lwd=1))
  for(i in 1:length(PRO))
  {
    ci = bootstrap_ci(data = as.integer(gpp[gpp$Provinces %in% PRO[i],XVAR[j]][[1]]))
    
    grid.lines(x = c(j,j)+OFFSET[i],y = ci[c(1,3)],default.units = 'native',gp = gpar(col = COLS[i], lwd=2,alpha = 0.5))
    grid.points(x = c(j)+OFFSET[i],y = ci[c(3)],default.units = 'native',pch = 15+i,gp = gpar(col = COLS[i], cex=0.7))
    grid.points(x = c(j)+OFFSET[i],y = mean(as.integer(gpp[gpp$Provinces %in% PRO[i],XVAR[j]][[1]])),default.units = 'native',pch = 1,gp = gpar(col = COLS[i], cex=0.7))
  }
}
dev.off()



k=4
XVAR = gpp_dict$`Variable name`[27:40]
XLAB = gpp_dict$`Variable name`[27:40]
XLAB = gpp_dict$Description[27:40]
XLAB[2] ="Glass cabinet for medicine" 
XLAB[4] ="Chairs for patients to sit"  
XLAB[5] ="A free drinking water in case of emergency"
XLAB[8] ="Certificates of participation in training related to medicine"
XLAB[10] = "The package or bag containing the drug contains\nwritten instructions for the use of the drug"
XLAB[12] = "Basic knowledge of Pharmacist\nTest form with 31 questions"
XLAB[1]="Separate pill counter"
XLAB[14]="Distribution of related medical equipment"
YRANGE = c(0,10.5)
XRANGE = c(0.5,14.5)
MAXSCORE = c(1,1,1,1,1,1,1,1,10,5,3,10,3,2)
png('output/gpp_indicator4.png',width=18,height=10,units='cm',res=700)
grid.newpage()

pushViewport(plotViewport(c(10,4.5,1,1),xscale=XRANGE,yscale=YRANGE))
grid.rect(gp=gpar(fill=NA))
grid.yaxis(gp = gpar(fontsize = 9))
grid.xaxis(1:max(XRANGE)-0.5,label = FALSE)
grid.text(x = unit(1:(max(XRANGE)-0.5),'native'), y = unit(-1,'lines'), label = XLAB, rot = 45,just = 'right', gp = gpar(fontsize = 7) )
grid.text(label = INDICATOR[k],y = unit(1,'npc')-unit(0.5,'lines'),gp = gpar(fontsize = 10,fontface = 'bold'))
grid.text(label = 'Itemized score',x = unit(-3.5,'lines'),rot = 90,gp = gpar(fontsize = 9,fontface = 'plain'))
i=1
j = 1

for(j in 1:length(XLAB))
{
  grid.lines(x = c(j-0.5,j+0.5),y = c(MAXSCORE[j],MAXSCORE[j]),default.units = 'native',gp = gpar(col = 'grey', lwd=1))
  for(i in 1:length(PRO))
  {
    ci = bootstrap_ci(data = as.integer(gpp[gpp$Provinces %in% PRO[i],XVAR[j]][[1]]))
    
    grid.lines(x = c(j,j)+OFFSET[i],y = ci[c(1,3)],default.units = 'native',gp = gpar(col = COLS[i], lwd=2,alpha = 0.5))
    grid.points(x = c(j)+OFFSET[i],y = ci[c(3)],default.units = 'native',pch = 15+i,gp = gpar(col = COLS[i], cex=0.7))
    grid.points(x = c(j)+OFFSET[i],y = mean(as.integer(gpp[gpp$Provinces %in% PRO[i],XVAR[j]][[1]])),default.units = 'native',pch = 1,gp = gpar(col = COLS[i], cex=0.7))
  }
}
dev.off()

