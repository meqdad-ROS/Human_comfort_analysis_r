#Plotting Sensitivity
meqdat<-read.csv("D:/DATA/Papers working/Raw Data_Meqdad/Pooled.csv",",",header=TRUE)
names(meqdat)
library(ggplot2)
library(cowplot)
p1<-ggplot(meqdat, aes(x=Sens, fill=Gen)) +geom_density(alpha=0.4,position="identity")
p1<-p1 + theme(legend.position="bottom")+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p1<-p1+scale_x_continuous(limits = c(0, 5))+scale_y_continuous(limits = c(0, 0.8))
p1
p2<-ggplot(meqdat, aes(x=Sens, fill=Age)) +geom_density(alpha=0.4)
p2<-p2 + theme(legend.position="bottom")+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p2<-p2+scale_x_continuous(limits = c(0, 5))+scale_y_continuous(limits = c(0, 0.8))
p2
p3<-ggplot(meqdat, aes(x=Sens, fill=Height)) +geom_density(alpha=0.4)
p3<-p3 + theme(legend.position="bottom")+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p3<-p3+scale_x_continuous(limits = c(0, 5))+scale_y_continuous(limits = c(0, 1.1))
p3
PA<-plot_grid(p1, p2,p3,ncol=3, labels = c('A', 'B','C'))
#Plotting performance
p4<-ggplot(meqdat, aes(x=Perf, fill=Gen)) +geom_density(alpha=0.4,position="identity")
p4<-p4 + theme(legend.position="bottom")+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p4<-p4+scale_x_continuous(limits = c(0, 5))+scale_y_continuous(limits = c(0, 0.8))
p4
p5<-ggplot(meqdat, aes(x=Perf, fill=Age)) +geom_density(alpha=0.4)
p5<-p5 + theme(legend.position="bottom")+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p5<-p5+scale_x_continuous(limits = c(0, 5))+scale_y_continuous(limits = c(0, 0.8))
p5
p6<-ggplot(meqdat, aes(x=Perf, fill=Height)) +geom_density(alpha=0.4)
p6<-p6 + theme(legend.position="bottom")+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p6<-p6+scale_x_continuous(limits = c(0, 5))+scale_y_continuous(limits = c(0, 1.1))
p6
PB<-plot_grid(p4, p5,p6,ncol = 3, labels = c('D', 'E','F'))
plot_grid(PA, PB,nrow = 2)
