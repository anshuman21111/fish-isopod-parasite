#####
#Testing for E being generalists#
#####

parasite_deg=read.csv("Data/owb_prop.csv")
parasite_deg$RegionAttachmnet=as.factor(parasite_deg$RegionAttachmnet)

#summary(lm(parasite_deg$degree~parasite_deg$RegionAttachmnet))

#removing the outliers detected
parasite_deg2=parasite_deg[-which(parasite_deg$Species=="Cymothoa indica"),]
parasite_deg2=parasite_deg2[-which(parasite_deg2$Species=="Catoessa boscii"),]
fixed.dum <-lm(effective.partners~RegionAttachmnet, data = parasite_deg2)
summary(fixed.dum)

av <- aov(fixed.dum)
summary(av)

tukey.test <- TukeyHSD(av)
tukey.test

boxplot(parasite_deg2$effective.partners~parasite_deg2$RegionAttachmnet)
boxplot(parasite_deg$degree~parasite_deg$RegionAttachmnet)
library(ggplot2)
p <- ggplot(parasite_deg2, aes(x=RegionAttachmnet, y=degree, fill=RegionAttachmnet)) +     geom_violin()
p