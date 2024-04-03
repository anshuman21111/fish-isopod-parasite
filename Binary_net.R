##Testing for binary networks

fish_WB=read.csv("Data/Fish_WB.csv")
fish_Odisha=read.csv("Data/Fish_Odisha.csv")

library(data.table)


wide=read.csv("Data/Odisha.csv")
wide[is.na(wide)]=0
long_O <- melt(setDT(wide), id.vars = c("X"), variable.name = "Parasite")
colnames(long_O)[1]="Species"
long_O$State="Odisha"
long_O$Fish_school=fish_Odisha$Schooling[match(long_O$Species,fish_Odisha$Species)]
X=parasite_deg[which(parasite_deg$State=="Odisha"),]

for(i in 1:nrow(X)){
  X$parasite[i]=paste(strsplit(X$Species[i]," ")[[1]][1], strsplit(X$Species[i]," ")[[1]][2], sep = ".")
}
long_O$Para_RegionAttachmnet=X$RegionAttachmnet[match(long_O$Parasite,X$parasite)]
long_O$Fish_Habitat=fish_Odisha$Habitat[match(long_O$Species,fish_Odisha$Species)]
long_O$Fish_MB=fish_Odisha$Marine_Brackish[match(long_O$Species,fish_Odisha$Species)]
long_O[is.na(long_O)]=0

wide=read.csv("Data/WestBengal.csv")
wide[is.na(wide)]=0
long_W <- melt(setDT(wide), id.vars = c("X"), variable.name = "Parasite")
colnames(long_W)[1]="Species"
long_W$State="WestBengal"
long_W$Fish_school=fish_WB$Schooling[match(long_W$Species,fish_WB$Species)]
X=parasite_deg[which(parasite_deg$State=="West Bengal"),]

for(i in 1:nrow(X)){
  X$parasite[i]=paste(strsplit(X$Species[i]," ")[[1]][1], strsplit(X$Species[i]," ")[[1]][2], sep = ".")
}
long_W$Para_RegionAttachmnet=X$RegionAttachmnet[match(long_W$Parasite,X$parasite)]
long_W$Fish_Habitat=fish_WB$Habitat[match(long_W$Species,fish_WB$Species)]
long_W$Fish_MB=fish_WB$Marine_Brackish[match(long_W$Species,fish_WB$Species)]
long_W[is.na(long_W)]=0

longall=rbind(long,long_O,long_W)
longall_1=longall[which(longall$value==1),]
longall$Fish_school=as.factor(longall$Fish_school)
longall$Para_RegionAttachmnet=as.factor(longall$Para_RegionAttachmnet)



fishall=rbind(fish_TN,fish_Odisha,fish_WB)

fishall[is.na(fishall)]=0

pel=longall_1[which(longall_1$Fish_Habitat=="P"),]
dem=longall_1[which(longall_1$Fish_Habitat=="D"),]

mar=longall_1[which(longall_1$Fish_MB=="M"),]
marfish=fishall[which(fishall$Marine_Brackish=="M"),]
marB=longall_1[which(longall_1$Fish_MB=="MB"),]
marBfish=fishall[which(fishall$Marine_Brackish=="MB"),]