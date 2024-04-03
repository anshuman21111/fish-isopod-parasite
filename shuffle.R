OWB_list= shuffle.web(OWB, 1000, legacy=F)

OWB_list2=NULL
for (i in 1:length(OWB_list)) {
  rownames(OWB_list[[i]])=rownames(OWB)
  colnames(OWB_list[[i]])=colnames(OWB)
  wide=OWB_list[[i]]
  wide$X=rownames(wide)
  wide[is.na(wide)]=0
  long <- melt(setDT(wide), id.vars = c("X"), variable.name = "Parasite")
  colnames(long)[1]="Species"
  long$State="OWB"
  long$Fish_school=fish_OWB$Schooling[match(long$Species,fish_OWB$Species)]
  X=parasite_deg[which(parasite_deg$State=="OWB"),]
  
  for(j in 1:nrow(X)){
    X$parasite[j]=paste(strsplit(X$Species[j]," ")[[1]][1], ... = strsplit(X$Species[j]," ")[[1]][2], sep = ".")
  }
  long$Para_RegionAttachmnet=X$RegionAttachmnet[match(long$Parasite,X$parasite)]
  long$Fish_Habitat=fish_OWB$Habitat[match(long$Species,fish_OWB$Species)]
  long$Fish_MB=fish_OWB$Marine_Brackish[match(long$Species,fish_OWB$Species)]
  long[is.na(long)]=0
  
  OWB_list2[[i]]= long
}



#eg_swimming_habitatsalinity
sw_MB=NULL
sw_M=NULL
nsw_MB=NULL
nsw_M=NULL
for (i in 1:length(TN_list2)){
  long_rand=TN_list2[[i]]
  sw_MB=c(sw_MB,table(long_rand$Fish_school,long_rand$Fish_MB)["1","MB"])
  sw_M=c(sw_MB,table(long_rand$Fish_school,long_rand$Fish_MB)["1","M"])
  nsw_MB=c(sw_MB,table(long_rand$Fish_school,long_rand$Fish_MB)["0","MB"])
  nsw_M=c(sw_MB,table(long_rand$Fish_school,long_rand$Fish_MB)["0","M"])
}

shapiro.test(sw_MB)
val=table(long$Fish_school,long$Fish_MB)["1","MB"]
pnorm(val,mean(sw_MB),sd(sw_MB))