# código augusto
base<-read.csv("C:/Users/Usuario/Desktop/pbp_matches_atp_main_archive.csv",
               header=TRUE, sep = ",")

porcentaje_jugador<-function(player, server)
    
    base_rafa<-base %>% filter(server1=="Rafael Nadal" )  
partidos<-strsplit( as.character(base_rafa$pbp),";" )

saque_rafa<-list(length=length(partidos))

for (i in 1:length(partidos)) {
    l <- length(partidos[[i]][])
    
    saque_rafa[[i]]<- partidos[[i]][seq(1,l,2)]
    
}

scores_rafa1<-saque_rafa
scores_rafa2<-saque_rafa
scores_rafa3<-saque_rafa


for (i in 1:length(partidos)) {
    l <- length(saque_rafa[[i]][])
    for (j in 1:l) {
        scores_rafa1[[i]][j] <- str_count (saque_rafa[[i]][j],  paste(c("S", "A"), collapse='|'))
        scores_rafa2[[i]][j] <- str_count (saque_rafa[[i]][j],  paste(c("R", "D"), collapse='|'))
        scores_rafa3[[i]][j] <- paste(scores_rafa1[[i]][j], scores_rafa2[[i]][j],sep="-" )
    }
    
}

coso<-list()
puntos<-matrix(0,ncol=2, nrow=length(scores_rafa3))


for (i  in 1:length(scores_rafa3)) {
    coso[[i]]<-strsplit(scores_rafa3[[i]], "-")
    l<-length(coso[[i]])
    
    for (j in 1:l) {
        
        puntos[i,1]<-  puntos[i,1]+as.numeric(coso[[i]][[j]][1])
        puntos[i,2]<-  puntos[i,2]+as.numeric(coso[[i]][[j]][2])
    }
}
