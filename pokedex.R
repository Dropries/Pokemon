library(tidyverse)
library(imager)
library(expm)

pokedex0=read.csv("pokedex.csv",sep=";",encoding="latin1")%>%mutate(Pokemon=ifelse(Forme=="",Nom,paste0(Nom," (",Forme,")")))
pokedex=pokedex0%>%group_by(Nom)%>%mutate(Numéro=which(Nom==unique(pokedex0$Nom)),PV=((2*PV+31+floor(255/4))*50/100)+50+10,across(ATT:VIT,~((2*.+31+floor(255/4))*50/100+5)*1.1))%>%ungroup()%>%arrange(Numéro)
table=read.csv("table.csv",sep=";",encoding="latin1")
attaques=read.csv("attaques.csv",sep=";",encoding="latin1")

#teams strat

groups=hclust(dist(pokedex%>%filter(Violet,Team,is.na(Interdit))%>%select(PV:VIT)%>%as.matrix))

score_team=function(dat,...){
  dat%>%filter(Pokemon%in%...)%>%summarise(score=sum(Score))
}

g_solo=pokedex%>%filter(Violet,Team,is.na(Interdit))%>%summarise(Pokemon,group=cutree(groups,3))
solo=read.csv("solo_rank.csv",sep=";",encoding="latin1")

p0=sort(runif(150)) ; p0=p0/sum(p0)
M=apply(matrix((crossing(Pokemon1=(solo%>%filter(Rang==0))$Pokemon,Pokemon2=(solo%>%filter(Rang==0))$Pokemon)%>%left_join(
  solo%>%mutate(group=rep(1:150,each=11))%>%group_by(group)%>%summarise(Pokemon1=first(Pokemon),Pokemon2=Pokemon[-1],p=rev(cumsum(1/(0.5*(10-0:9)))/sum(cumsum(1/(0.5*(10-0:9))))))%>%ungroup
)%>%replace_na(list(p=0)))$p,
nrow=150),
2,function(x)x/sum(x)) ; crossing(Pokemon_adv=(solo%>%filter(Rang==0))$Pokemon)%>%mutate(p=((M%^%10000%*%as.matrix(p0))*3)[,1])%>%mutate(p=ifelse(is.na(p),0,p))%>%arrange(desc(p))
solo_top=pokedex%>%filter(Violet,Team,is.na(Interdit))%>%select(Pokemon,Type.1:VIT)%>%rename_all(~paste0(.,"_team"))%>%
  crossing(solo%>%filter(Rang==0)%>%left_join(pokedex)%>%select(Pokemon,Type.1:VIT)%>%rename_all(~paste0(.,"_adv")))%>%
  group_by(Pokemon_team)%>%mutate(p=((M%^%10000%*%as.matrix(p0))*3)[,1])%>%mutate(p=ifelse(is.na(p),0,p))%>%filter(p>0)%>%ungroup%>%
  slice(rep(1:n(),each=nrow(attaques)))%>%cbind(slice(attaques,rep(1:nrow(attaques),nrow(.)/nrow(attaques))))%>%
  mutate(STAB_team=ifelse(Type==Type.1_team | Type==Type.2_team,1.5,1),STAB_adv=ifelse(Type==Type.1_adv|Type==Type.2_adv,1.5,1))%>%
  gather(var,Type_def,Type.1_team,Type.2_team,Type.1_adv,Type.2_adv)%>%filter(Type_def!="")%>%
    group_by(Type,Type_def)%>%mutate(res=(table%>%filter(Type==first(Type_def)))[,first(Type)])%>%ungroup%>%
    group_by(Pokemon_team,Pokemon_adv,Type,Nature)%>%mutate(res_team=ifelse(str_detect(var,"team"),res,NA),
                                                            res_adv=ifelse(str_detect(var,"adv"),res,NA))%>%
  mutate(res_team=prod(res_team,na.rm=T),res_adv=prod(res_adv,na.rm=T))%>%select(-(var:res))%>%slice(1)%>%ungroup%>%
  mutate(off=(1+0.5*4.17/100)*ifelse(Nature=="Phy",(22*ATT_team*Puissance/50/DEF_adv+2),(22*SATT_team*Puissance/50/SDEF_adv+2))*STAB_team*res_adv,
         def=(1+0.5*4.17/100)*ifelse(Nature=="Phy",(22*ATT_adv*Puissance/50/DEF_team+2),(22*SATT_adv*Puissance/50/SDEF_team+2))*STAB_adv*res_team)%>%
  group_by(Pokemon_team,Pokemon_adv)%>%mutate(off=max(off),def=max(def))%>%slice(1)%>%ungroup%>%select(-(Puissance:Nature))%>%
  mutate(n_off=PV_adv/off,n_def=PV_team/def,
         VIT_team_up=VIT_team*1.1,VIT_team_down=VIT_team*0.9,
         VIT_adv_up=VIT_adv*1.1,VIT_adv_down=VIT_adv*0.9,
         proba_first_team=321/625*((VIT_team>VIT_adv)+0.5*(VIT_team==VIT_adv))+
           68/625*((VIT_team>VIT_adv_up)+0.5*(VIT_team==VIT_adv_up)+
                   (VIT_team>VIT_adv_down)+0.5*(VIT_team==VIT_adv_down)+
                   (VIT_team_up>VIT_adv)+0.5*(VIT_team_up==VIT_adv)+
                   (VIT_team_down>VIT_adv)+0.5*(VIT_team_down==VIT_adv))+
           16/625*((VIT_team_up>VIT_team_down)+0.5*(VIT_team_up==VIT_team_down)+
                   (VIT_team_down>VIT_team_up)+0.5*(VIT_team_down==VIT_team_up)),
         score=proba_first_team*(ceiling(n_off)<=ceiling(n_def))+(1-proba_first_team)*(ceiling(n_off)<ceiling(n_def)),
         score=ifelse(is.na(score),min(score,na.rm=T),score))%>%
  group_by(Pokemon=Pokemon_team)%>%summarise(Score=sum(score*p))%>%left_join(g_solo)
solo_top%>%arrange(desc(Score))%>%print(n=Inf)

expand.grid(P1=(g_solo%>%filter(group==1))$Pokemon,P2=(g_solo%>%filter(group==2))$Pokemon,P3=(g_solo%>%filter(group==3))$Pokemon)%>%
  rowwise%>%mutate(score_team(dat=solo_top,c(P1,P2,P3)))%>%View




g_duo=pokedex%>%filter(Violet,Team,is.na(Interdit))%>%summarise(Pokemon,group=cutree(groups,4))
duo=read.csv("duo_rank.csv",sep=";",encoding="latin1")

p0=sort(runif(150)) ; p0=p0/sum(p0)
M=apply(matrix((crossing(Pokemon1=(duo%>%filter(Rang==0))$Pokemon,Pokemon2=(duo%>%filter(Rang==0))$Pokemon)%>%left_join(
  duo%>%mutate(group=rep(1:150,each=11))%>%group_by(group)%>%summarise(Pokemon1=first(Pokemon),Pokemon2=Pokemon[-1],p=rev(cumsum(1/(0.5*(10-0:9)))/sum(cumsum(1/(0.5*(10-0:9))))))%>%ungroup
)%>%replace_na(list(p=0)))$p,
nrow=150),
2,function(x)x/sum(x)) ; crossing(Pokemon_adv=(duo%>%filter(Rang==0))$Pokemon)%>%mutate(p=((M%^%10000%*%as.matrix(p0))*3)[,1])%>%mutate(p=ifelse(is.na(p),0,p))%>%arrange(desc(p))
duo_top=pokedex%>%filter(Violet,Team,is.na(Interdit))%>%select(Pokemon,Type.1:VIT)%>%rename_all(~paste0(.,"_team"))%>%
  crossing(duo%>%filter(Rang==0)%>%left_join(pokedex)%>%select(Pokemon,Type.1:VIT)%>%rename_all(~paste0(.,"_adv")))%>%
  group_by(Pokemon_team)%>%mutate(p=((M%^%10000%*%as.matrix(p0))*4)[,1])%>%mutate(p=ifelse(is.na(p),0,p))%>%filter(p>0)%>%ungroup%>%
  slice(rep(1:n(),each=nrow(attaques)))%>%cbind(slice(attaques,rep(1:nrow(attaques),nrow(.)/nrow(attaques))))%>%
  mutate(STAB_team=ifelse(Type==Type.1_team | Type==Type.2_team,1.5,1),STAB_adv=ifelse(Type==Type.1_adv|Type==Type.2_adv,1.5,1))%>%
  gather(var,Type_def,Type.1_team,Type.2_team,Type.1_adv,Type.2_adv)%>%filter(Type_def!="")%>%
  group_by(Type,Type_def)%>%mutate(res=(table%>%filter(Type==first(Type_def)))[,first(Type)])%>%ungroup%>%
  group_by(Pokemon_team,Pokemon_adv,Type,Nature)%>%mutate(res_team=ifelse(str_detect(var,"team"),res,NA),
                                                          res_adv=ifelse(str_detect(var,"adv"),res,NA))%>%
  mutate(res_team=prod(res_team,na.rm=T),res_adv=prod(res_adv,na.rm=T))%>%select(-(var:res))%>%slice(1)%>%ungroup%>%
  mutate(off=(1+0.5*4.17/100)*ifelse(Nature=="Phy",(22*ATT_team*Puissance/50/DEF_adv+2),(22*SATT_team*Puissance/50/SDEF_adv+2))*STAB_team*res_adv,
         def=(1+0.5*4.17/100)*ifelse(Nature=="Phy",(22*ATT_adv*Puissance/50/DEF_team+2),(22*SATT_adv*Puissance/50/SDEF_team+2))*STAB_adv*res_team)%>%
  group_by(Pokemon_team,Pokemon_adv)%>%mutate(off=max(off),def=max(def))%>%slice(1)%>%ungroup%>%select(-(Puissance:Nature))%>%
  mutate(n_off=PV_adv/off,n_def=PV_team/def,
         VIT_team_up=VIT_team*1.1,VIT_team_down=VIT_team*0.9,
         VIT_adv_up=VIT_adv*1.1,VIT_adv_down=VIT_adv*0.9,
         proba_first_team=321/625*((VIT_team>VIT_adv)+0.5*(VIT_team==VIT_adv))+
           68/625*((VIT_team>VIT_adv_up)+0.5*(VIT_team==VIT_adv_up)+
                     (VIT_team>VIT_adv_down)+0.5*(VIT_team==VIT_adv_down)+
                     (VIT_team_up>VIT_adv)+0.5*(VIT_team_up==VIT_adv)+
                     (VIT_team_down>VIT_adv)+0.5*(VIT_team_down==VIT_adv))+
           16/625*((VIT_team_up>VIT_team_down)+0.5*(VIT_team_up==VIT_team_down)+
                     (VIT_team_down>VIT_team_up)+0.5*(VIT_team_down==VIT_team_up)),
         score=proba_first_team*(ceiling(n_off)<=ceiling(n_def))+(1-proba_first_team)*(ceiling(n_off)<ceiling(n_def)),
         score=ifelse(is.na(score),min(score,na.rm=T),score))%>%
  group_by(Pokemon=Pokemon_team)%>%summarise(Score=sum(score*p))%>%left_join(g_duo)



expand.grid(P1=(g_duo%>%filter(group==1))$Pokemon,P2=(g_duo%>%filter(group==2))$Pokemon,P3=(g_duo%>%filter(group==3))$Pokemon,P4=(g_duo%>%filter(group==4))$Pokemon)%>%
  rowwise%>%mutate(score_team(dat=duo_top,c(P1,P2,P3,P4)))%>%View



degats=function(x,stat_att,type_att,puissance,niveau,offensif_stat,stab,bonus){
  if(stat_att=="ATT")defensif_stat="DEF"
  if(stat_att=="SATT")defensif_stat="SATT"
  resistance=x%>%select(id,Pokemon,contains("Type"),PV:VIT)%>%gather(var,Type,-(PV:VIT),-Pokemon,-id)%>%filter(Type!="")%>%
    left_join(table,by="Type")%>%group_by(Pokemon)%>%mutate_at(vars(Acier:Vol),function(...)prod(...))%>%slice(1)%>%ungroup%>%arrange(id)%>%
    select(all_of(type_att))%>%unlist%>%as.numeric
  return((x%>%mutate(a=((((2*niveau/5+2)*offensif_stat*puissance/.[[defensif_stat]])/50)+2)*stab*resistance*bonus))$a)
}






pokedex%>%filter(Violet,Team)%>%mutate(id=row_number())%>%mutate(
  att_Rapace=degats(.,"ATT","Vol",puissance=120,niveau=100,offensif_stat=355,stab=1,bonus=1),
  att_TripleFleche=degats(.,"ATT","Combat",puissance=90,niveau=100,offensif_stat=355,stab=1.5,bonus=1),
  att_LameFeuille=degats(.,"ATT","Plante",puissance=90,niveau=100,offensif_stat=355,stab=2,bonus=1))%>%
  rowwise%>%
  mutate(
    degats=max(c_across(starts_with("att",ignore.case=F))),
    coups_possibles=floor(PV/degats)
  )%>%
  select(Pokemon,starts_with("att",ignore.case=F),degats)%>%arrange(degats)%>%print(n=Inf)


tr_6iv=function(c0,r,z){
  return(32*6*2**r/((2*r+c0)*ifelse(z==0,1,0)+ifelse(z==1,1,0)))
}
tr_6iv_tc0=function(c0,r,z){
  if(c0==0)return(tr_6iv(c0,r,z))
  return((32*5*2**r/((2*r+c0-1)*ifelse(z==0,1,0)+ifelse(z==1,1,0))))
}
tr_6iv_tr=function(c0,r,z){
  if(r==0)return(tr_6iv(c0,r,z))
  return((32*5*2**(r-1)/((2*r+c0-2)*ifelse(z==0,1,0)+ifelse(z==1,1,0))))
}
expand.grid(0:6,0:6,0:6)%>%rename(c0=1,r=2,z=3)%>%filter(c0+r+z==6)%>%rowwise%>%
  mutate(E_6iv=tr_6iv(c0,r,z),E_6iv_tc0=tr_6iv_tc0(c0,r,z),E_6iv_tr=tr_6iv_tr(c0,r,z))%>%arrange(E_6iv)%>%print(n=14)


match=function(ordre,a,b){
  par(mfrow=c(1,2))
  sampled=sample(c(a,b),2)
  plot(load.image((pokedex%>%filter(Pokemon==sampled[1]))$Sprite),axes=F)
  plot(load.image((pokedex%>%filter(Pokemon==sampled[2]))$Sprite),axes=F)
  return(c(sampled)[menu(sampled,title=as.character(ordre))])
}

# formes différentes
formes=pokedex%>%filter(Principale==1)%>%group_by(Numéro,Génération)%>%mutate(n=n())%>%filter(n>1)%>%
  summarise(as.data.frame(t(combn(Pokemon,2))))%>%
  rename(P1=3,P2=4)%>%ungroup%>%mutate(ordre=round(sample(nrow(.),nrow(.))*100/n(),2))%>%group_by(ordre)%>%
  group_modify(~.x%>%mutate(Vainqueur=match(.y$ordre,.$P1,.$P2)))%>%ungroup()%>%dplyr::select(-ordre)%>%
  group_by(Numéro,Génération)%>%count(Vainqueur)%>%rename(Pokemon=Vainqueur)%>%filter(n==max(n))

completer=function(Gen){ 
  return(rbind(
    expand.grid((pokedex%>%filter(Génération==Gen,Evolue==F,Principale==1,is.na(Team)))$Pokemon,
                     (pokedex%>%filter(Génération==Gen,Evolue==F,Principale==1,!is.na(Team)))$Pokemon)%>%rename(P1=1,P2=2),
    pokedex%>%filter(Génération==Gen,Evolue==F,Principale==1,is.na(Team))%>%
      summarise(as.data.frame(t(combn(Pokemon,2))))%>%
      rename(P1=1,P2=2))%>%
      mutate_all(~as.character(.))%>%filter(P1!=P2)%>%mutate(ordre=round(sample(nrow(.),nrow(.))*100/n(),2))%>%group_by(ordre)%>%
           group_modify(~.x%>%mutate(Vainqueur=match(.y$ordre,.$P1,.$P2)))%>%ungroup()%>%dplyr::select(-ordre))
}

G=function(Gen){
  return(pokedex%>%filter(Génération==Gen,Evolue==F,Principale==1)%>%mutate(Pokemon=ifelse(Forme=="",Nom,paste0(Nom," (",Forme,")")))%>%
           summarise(as.data.frame(t(combn(Pokemon,2))))%>%
           rename(P1=1,P2=2)%>%mutate(ordre=round(sample(nrow(.),nrow(.))*100/n(),2))%>%group_by(ordre)%>%
           group_modify(~.x%>%mutate(Vainqueur=match(.y$ordre,.$P1,.$P2)))%>%ungroup()%>%dplyr::select(-ordre))
}

loadG=function(Gen){
  return(read.csv(paste0("G",Gen,".csv"),encoding="latin1",sep=";")%>%
           filter(P1%in%((pokedex%>%filter(Génération==Gen,Evolue==F,Principale==1)%>%mutate(Pokemon=ifelse(Forme=="",Nom,paste0(Nom," (",Forme,")"))))$Pokemon),
                  P2%in%((pokedex%>%filter(Génération==Gen,Evolue==F,Principale==1)%>%mutate(Pokemon=ifelse(Forme=="",Nom,paste0(Nom," (",Forme,")"))))$Pokemon))
  )
}

resG=function(G){
  res=pokedex%>%filter(Génération==G,Evolue==F,Principale==1)%>%dplyr::select(Pokemon)%>%
           left_join(loadG(G)%>%count(Vainqueur)%>%rename(Pokemon=Vainqueur))%>%
           replace_na(list(n=0))%>%arrange(desc(n))%>%summarise(Pokemon,Taux=round(100*n/n(),2))
  
  nqualif=res%>%filter(Taux>=nth(Taux,6))%>%nrow(.)
  
  if(nqualif==6){return(res%>%mutate(Team=Taux>=nth(Taux,6)))}
  
  # places - déjé qualifiés = 6-nqualif
  
  negalite=res%>%filter(Taux==nth(Taux,6))%>%nrow(.)
  
  egal=loadG(G)%>%filter(P1%in%(res%>%filter(Taux==nth(Taux,6)))$Pokemon,
                         P2%in%(res%>%filter(Taux==nth(Taux,6)))$Pokemon)%>%
    count(Vainqueur)%>%rename(Pokemon=Vainqueur)%>%arrange(desc(n))%>%filter(n>=nth(n,6-(nqualif-negalite)))%>%mutate(Team=1)
  
  return(res%>%mutate(Team=Taux>nth(Taux,6)|Pokemon%in%egal$Pokemon)%>%arrange(desc(Taux),desc(Team)))
}



write.table(G1,"G1.csv",row.names=F,sep=";",fileEncoding = "latin1")
write.table(G2,"G2.csv",row.names=F,sep=";",fileEncoding = "latin1")
write.table(G3,"G3.csv",row.names=F,sep=";",fileEncoding = "latin1")
write.table(G4,"G4.csv",row.names=F,sep=";",fileEncoding = "latin1")
write.table(G5,"G5.csv",row.names=F,sep=";",fileEncoding = "latin1")
write.table(G6,"G6.csv",row.names=F,sep=";",fileEncoding = "latin1")
write.table(G7,"G7.csv",row.names=F,sep=";",fileEncoding = "latin1")
write.table(G8,"G8.csv",row.names=F,sep=";",fileEncoding = "latin1")
write.table(G9,"G9.csv",row.names=F,sep=";",fileEncoding = "latin1")






pokedex%>%dplyr::select(-Team)%>%left_join(rbind(resG(1),
                                                 resG(2),
                                                 resG(3),
                                                 resG(4),
                                                 resG(5),
                                                 resG(6),
                                                 resG(7),
                                                 resG(8),
                                                 resG(9))%>%
                                             select(Pokemon,Team))%>%
  replace_na(list(Team=0))%>%select(-Pokemon)%>%write.table(.,"pokedex_final.csv",row.names=F,fileEncoding = "latin1",sep=";")



pokedex%>%dplyr::select(-Team)%>%left_join(rbind(resG(1),
                                                 resG(2),
                                                 resG(3),
                                                 resG(4),
                                                 resG(5),
                                                 resG(6),
                                                 resG(7),
                                                 resG(8),
                                                 resG(9))%>%
                                             select(Pokemon,Team,Taux))%>%filter(Principale==1,Team==1)%>%arrange(desc(Taux))%>%
  select(Numéro,Nom,Forme,Génération,Taux)%>%View



