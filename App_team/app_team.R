# Load packages ----
library(shiny)
library(tidyverse)
library(imager)
library(expm)
library(DT)

pokedex=read.csv("pokedex.csv",sep=";",encoding="latin1")%>%mutate(Pokemon=ifelse(Forme=="",Nom,paste0(Nom," (",Forme,")")))
pokedex=pokedex%>%group_by(Nom)%>%mutate(Numéro=which(Nom==unique(pokedex$Nom)),PV=((2*PV+31+floor(255/4))*50/100)+50+10,across(ATT:VIT,~((2*.+31+floor(255/4))*50/100+5)*1.1))%>%ungroup()%>%arrange(Numéro)
table=read.csv("table.csv",sep=";",encoding="latin1")
attaques=read.csv("attaques.csv",sep=";",encoding="latin1")
groups=hclust(dist(pokedex%>%filter(Violet,Team,is.na(Interdit))%>%select(PV:VIT)%>%as.matrix))
score_team=function(dat,...){
  dat%>%filter(Pokemon%in%...)%>%summarise(score=sum(Score))
}



g_solo=pokedex%>%filter(Violet,Team,is.na(Interdit))%>%summarise(Pokemon,group=cutree(groups,3))
g_duo=pokedex%>%filter(Violet,Team,is.na(Interdit))%>%summarise(Pokemon,group=cutree(groups,4))


score_ind=function(Pokemon,scor){
  return((scor%>%filter(Pokemon_team==Pokemon)%>%summarise(score=sum(score)))$score)
}

ui = navbarPage("Team Pokemon",
                
                tabPanel("Accueil",
                         p("Choisir mode Solo ou Duo.")),
                
                tabPanel("Solo",
                         
                         column(12,plotOutput("plot_team_solo")),
                         br(),
                         br(),
                         
                         fluidRow(column(6,
                                         tableOutput("team_solo")),
                                  column(6,
                                         DT::dataTableOutput("adv_stat_solo"))),
                         br(),
                         br(),
                         
                         fluidRow(column(5,
                                         checkboxGroupInput("team_solo", 
                                                            "Pokemon sélectionnés", 
                                                            choices = (pokedex%>%filter(Violet,Team,is.na(Interdit)))$Pokemon%>%sort,
                                                            selected = c("Spectreval","Malvalame","Feunard","Zoroark (Hisui)","Fulgulairo","Sulfura (Galar)")) ),
                                  column(2,
                                         actionLink("eff1","Tout effacer"),
                                         br(),
                                         br(),
                                         selectInput("adv_solo","Team adverse",
                                                     choices=((pokedex%>%filter(Violet))$Pokemon%>%sort),
                                                     multiple=T)),
                                  column(1,actionButton("valid_solo","Valider"))
                                  
                         )
                         
                         
                         
                ),
                
                tabPanel("Duo",
                         
                         column(12,plotOutput("plot_team_duo")),
                         br(),
                         br(),
                         
                         fluidRow(column(6,
                                         tableOutput("team_duo")),
                                  column(6,
                                         DT::dataTableOutput("adv_stat_duo"))),
                         br(),
                         br(),
                         
                         fluidRow(column(5,
                                         checkboxGroupInput("team_duo", 
                                                            "Pokemon sélectionnés", 
                                                            choices = (pokedex%>%filter(Violet,Team,is.na(Interdit)))$Pokemon%>%sort,
                                                            selected = c("Spectreval","Malvalame","Feunard","Givrali","Drattak","Mustéflott (Mâle)")) ),
                                  column(2,
                                         actionLink("eff2","Tout effacer"),
                                         br(),
                                         br(),
                                         selectInput("adv_duo","Team adverse",
                                                     choices=((pokedex%>%filter(Violet))$Pokemon%>%sort),
                                                     multiple=T)),
                                  column(1,actionButton("valid_duo","Valider")))
                )
)



# Server logic
server <- function(input, output, session) {
  
  solo=reactive({
    expand.grid(P1=(g_solo%>%filter(Pokemon%in%input$team_solo)%>%filter(group==1))$Pokemon,
                P2=(g_solo%>%filter(Pokemon%in%input$team_solo)%>%filter(group==2))$Pokemon,
                P3=(g_solo%>%filter(Pokemon%in%input$team_solo)%>%filter(group==3))$Pokemon)%>%
      rowwise%>%mutate(Nom=paste(c(P1,P2,P3),collapse=" / "))
  })
  
  adv_stats_solo=reactive({pokedex%>%filter(Pokemon%in%c(input$team_solo,input$adv_solo))%>%select(Pokemon,PV:VIT)%>%arrange(desc(VIT))})
  
  output$adv_stat_solo=DT::renderDataTable({
    datatable(adv_stats_solo(),
              options = list("pageLength" = 12))
  })
  
  score_solo=eventReactive(input$valid_solo,{
    crossing(pokedex%>%filter(Pokemon%in%c(input$team_solo))%>%select(Pokemon,Type.1:VIT)%>%rename_all(~paste0(.,"_team")),
             pokedex%>%filter(Pokemon%in%c(input$adv_solo))%>%select(Pokemon,Type.1:VIT)%>%rename_all(~paste0(.,"_adv")))%>%
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
      select(Pokemon_team,Pokemon_adv,score)%>%ungroup
  })
  
  
  team_solo=eventReactive(input$valid_solo,{
    solo()%>%mutate(across((starts_with("P")),~score_ind(.,score_solo())),Score=sum(c_across(starts_with("P"))))%>%arrange(desc(Score))%>%as.data.frame
  })
  plot_team_solo=reactive({
    ggplot()+
      geom_bar(data=score_solo()%>%left_join(g_solo%>%rename(Pokemon_team=Pokemon)),aes(x=Pokemon_adv,y=score,fill=reorder(Pokemon_team,group)),stat="identity",position=position_dodge())+
      geom_hline(yintercept = 0.5,linetype="dashed")+
      scale_fill_brewer(palette="Accent")+
      labs(x="Pokemon",y="Score",fill="Pokemon")
  })
  
  output$team_solo=renderTable({
    team_solo()
  })
  
  output$plot_team_solo=renderPlot({
    plot_team_solo()
  })
  
  
  
  duo=reactive({
    expand.grid(P1=(g_duo%>%filter(Pokemon%in%input$team_duo)%>%filter(group==1))$Pokemon,
                P2=(g_duo%>%filter(Pokemon%in%input$team_duo)%>%filter(group==2))$Pokemon,
                P3=(g_duo%>%filter(Pokemon%in%input$team_duo)%>%filter(group==3))$Pokemon,
                P4=(g_duo%>%filter(Pokemon%in%input$team_duo)%>%filter(group==4))$Pokemon)%>%
      rowwise%>%mutate(Nom=paste(c(P1,P2,P3,P4),collapse=" / "))
  })
  
  adv_stats_duo=reactive({pokedex%>%filter(Pokemon%in%c(input$team_duo,input$adv_duo))%>%select(Pokemon,PV:VIT)%>%arrange(desc(VIT))})
  
  output$adv_stat_duo=DT::renderDataTable({
    datatable(adv_stats_duo(),
              options = list("pageLength" = 12))
  })
  
  
  score_duo=eventReactive(input$valid_duo,{
    crossing(pokedex%>%filter(Pokemon%in%c(input$team_duo))%>%select(Pokemon,Type.1:VIT)%>%rename_all(~paste0(.,"_team")),
             pokedex%>%filter(Pokemon%in%c(input$adv_duo))%>%select(Pokemon,Type.1:VIT)%>%rename_all(~paste0(.,"_adv")))%>%
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
      select(Pokemon_team,Pokemon_adv,score)%>%ungroup
  })
  
  team_duo=eventReactive(input$valid_duo,{
    duo()%>%mutate(across((starts_with("P")),~score_ind(.,score_duo())),Score=sum(c_across(starts_with("P"))))%>%arrange(desc(Score))%>%as.data.frame
  })
  plot_team_duo=reactive({
    ggplot()+
      geom_bar(data=score_duo()%>%left_join(g_duo%>%rename(Pokemon_team=Pokemon)),aes(x=Pokemon_adv,y=score,fill=reorder(Pokemon_team,group)),stat="identity",position=position_dodge())+
      geom_hline(yintercept = 0.5,linetype="dashed")+
      scale_fill_brewer(palette="Accent")+
      labs(x="Pokemon",y="Score",fill="Pokemon")
  })
  
  output$team_duo=renderTable({
    team_duo()
  })
  
  output$plot_team_duo=renderPlot({
    plot_team_duo()
  })
  
  
  observe({
    if(input$eff1>0) updateSelectInput(session,"adv_solo",
                                       choices=((pokedex%>%filter(Violet))$Pokemon%>%sort))
  })
  observe({
    if(input$eff2>0) updateSelectInput(session,"adv_duo",
                                       choices=((pokedex%>%filter(Violet))$Pokemon%>%sort))
  })
  
}

# Run the app
shinyApp(ui, server)
