


dades=fread("../../data/kobe-bryant-shot-selection/data.csv")
# dades[!is.na(shot_made_flag)][,.(Mitjana=mean(shot_made_flag),desviacio_standar=sd(shot_made_flag),.N),by=.(action_type,shot_type)]
dades[,game_date:=as.Date(game_date,"%Y-%m-%d ")]
dades[,seconds_from_start:=((11-minutes_remaining)*60+(60-seconds_remaining))+12*60*(period-1)]

dades[,points:=as.numeric(substr(shot_type,0,1))]
dades[,season:=as.numeric(substr(season,0,4))]
dades[,matchup:=NULL]

setkeyv(dades,c("game_date","seconds_from_start"))


# are you sure?-------------------
dades = dades[!shot_id %in% c(dades[points==2& shot_distance>23,shot_id],
                              dades[points==3& shot_distance<20,shot_id])]

ggplot_basket_court +
  geom_point(data=dades,aes(loc_x,loc_y,colour=as.factor(shot_made_flag)),size=0.1)

ggplot_basket_court + geom_point( data = dades, aes( loc_x, loc_y, colour=as.factor(points)),size=0.1)
ggplot_basket_court + geom_point( data = dades, aes( loc_x, loc_y, colour=as.factor(shot_zone_area)),size=0.1)
ggplot_basket_court + geom_point( data = dades, aes( loc_x, loc_y, colour=as.factor(shot_zone_basic)),size=0.1)
ggplot_basket_court + geom_point( data = dades, aes( loc_x, loc_y, colour=as.factor(shot_zone_range)),size=0.1)
ggplot_basket_court + geom_point( data = dades, aes( loc_x, loc_y, colour=shot_distance),size=0.1)+ scale_colour_gradient2(low="red",midpoint = 22 ,high="blue")
ggplot_basket_court + geom_point( data = dades, aes( loc_x, loc_y, colour=as.factor(combined_shot_type)),size=0.1)

# media por variables ------------------------------------------------------
variables=names(dades)
variables=setdiff(variables,c("shot_id","game_date","team_id","team_name","lon","loc_y","loc_x","lat","game_id","game_event_id","shot_made_flag","shot_type"))
variables=c(variables,c("sign(loc_x)","sign(loc_y)"))

bys = lapply(as.list(variables),function(x){parse(text =x)})
list_of_agregations=list()
for (variable in bys)
  list_of_agregations[[gsub("[^a-z]","",paste(variable))]] =dades[!is.na(shot_made_flag),.(Mitjana=mean(shot_made_flag),desviacio_standar=sd(shot_made_flag),.N),by=.(eval(variable) )]

lapply(list_of_agregations,nrow)
lapply(list_of_agregations,function(x){class(x$variable)})
list_of_agregations$season

for (variable in names(list_of_agregations))
{
  if(class(list_of_agregations[[variable]]$variable)=="character")
    {p=ggplot(list_of_agregations[[variable]],aes(variable,Mitjana))+
      geom_point()+
      geom_errorbar(aes(ymin=Mitjana-desviacio_standar/sqrt(N), ymax=Mitjana+desviacio_standar/sqrt(N)),group=1,alpha=0.2)+
        ggtitle(variable)+
        xlab(variable)+
        theme(axis.text.x=element_text(angle=90))
  }else{
    p=ggplot(list_of_agregations[[variable]],aes(variable,Mitjana))+
      geom_line(group=1)+
      geom_ribbon(aes(ymin=Mitjana-desviacio_standar/sqrt(N), ymax=Mitjana+desviacio_standar/sqrt(N)),group=1,alpha=0.2)+
      ggtitle(variable)+
      xlab(variable)+
      theme(axis.text.x=element_text(angle=90))
  }

    plot( p )
}


ggplot_basket_court +
  geom_point(data=dades,aes(loc_x,loc_y,colour=as.factor(shot_made_flag)),size=0.1)
  

# media de puntos por tiro---------------------------------------
p1 = dades[!is.na(shot_made_flag)][,.(Mitjana=mean(shot_made_flag),desviacio_standar=sd(shot_made_flag),.N),by=points]
p1[,points*Mitjana]

# dades[game_id==21501228,.(period,minutes_remaining,seconds_remaining,seconds_from_start,shot_made_flag)]

# dades[game_id==21501228 & !is.na(shot_made_flag),.(seconds_from_start,score=cumsum(shot_made_flag*points))]

ggplot(dades[season==2002 & !is.na(shot_made_flag),.(seconds_from_start,score=cumsum(shot_made_flag*points)),by=game_id])+
  geom_line(aes(seconds_from_start,score,colour=game_id,group=game_id))


ggplot(dades[season==2015 & !is.na(shot_made_flag),.(seconds_from_start,score=cumsum(shot_made_flag*points)),by=game_id])+
  geom_line(aes(seconds_from_start,score,colour=game_id,group=game_id))





