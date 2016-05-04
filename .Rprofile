require(data.table)
require(ggplot2)
require(lubridate)

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.table(x = xx, y = yy))
}

circle_tree_pints <- circleFun(c(0,0),2*232.5,npoints = 200)[y>70]
circle_free_throw <- circleFun(c(0,142.5),120,npoints = 100)
circle_Center     <- circleFun(c(0, 422.5),120,npoints = 100)

ggplot_basket_court =   ggplot()+geom_rect(data=NULL,aes(xmin = -250, xmax = 250, ymin = -47.5, ymax = 422.5),fill=NA,colour="black")+
  geom_rect(data=NULL,aes(xmin = -80, xmax = 80, ymin = -47.5, ymax = 142.5),fill=NA,colour="black")+
  geom_rect(data=NULL,aes(xmin = -60, xmax = 60, ymin = -47.5, ymax = 142.5),fill=NA,colour="black")+
  geom_rect(data=NULL,aes(xmin = -30, xmax = 30, ymin = -7.5, ymax = -6.5),fill=NA,colour="black")+
  geom_segment(data=NULL,aes(x = -220, y = -47.5, xend = -220, yend = 75.5))+
  geom_segment(data=NULL,aes(x = 220, y = -47.5, xend = 220, yend = 75.5))+
  geom_path(data=circle_free_throw,aes(x,y))+
  geom_path(data=circle_tree_pints,aes(x,y))+
  geom_path(data=circle_Center,aes(x,y))+
  geom_point(data=NULL,aes(x=0,y=0))+
  coord_fixed(ratio = 1)+
  theme(legend.position="none",axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
        panel.background=element_rect(fill=alpha("orange3", 0.6)),
        panel.grid.minor=element_line(size=0.1),
        panel.grid.major=element_line(size=0.1))

# ggplot_basket_court + geom_point( data = dades, aes( loc_x, loc_y, colour=as.factor(points)),size=0.1)

