df_FDG = read.csv("df_PSAM_FDG.csv")

df_FDG = transform(df_FDG,cond=factor(cond,levels=c("Veh","u817-1","u817-10","u817-100","u817-300","u792-100","Vare-100")))

df_FDG_mean= df_FDG %>%
  group_by(cond) %>%
  summarise(mean=mean(suvr))

df_FDG_mean = transform(df_FDG_mean,cond=factor(cond,levels=c("Veh","u817-1","u817-10","u817-100","u817-300","u792-100","Vare-100")))

xlabs = c("Vehicle","1","10","100","300","100","100")

gFDG = ggplot(df_FDG,mapping = aes(x=cond,y=suvr))+
  geom_col(data=df_FDG_mean,aes(x=cond,y=mean,fill=cond),color="black",width=0.6)+
  geom_point(mapping=aes(shape=Region),size=3,color="black",fill="white")+
  scale_fill_manual(values=c("#000000",rep("#FF0D0D",4),"#F19A69","#F785F2"))+
  scale_shape_manual(values=c(21,22,23,24))+
  theme_classic()+
  #  theme(legend.position = "none")+
  theme(#legend.position = c(0.02,1),
    #legend.justification=c(0,1),
    legend.box = "vertical",
    #legend.key.height = unit(5,"mm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_text(size=6,color="black"),
    axis.text.y=element_text(size=16,color="black"),
    axis.title=element_text(size=16))+
  labs(x="",y="Activity (%Vehicle)")+
  scale_x_discrete(labels=xlabs)+
  scale_y_continuous(expand=c(0,NA),limits=c(0,140),breaks=seq(10,200,30))

gFDG

ggsave("fig/FDG_results.pdf",gFDG,width=6,height=5)

