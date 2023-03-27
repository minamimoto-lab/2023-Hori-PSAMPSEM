df_FDG = read.csv("df_PSAM_FDG_striatum.csv")

df_FDG = df_FDG[df_FDG$cond!="Veh",]
df_FDG = transform(df_FDG,cond=factor(cond,levels=c("u817-1","u817-10","u817-100","u817-300","u792-100","vare-100")))

xlabs = c("1","10","100","300","100","100")

# boxplot
gFDG_box = ggplot(df_FDG,mapping = aes(x=cond,y=suvrd))+
  geom_vline(xintercept=c(4.5,5.5),color="#AAAAAA",linetype="dashed")+
  geom_hline(yintercept=0,color="#AAAAAA",linetype="dashed")+
  geom_boxplot(aes(fill=cond),width=0.6)+
  geom_point(mapping=aes(shape=Region),size=4,color="black",fill="white")+
  scale_fill_manual(values=c(rep("#FF0D0D",4),"#F19A69","#F785F2"))+
  scale_shape_manual(values=c(21,22,23,24))+
  annotate("text",x=2.5,y=23,label="uPSEM817",size=5)+
  annotate("text",x=5,y=23,label="uPSEM792",size=5)+
  annotate("text",x=6,y=23,label="Varenicline",size=5)+
  theme_classic()+
  theme(legend.box = "vertical",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_text(size=16,color="black"),
    axis.text.y=element_text(size=16,color="black"),
    axis.title=element_text(size=16))+
  labs(x="",y="Activity (%changes from Vehicle)")+
  scale_x_discrete(labels=xlabs)+
  coord_cartesian(ylim=(c(-5,25)))
