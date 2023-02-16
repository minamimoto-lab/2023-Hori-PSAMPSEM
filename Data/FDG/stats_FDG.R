df_FDG = read.csv("df_PSAM_FDG.csv")

statsFDG = summary(aov(data=df_FDG,suvr~cond))
statsFDG

statFDGTukey = TukeyHSD(aov(data=df_FDG,suvr~cond))
statFDGTukey
