
# Correlation with extremes removed
Sable_Spec_Cor <- renum(data.frame(Freq = as.numeric(names(Sable_2017_2019_noEx[, 2:1155])), Cor = cor(Sable_2017_2019_noEx[, 2:1155], Sable_2017_2019_noEx$TMA, use = "na.or.complete")))
sort.f(Sable_Spec_Cor, 'Cor', rev = T)[1:5,]
  Freq       Cor
1 4081 0.7233784
2 4073 0.7233294
3 4089 0.7229155
4 4066 0.7228655
5 4058 0.7220165

dev.new()
plot(Sable_Spec_Cor$Freq, Sable_Spec_Cor$Cor)

dev.new()
plot(Sable_2017_2019_noEx[, '4081'], Sable_2017_2019_noEx$TMA)

dev.new()
plot(jitter(Sable_2017_2019_noEx[, '4081']), jitter(Sable_2017_2019_noEx$TMA))

cor(Sable_2017_2019_noEx[, '4081'], Sable_2017_2019_noEx$TMA,  use = "na.or.complete")
[1] 0.7233784


# Use correlation plot to define the best freg. areas to use [using identify()]

Sable_Spec_Cor[c(936, 1148), ]
     Freq           Cor
936  5277 0.46227826962
1148 3641 0.46627681593


# WB = Wide Best area from correlation plot
Sable_WB <- d[d$Band >= 3641 & d$Band <= 5277, ]
dim(Sable_WB)

Bands <- as.numeric(names(Sable_2017_2019_noEx[, 2:1155]))
Bands.WB <- Bands[Bands >= 3641 & Bands <= 5277]
Bands.TF <- Bands %in% Bands.WB
len(Bands.TF) # 1154
sum(Bands.TF) #   213

dim(Sable_2017_2019_noEx)
[1] 1560 1175

Sable_2017_2019_WB <- Sable_2017_2019_noEx[, c(T, Bands.TF, rep(T, len(1156:1175)))]
Sable_2017_2019_WB <- Sable_2017_2019_WB[!is.na(Sable_2017_2019_WB$TMA), ]

plotly.Spec(Sable_2017_2019_WB, 'all')

plotly.Spec(Sable_2017_2019_WB, 300, colorGroup = 'scanGroup')


d <- plotly.Spec(Sable_2017_2019_WB[!is.na(Sable_2017_2019_noEx$TMA), ], 'all', colorGroup = 'TMA', plot = FALSE) 
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 





























