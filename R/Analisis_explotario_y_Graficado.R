


#####
#####  Este es un ejemplo de tratamiento de datos, graficación y 
#####  estadística utilizados en la tesis.
#####




library("xlsx")
library(ggplot2)
library("psych")
library(dplyr)
library(tidyr)
library(officer)
library(rvg)
library(plotly)
library("ggsci")
library("gridExtra")
library("agricolae")



setwd("C:/Users/GermanBo/Desktop/Entorno_R/articulo_Fesbi") 
list.files()




###
### Hormonas 
###


hr_h_IAA <- read.xlsx("hormonas_articulo.xlsx",1 , header = TRUE)
hr_h_ABA <- read.xlsx("hormonas_articulo.xlsx",2 , header = TRUE)
hr_h_SA <- read.xlsx("hormonas_articulo.xlsx",3 , header = TRUE)
hr_r_IAA <- read.xlsx("hormonas_articulo.xlsx",4 , header = TRUE)
hr_r_ABA <- read.xlsx("hormonas_articulo.xlsx",5 , header = TRUE)
hr_r_SA <- read.xlsx("hormonas_articulo.xlsx",6 , header = TRUE)

x <- list (hr_h_IAA,hr_h_ABA,hr_h_SA,hr_r_IAA,hr_r_ABA,hr_r_SA)




hormonas = function(i) {
  d <- i %>% gather(hormona, valor ,3:7) %>%  separate(hormona, c("hormonas","tejido"), sep = "_") %>% 
    separate(tejido, c("tejidos","."), sep = "\\.(?=\\d+)")
  
  d1 <- d %>%  filter(t == 1)
  modelo_anova <- with(data = d1 , aov( valor ~ trat))
  resultado_tukey <- HSD.test (modelo_anova, "trat", group = TRUE)
  print(resultado_tukey$groups)
  tabla_tukey_ <- data.frame(resultado_tukey$groups) #damos un data.frame para los grupos
  tabla_tukey_ <- tibble::rownames_to_column(tabla_tukey_, var = "trat")
  tuki_<- tabla_tukey_
  tuki_1 <- tuki_ %>%  mutate (t =1)
  
  d3 <- d %>%  filter(t == 3)
  modelo_anova <- with(data = d3 , aov( valor ~ trat))
  resultado_tukey <- HSD.test (modelo_anova, "trat", group = TRUE)
  print(resultado_tukey$groups)
  tabla_tukey_ <- data.frame(resultado_tukey$groups) #damos un data.frame para los grupos
  tabla_tukey_ <- tibble::rownames_to_column(tabla_tukey_, var = "trat")
  tuki_<- tabla_tukey_
  tuki_3 <- tuki_ %>%  mutate (t =3)
  
  d6 <- d %>%  filter(t == 6)
  modelo_anova <- with(data = d6 , aov( valor ~ trat))
  resultado_tukey <- HSD.test (modelo_anova, "trat", group = TRUE)
  print(resultado_tukey$groups)
  tabla_tukey_ <- data.frame(resultado_tukey$groups) #damos un data.frame para los grupos
  tabla_tukey_ <- tibble::rownames_to_column(tabla_tukey_, var = "trat")
  tuki_<- tabla_tukey_
  tuki_6 <- tuki_ %>%  mutate (t =6)
  
  fus <- rbind(tuki_1,tuki_3,tuki_6)
  i <- merge(d , fus,by= c("trat", "t") )

  }


hor <- x %>% map_df(hormonas)


write.table(hor, file = "graficado_hormonas_FeSBi_Art.txt", append = FALSE, sep = "/", dec = ".",
            row.names = TRUE, col.names = TRUE)

# lectura y graficado

hor <- read.table("graficado_hormonas_FeSBi_Art.txt", header = TRUE, sep= "/" , dec = ".")



hor$trat <- sub(" Fe+", "Fe+", hor$trat , fixed = TRUE)
hor$trat <- sub("FeS_1", "F1",hor$trat , fixed = TRUE)
hor$trat <- sub("FeS_2", "F2",hor$trat , fixed = TRUE)
hor$trat <- sub("FeBi_1", "FB1",hor$trat , fixed = TRUE)
hor$trat <- sub("FeBi_2", "FB2",hor$trat , fixed = TRUE)


hor$trat <- factor(hor$trat,levels =  c("Fe+", "Bic+","Fe-", "Bic-", "F1", "FB1", "F2", "FB2"))



# IAA hoja

hor_h_IAA <- hor %>% filter(hormonas =="IAA") %>% filter(tejidos =="hoja")

x <- ggplot (hor_h_IAA,  aes(x = trat, y = valor.x, fill = trat)) + geom_boxplot() + 
  facet_wrap(~ t, scales = "fixed" ) + 
  theme_bw() +  scale_fill_manual(values=c('#ED0000FF','#AD002AFF','#0D3B66','#925E9FFF','#00743F','#8BC34A','#FFC300FF','#B8860B'))+
  labs(title  ="IAA en hoja ", x = "Tratamiento", y = "pmoles/g" ) + theme_bw() + 
  geom_text(aes(label = groups, y = valor.y + valor.y/5 ),  
            size = 5)

x


doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 9, height = 4, left = 1, top = 2 ) # tamaño adaptado a 3 graficas de cajas
print(doc, target = "graficas_FeSBi_cap_III.xlsx")




# ABA hoja 

hor_h_ABA <- hor %>% filter(hormonas =="ABA") %>% filter(tejidos =="hoja")

x <- ggplot (hor_h_ABA,  aes(x = trat, y = valor.x, fill = trat)) + geom_boxplot() + 
  facet_wrap(~ t, scales = "fixed" ) + 
  theme_bw() +  scale_fill_manual(values=c('#ED0000FF','#AD002AFF','#0D3B66','#925E9FFF','#00743F','#8BC34A','#FFC300FF','#B8860B'))+
  labs(title  ="ABA en hoja ", x = "Tratamiento", y = "pmoles/g" ) + theme_bw() + 
  geom_text(aes(label = groups, y = valor.y + valor.y/5 ),  
            size = 5)

x


doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 9, height = 4, left = 1, top = 2 ) # tamaño adaptado a 3 graficas de cajas
print(doc, target = "graficas_FeSBi_cap_III.xlsx")





# SA

hor_h_SA <- hor %>% filter(hormonas =="SA") %>% filter(tejidos =="hoja")

x <- ggplot (hor_h_SA,  aes(x = trat, y = valor.x, fill = trat)) + geom_boxplot() + 
  facet_wrap(~ t, scales = "fixed" ) + 
  theme_bw() +  scale_fill_manual(values=c('#ED0000FF','#AD002AFF','#0D3B66','#925E9FFF','#00743F','#8BC34A','#FFC300FF','#B8860B'))+
  labs(title  ="SA en hoja ", x = "Tratamiento", y = "pmoles/g" ) + theme_bw() + 
  geom_text(aes(label = groups, y = valor.y + valor.y/5 ),  
            size = 5)

x


doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 9, height = 4, left = 1, top = 2 ) # tamaño adaptado a 3 graficas de cajas
print(doc, target = "graficas_FeSBi_cap_III.xlsx")












###
### Expresion genica en referencia al control positivo 
###

setwd("C:/Users/GermanBo/Desktop/Entorno_R/articulo_Fesbi") 
list.files()



# Graficado  



fuss <- read.table("graficado_expresion_gen_Fe+_FeSBi_R.txt", header = TRUE, sep= "/" , dec = ".")

fuss$trat <- sub("FeS_1", "F1", fuss$trat , fixed = TRUE)
fuss$trat <- sub("FeBi_1", "FB1",fuss$trat , fixed = TRUE)
fuss$trat <- sub("FeS_2", "F2",fuss$trat , fixed = TRUE)
fuss$trat <- sub("FeBi_2", "FB2",fuss$trat , fixed = TRUE)

fuss$dia <- sub("dia1", "1",fuss$dia , fixed = TRUE)
fuss$dia <- sub("dia3", "3",fuss$dia , fixed = TRUE)
fuss$dia <- sub("dia6", "6",fuss$dia , fixed = TRUE)


fuss$sd <-  as.numeric(fuss$sd)
fuss$exp <-  as.numeric(fuss$exp)
fuss$Días <- as.factor(fuss$dia)



fuss$trat <- factor(fuss$trat,levels = c("Fe+", "Bic+","Fe-","Bic-","F1","FB1" , "F2", "FB2"))
fuss <- fuss %>% filter(!is.na(trat))





###  Graficado 


exp_FIT <- fuss %>% filter(gen == "FIT")
class(exp_FIT$exp)

x <- ggplot(exp_FIT, aes(x = trat, y = exp , fill = Días)) + 
  geom_bar (stat = "identity", position=position_dodge(width = 0.8), width = 0.95, color = "Black") +
  geom_errorbar(data = exp_FIT, aes( ymin = exp - sd , ymax = exp + sd, y = sd), width=.2,position=position_dodge(.8))+
  theme_bw() +   labs(title  ="Expresión log de FIT    ", x = "Tratamiento", y = "Expr Log" )+scale_fill_manual(values =c("#FFFFFF", "#C4C4C4", "#000000")) +
  geom_text(aes(label = Sig, y = exp + sd + sd ), position = position_dodge(width = 0.8), 
            size = 8)


x

doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 6, height = 4, left = 1, top = 2 )
print(doc, target = "graficas_HFeG_6_B.xlsx")





exp_FRO2 <- fuss %>% filter(gen == "FRO2")

x <- ggplot(exp_FRO2, aes(x = trat, y = exp , fill = Días)) + 
  geom_bar (stat = "identity", position=position_dodge(width = 0.8), width = 0.95, color = "Black") +
  geom_errorbar(data = exp_FRO2, aes( ymin = exp - sd , ymax = exp + sd, y = sd), width=.2,position=position_dodge(.8))+
  theme_bw() +   labs(title  ="Expresión log de FRO1    ", x = "Tratamiento", y = "Expr Log" )+scale_fill_manual(values =c("#FFFFFF", "#C4C4C4", "#000000")) +
  geom_text(aes(label = Sig, y = exp + sd + sd ), position = position_dodge(width = 0.8), 
            size = 8)


x


doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 6, height = 4, left = 1, top = 2 )
print(doc, target = "graficas_HFeG_6_B.xlsx")








###
### SN RB y derivados HPLC  
###


rb <- read.xlsx("ajuste_SN_HPLC_FeSBi.xlsx",1, header = TRUE)
rb_1 <- rb %>% rename ( Rb = 5, krb =6, Lf = 7 , Lc = 8)  %>%  gather(Molecula, pmol.ml , 5:8)

rb_4 <- rb_1 %>%  mutate( Tratamiento = case_when(Tratamiento == "FeS1" ~ "F1",
                                                  Tratamiento == "FeS2" ~ "F2",
                                                  Tratamiento == "-Fe" ~ "Fe-",
                                                  Tratamiento == "BIC-" ~ "Bic-",
                                                  Tratamiento == "BIC+" ~ "Bic+",
                                                  Tratamiento == "FeS1_BIC" ~ "FB1",
                                                  Tratamiento == "FeS2_BIC" ~ "FB2",
                                                  TRUE ~ Tratamiento))

rb_4$Tratamiento <- factor(rb_4$Tratamiento,levels = c("Fe-", "Fe+","Bic-", "Bic+", "F1", "F2", "FB1", "FB2"))
rb_4$Tratamiento <- factor(rb_4$Tratamiento,levels =  c("Fe+", "Bic+","Fe-", "Bic-", "F1", "FB1", "F2", "FB2"))

rb_4 <- rb_4 %>%  filter(!is.na(Tratamiento))
ggplot( rb_4, aes( x = Molecula , y = pmol.ml, color = Tratamiento)) +geom_boxplot()

rb_5 <- rb_4 %>% group_by(Tratamiento, Dia,Molecula) %>%
  summarise( mean_pmol.ml = mean(pmol.ml), sd_pmol.ml = sd(pmol.ml)/ sqrt(length(pmol.ml)) ) # aunque aqui este puesto nM/g, la medida es pM por ml


# Graficas 

merge <- rb_5

merge_Rb<- merge %>% filter( Molecula == "Rb" )

x <- ggplot(data = merge_Rb, aes(x = Dia, y = mean_pmol.ml, color  =  Tratamiento )) + geom_line(size=1) + geom_point(size = 3)+
  geom_errorbar(data = merge_Rb, aes( ymin = mean_pmol.ml- sd_pmol.ml , ymax = mean_pmol.ml + sd_pmol.ml, y = mean_pmol.ml), width=0.1,size=1) +
  theme_bw() + 
  scale_color_manual(values=c('#ED0000FF','#AD002AFF','#0D3B66','#925E9FFF','#00743F','#8BC34A','#FFC300FF','#B8860B'))+
  scale_x_continuous(n.breaks = 6) + 
  labs(title  ="  Riboflavinas en SN  ", x = "Días", y = " pmol/ml" ) 

x


doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 6, height = 4, left = 1, top = 2 )
print(doc, target = "graficas_FeSBi_cap_III.xlsx")






merge_krb <- merge %>% filter( Molecula == "krb" )


x <- ggplot(data = merge_krb, aes(x = Dia, y = mean_pmol.ml, color  =  Tratamiento )) + geom_line(size=1) + geom_point(size = 3)+
  geom_errorbar(data = merge_krb, aes( ymin = mean_pmol.ml- sd_pmol.ml , ymax = mean_pmol.ml + sd_pmol.ml, y = mean_pmol.ml), width=0.1,size=1) +
  theme_bw() + 
  scale_color_manual(values=c('#ED0000FF','#AD002AFF','#0D3B66','#925E9FFF','#00743F','#8BC34A','#FFC300FF','#B8860B'))+
  scale_x_continuous(n.breaks = 6) + 
  labs(title  ="  4-keto-riboflavinas en SN  ", x = "Días", y = " pmol/ml" ) 

x


doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 6, height = 4, left = 1, top = 2 )
print(doc, target = "graficas_FeSBi_cap_III.xlsx")








###
### Fluorometria 370nm
###



setwd("C:/Users/GermanBo/Desktop/Entorno_R/FeSBi/csv_FeSBi") 


dfs_merge_4 <- read.table("370nm_FeSBi_R.txt", header = TRUE, sep= "/" , dec = ".")
df <- dfs_merge_4 %>% filter (long == 520)

ggplot (df, aes(x = t, y = nm, color = trat)) + geom_line()

df_1 <- df %>% group_by( trat,t) %>% summarise( mean_int = mean(nm), sd_int = sd(nm)/ sqrt(length(nm)))
df_1$trat <- factor(df_1$trat,levels =  c("Fe+", "Bic+", "Fe-", "Bic-", "FeS1", "FeBi1", "FeS2", "FeBi2"))

# Graficado 

x <- ggplot(data = df_1, aes(x = t, y = mean_int, color  = trat)) + geom_line(size=1)+ geom_point(size=3)+
  geom_errorbar(data = df_1, aes( ymin = mean_int - sd_int , ymax = mean_int + sd_int, y = mean_int), width=0.1,size=1) +
  theme_bw() + scale_color_manual(values=c('#ED0000FF','#AD002AFF','#0D3B66','#925E9FFF','#00743F','#8BC34A','#FFC300FF','#B8860B'))+
  labs(title  ="Intensidad a 520 nm con excitacion 370", x = "Días", y = "Intensidad" ) 

x


doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 6, height = 4, left = 1, top = 2 )
print(doc, target = "graficas_FeSBi_cap_III.xlsx")







###
### SPAD 
### 


setwd("C:/Users/GermanBo/Desktop/Entorno_R/FeSBi/SPAD")   
list.files()

spad_1 <- read.xlsx("spad_FeSBi_R.xlsx", 1 , header = TRUE)
spad_1$trat <- factor(spad_1$trat,levels = c("Fe-" , "Fe+", "Bic-", "Bic+", "FeS_1", "FeS_2", "FeBi_1", "FeBi_2"))
spad_1$trat <- sub("FeS_1", "F1",spad_1$trat, fixed = TRUE)
spad_1$trat <- sub("FeS_2", "F2",spad_1$trat, fixed = TRUE)
spad_1 <- spad_1 %>%  filter(!is.na(trat))

spad_2 <- spad_1 %>% group_by( trat,t) %>% summarise( mean_chl = mean(Chl), sd_chl = sd(Chl)/ sqrt(length(Chl)))


ggplot(data = spad_2, aes(x = t, y = mean_chl, color  = trat)) + geom_line(size=1)+ geom_point(size=3)+
  geom_errorbar(data = spad_2, aes( ymin = mean_chl - sd_chl , ymax = mean_chl + sd_chl, y = mean_chl),
                width=0.1,size=1)


# Estadistica

modelo_anova <- with(data = subset(spad_1, t %in% c("6")), aov(Chl ~ trat))
resultado_tukey <- HSD.test (modelo_anova, "trat", group = TRUE)
print(resultado_tukey$groups)


tabla_tukey_ <- data.frame(resultado_tukey$groups) #damos un data.frame para los grupos
tabla_tukey_ <- tibble::rownames_to_column(tabla_tukey_, var = "trat")

tuki_<- tabla_tukey_
tuki_6 <- tuki_ %>%  mutate (t = 6)


tuki <- rbind( tuki_1,tuki_2,tuki_3,tuki_4,tuki_5, tuki_6)
merge <- merge(spad_2,tuki , by= c("trat", "t"))


# Guardado
write.table(merge, file = "graficado_SPAD_FeSBi_Articulo.txt", append = FALSE, sep = "/", dec = ".",
            row.names = TRUE, col.names = TRUE)

# Graficas

merge <- read.table("graficado_SPAD_FeSBi_Articulo.txt", header = TRUE, sep= "/" , dec = ".")

merge$trat <- factor(merge$trat,levels =  c("Fe+", "Bic+", "Fe-", "Bic-", "F1", "FeBi_1", "F2", "FeBi_2"))


x <- ggplot(data = merge, aes(x = t, y = mean_chl, color  = trat)) + geom_line(size=1)+ geom_point(size=3)+
  geom_errorbar(data = merge, aes( ymin = mean_chl - sd_chl , ymax = mean_chl + sd_chl, y = mean_chl), width=0.1,size=1) +
  theme_bw() + scale_color_manual(values=c('#ED0000FF','#AD002AFF','#0D3B66','#925E9FFF','#00743F','#8BC34A','#FFC300FF','#B8860B'))+
  labs(title  ="Nivel de Chl en la primera hoja", x = "Días", y = "Chl en hoja" ) +
  geom_text(aes(label = groups, y = mean_chl + sd_chl + sd_chl/2 ),  
            size = 5)

x


doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 6, height = 4, left = 1, top = 2 )
print(doc, target = "graficas_FeSBi_cap_III.xlsx")






###
### Biomasa 
###

ms <- read.xlsx("tablas_FeSbi.xlsx",1 , header = TRUE)
ms_hoja <- ms %>%  gather(trat, g , 2:9) 

ms <- read.xlsx("tablas_FeSbi.xlsx",2 , header = TRUE)
ms_riz <- ms %>%  gather(trat, g , 2:9)

ms_2 <- rbind(ms_hoja,ms_riz)

ms_2$trat <- sub("Bicmas", "Bic+",ms_2$trat, fixed = TRUE)
ms_2$trat <- sub("Bic_", "Bic-",ms_2$trat, fixed = TRUE)
ms_2$trat <- sub("Femas", "Fe+",ms_2$trat, fixed = TRUE)
ms_2$trat <- sub("Fe_", "Fe-",ms_2$trat, fixed = TRUE)




# Estadistica 

modelo_anova <- with(data = subset(ms_2, tejido  == "root"), aov(g ~ trat))
resultado_tukey <- HSD.test (modelo_anova, "trat", group = TRUE)
print(resultado_tukey$groups)

tabla_tukey_ <- data.frame(resultado_tukey$groups) #damos un data.frame para los grupos
tabla_tukey_ <- tibble::rownames_to_column(tabla_tukey_, var = "trat")

tuki_<- tabla_tukey_
tuki_root <- tuki_ %>%  mutate (t = "root")

tuki<- rbind(tuki_shoot,tuki_root) 
tuki <- rename(tuki, tejido= t)

merge <- merge(ms_2,tuki , by= c("trat", "tejido"))

# Guardado 

write.table(merge, file = "graficado_Biomasa_FesBi_Art.txt", append = FALSE, sep = "/", dec = ".",
            row.names = TRUE, col.names = TRUE)


# Graficas

merge <- read.table("graficado_Biomasa_FesBi_Art.txt", header = TRUE, sep= "/" , dec = ".")

merge$trat <- sub("FeS_1", "F1",merge$trat, fixed = TRUE)
merge$trat <- sub("FeS_2", "F2",merge$trat, fixed = TRUE)
merge$trat <- sub("FesBI1", "FB1",merge$trat, fixed = TRUE)
merge$trat <- sub("FeSBi2", "FB2",merge$trat, fixed = TRUE)


merge$trat <- factor(merge$trat,levels =  c("Fe+", "Fe-","F1", "F2", "Bic+", "Bic-", "FB1", "FB2"))


x <- ggplot (merge,  aes(x = trat, y = g.x, fill = trat)) + geom_boxplot() + 
  facet_wrap(~ tejido, scales = "fixed" ) + 
  theme_bw() +    scale_fill_manual(values=c('#ED0000FF','#0D3B66','#00743F','#FFC300FF','#AD002AFF','#925E9FFF','#8BC34A','#B8860B'))+
  labs(title  ="Biomasa ", x = "Tratamiento", y = "g" ) + theme_bw() + 
  geom_text(aes(label = groups, y = g.y + g.y/5 ),  
            size = 5)
x

doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 6, height = 4, left = 1, top = 2 ) 
print(doc, target = "graficas_FeSBi.xlsx")







###
### Concentraciones microelementos Hoja y raiz  
###



icp <- read.xlsx("tablas_FeSbi.xlsx",3 , header = TRUE)

icp_1 <- icp %>%  filter(!is.na(tratamiento)) %>% select(-11 , -12) %>%  gather(trat, ug_g , 3:10) %>%
  separate(tratamiento, c("tip","elemento", "tej"), sep = " ") %>% separate(elemento, c("element","m"), sep = "/g") %>%
  filter(!is.na(ug_g))

icp_1$trat <- sub("Bicmas", "Bic+", icp_1$trat , fixed = TRUE)
icp_1$trat <- sub("Bic_", "Bic-",icp_1$trat , fixed = TRUE)
icp_1$trat <- sub("Femas", "Fe+",icp_1$trat , fixed = TRUE)
icp_1$trat <- sub("Fe_", "Fe-",icp_1$trat , fixed = TRUE)
icp_1$ug_g <- as.numeric(icp_1$ug_g)
icp_1 <- icp_1 %>% filter(!is.na(ug_g))

# estadistica 

modelo_anova <- with(data = subset(icp_1, tejido  == "root" & element == "Mn"), aov(ug_g ~ trat))
resultado_tukey <- HSD.test (modelo_anova, "trat", group = TRUE)
print(resultado_tukey$groups)


tabla_tukey_ <- data.frame(resultado_tukey$groups) #damos un data.frame para los grupos
tabla_tukey_ <- tibble::rownames_to_column(tabla_tukey_, var = "trat")

tuki_<- tabla_tukey_
tuki_rr_mn <- tuki_ %>%  mutate (tejido = "root") %>%  mutate (element = "Mn")


tuki <- rbind(tuki_r_fe,tuki_r_cu,tuki_r_zn,tuki_r_mn,tuki_rr_fe,tuki_rr_cu,tuki_rr_zn,tuki_rr_mn )



merge <- merge(icp_1,tuki , by= c("trat", "tejido", "element"))

### Grabado 

write.table(merge, file = "graficado_micros_Art.txt", append = FALSE, sep = "/", dec = ".",
            row.names = TRUE, col.names = TRUE)


# Graficado

merge <- read.table("graficado_micros_Art.txt", header = TRUE, sep= "/" , dec = ".")
merge_hoja <- merge %>% filter(tejido=="shoot")

merge_hoja$trat <- factor(merge_hoja$trat,levels =  c("Fe+", "Bic+","Fe-", "Bic-", "F1", "FB1", "F2", "FB2"))
merge_hoja$element <- factor(merge_hoja$element,levels =  c("Fe", "Cu", "Zn", "Mn"))



x <- ggplot (merge_hoja,  aes(x = trat, y = ug_g.x, fill = trat)) + geom_boxplot() + 
  facet_wrap(~ element, scales = "free" ) + 
  theme_bw() +    scale_fill_manual(values=c('#ED0000FF','#AD002AFF','#0D3B66','#925E9FFF','#00743F','#8BC34A','#FFC300FF','#B8860B'))+
  labs(title  ="Microelementos en hoja ", x = "Tratamiento", y = "µg/g" ) + theme_bw() + 
  geom_text(aes(label = groups, y = ug_g.y + ug_g.y/5 ),  
            size = 5)

x
doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 7, height = 5, left = 1, top = 2 ) # tamaño adaptado a 3 graficas de cajas
print(doc, target = "graficas_FeSBi.xlsx")







###
### QR
###


qr_3y6d <- read.xlsx("qr_FeSBi_R.xlsx",1, header = TRUE)


qr_3y6d <- qr_3y6d %>%  filter(!is.na(trat))


ggplot ( qr_3y6d, aes(x = trat, y = mmol...gPF...hora, fill = trat)) + 
  geom_boxplot() + facet_wrap(~ t )+
  theme_bw() +   scale_fill_lancet()+ labs(title  ="Actividad QR en condiciones normales", x = "Días", y = "mmol/gPF/hora    " )


# Estadistica

modelo_anova <- with(data = subset(qr_3y6d, t %in% c("1")), aov( mmol...gPF...hora ~ trat))
resultado_tukey <- HSD.test (modelo_anova, "trat", group = TRUE)
print(resultado_tukey$groups)


tabla_tukey_ <- data.frame(resultado_tukey$groups) #damos un data.frame para los grupos
tabla_tukey_ <- tibble::rownames_to_column(tabla_tukey_, var = "trat")

tuki_<- tabla_tukey_
tuki_1 <- tuki_ %>%  mutate (t =1)


tuki <- rbind(tuki_1,tuki_3,tuki_6)

merge <- merge(qr_3y6d,tuki , by= c("trat", "t"))


# Guardado

write.table(merge, file = "graficado_QR_FeSBi_Art.txt", append = FALSE, sep = "/", dec = ".",
            row.names = TRUE, col.names = TRUE)


# Graficas

merge <- read.table("graficado_QR_FeSBi_Art.txt", header = TRUE, sep= "/" , dec = ".")


merge$trat <- sub("FeS1", "F1",merge$trat, fixed = TRUE)
merge$trat <- sub("FeS2", "F2",merge$trat, fixed = TRUE)
merge$trat <- sub("FeBi1", "FB1",merge$trat, fixed = TRUE)
merge$trat <- sub("FeBi2", "FB2",merge$trat, fixed = TRUE)

merge$trat <- factor(merge$trat,levels =  c("Fe+", "Bic+","Fe-", "Bic-", "F1", "FB1", "F2", "FB2"))


x <- ggplot ( merge, aes(x = trat, y = mmol...gPF...hora.x, fill = trat)) + 
  geom_boxplot() + facet_wrap(~ t , scales="fixed" ) + 
  theme_bw() +      scale_fill_manual(values=c('#ED0000FF','#AD002AFF','#0D3B66','#925E9FFF','#00743F','#8BC34A','#FFC300FF','#B8860B'))+
  labs(title  ="Actividad QR ", x = "Días", y = "mmol/gPF/hora    " ) +
  geom_text(aes(label = groups, y = mmol...gPF...hora.y + mmol...gPF...hora.y/2 ),  
            size = 5)


x


doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 9, height = 4, left = 1, top = 2 )
print(doc, target = "graficas_FeSBi_cap_III.xlsx")







###
### pH 
###


setwd("C:/Users/GermanBo/Desktop/Entorno_R/FeSBi") 
list.files()



ph_3 <- read.table("ph_FeSBi_R.txt", header = TRUE, sep= "/" , dec = ".")
ph_3$trat <- factor(ph_3$trat,levels = c("Fe+", "Bic+", "Fe-", "Bic-", "FeS_1", "FeBi_1", "FeS_2", "FeBi_2"))
ph_3$trat <- sub("FeS_1", "F1",ph_3$trat, fixed = TRUE)
ph_3$trat <- sub("FeS_2", "F2",ph_3$trat, fixed = TRUE)
ph_3 <- ph_3 %>%  filter(!is.na(trat))


ggplot(ph_3, aes(x = t, y = pH, color = trat)) + geom_line() +geom_point()+  ggtitle("  FeSBi   pH ")

ph_4 <- ph_3 %>% group_by(trat,t) %>% summarise( mean= mean(pH), sd = sd (pH) / sqrt(length(pH)))

write.table(ph_4, file = "pH_Fesbi.txt", append = FALSE, sep = "/", dec = ".",
            row.names = TRUE, col.names = TRUE)


# Graficado 

ph_4$trat <- factor(ph_4$trat,levels = c("Fe+", "Bic+", "Fe-", "Bic-", "FeS1", "FeBi1", "FeS2", "FeBi2"))


x <- ggplot( ph_4, aes( x = t, y = mean, color = trat)) + geom_line(size=1)+ 
  geom_errorbar(data = ph_4, aes( ymin = mean - sd , ymax = mean + sd, y = mean), width=0.1,size=1) +
  theme_bw() +   scale_color_manual(values=c('#0D3B66','#ED0000FF','#00743F','#FFC300FF'))+  scale_x_continuous(n.breaks = 7) + 
  labs(title  ="pH de la SN", x = "Días", y = "pH" )

x
doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(x), 
                 width = 6, height = 4, left = 1, top = 2 )
print(doc, target = "graficas_FeSBi_cap_III.xlsx")



