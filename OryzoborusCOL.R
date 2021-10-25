#"Oryzoborus" Rocha et al
#Para cargar paquetes usados
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# Llamando los paquetes
packages <- c("ggplot2","reshape2","tanagR","Rmisc","readxl","tidyr")
ipak(packages)

#Medidas morfológicas entre las diferentes especies####
data<-read.csv("Oryzoborus.csv")
head(data)
data2=melt(as.data.frame(data),id=c("Catalog.Number","Species","Sex"),
                  variable.name = "Variable", value.name = "Medida")
head(data2)

#Culmen####
culmen<-subset(data2,Variable=="Culmen")
table(culmen$Species)

cul_funerea<-subset(culmen,Species=="Sporophila funerea")
#B1: Culmen funerea
R<-5000
B1<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(cul_funerea,.(Catalog.Number))
  boot.sample <- sample(cul_funerea$Medida, replace = TRUE)
  B1[kk] <- mean(boot.sample)
  print(B1)
}
boxplot(B1)
quantile(B1,c(0.025,0.975))
hist(B1, breaks = 30)
dB1<-c(Bmean=mean(B1),quantile(B1,c(0.025,0.975)),n=nrow(cul_funerea))
dB1

cul_angolensis<-subset(culmen,Species=="Sporophila angolensis")
#B2: Culmen angolensis
R<-5000
B2<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(cul_angolensis,.(Catalog.Number))
  boot.sample <- sample(cul_angolensis$Medida, replace = TRUE)
  B2[kk] <- mean(boot.sample)
  print(B2)
}
boxplot(B2)
quantile(B2,c(0.025,0.975))
hist(B2, breaks = 30)
dB2<-c(Bmean=mean(B2),quantile(B2,c(0.025,0.975)),n=nrow(cul_angolensis))
dB2

cul_crassirostris<-subset(culmen,Species=="Sporophila crassirostris")
#B3: Culmen crassirostris
R<-5000
B3<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(cul_crassirostris,.(Catalog.Number))
  boot.sample <- sample(cul_crassirostris$Medida, replace = TRUE)
  B3[kk] <- mean(boot.sample)
  print(B3)
}
boxplot(B3)
quantile(B3,c(0.025,0.975))
hist(B3, breaks = 30)
dB3<-c(Bmean=mean(B3),quantile(B3,c(0.025,0.975)),n=nrow(cul_crassirostris))
dB3

cul_maximiliani<-subset(culmen,Species=="Sporophila maximiliani")
cul_max_mean<-mean(cul_maximiliani$Medida)
cul_max_sd<-sd(cul_maximiliani$Medida)

#LPico####
LPico<-subset(data2,Variable=="Bill..tip.to.nostril.")
table(LPico$Species)

LPico_funerea<-subset(LPico,Species=="Sporophila funerea")
#B4: LPico funerea
R<-5000
B4<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(LPico_funerea,.(Catalog.Number))
  boot.sample <- sample(LPico_funerea$Medida, replace = TRUE)
  B4[kk] <- mean(boot.sample)
  print(B4)
}
boxplot(B4)
quantile(B4,c(0.025,0.975))
hist(B4, breaks = 30)
dB4<-c(Bmean=mean(B4),quantile(B4,c(0.025,0.975)),n=nrow(LPico_funerea))
dB4

LPico_angolensis<-subset(LPico,Species=="Sporophila angolensis")
#B5: LPico angolensis
R<-5000
B5<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(LPico_angolensis,.(Catalog.Number))
  boot.sample <- sample(LPico_angolensis$Medida, replace = TRUE)
  B5[kk] <- mean(boot.sample)
  print(B5)
}
boxplot(B5)
quantile(B5,c(0.025,0.975))
hist(B5, breaks = 30)
dB5<-c(Bmean=mean(B5),quantile(B5,c(0.025,0.975)),n=nrow(LPico_angolensis))
dB5

LPico_crassirostris<-subset(LPico,Species=="Sporophila crassirostris")
#B6: LPico crassirostris
R<-5000
B6<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(LPico_crassirostris,.(Catalog.Number))
  boot.sample <- sample(LPico_crassirostris$Medida, replace = TRUE)
  B6[kk] <- mean(boot.sample)
  print(B6)
}
boxplot(B6)
quantile(B6,c(0.025,0.975))
hist(B6, breaks = 30)
dB6<-c(Bmean=mean(B6),quantile(B6,c(0.025,0.975)),n=nrow(LPico_crassirostris))
dB6

LPico_maximiliani<-subset(LPico,Species=="Sporophila maximiliani")
LPico_max_mean<-mean(LPico_maximiliani$Medida)
LPico_max_sd<-sd(LPico_maximiliani$Medida)

#MaxiWid####
MaxiWid<-subset(data2,Variable=="Maxilla.width")
table(MaxiWid$Species)

MaxiWid_funerea<-subset(MaxiWid,Species=="Sporophila funerea")
#B7: MaxiWid funerea
R<-5000
B7<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(MaxiWid_funerea,.(Catalog.Number))
  boot.sample <- sample(MaxiWid_funerea$Medida, replace = TRUE)
  B7[kk] <- mean(boot.sample)
  print(B7)
}
boxplot(B7)
quantile(B7,c(0.025,0.975))
hist(B7, breaks = 30)
dB7<-c(Bmean=mean(B7),quantile(B7,c(0.025,0.975)),n=nrow(MaxiWid_funerea))
dB7

MaxiWid_angolensis<-subset(MaxiWid,Species=="Sporophila angolensis")
#B8: LPico angolensis
R<-5000
B8<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(MaxiWid_angolensis,.(Catalog.Number))
  boot.sample <- sample(MaxiWid_angolensis$Medida, replace = TRUE)
  B8[kk] <- mean(boot.sample)
  print(B8)
}
boxplot(B8)
quantile(B8,c(0.025,0.975))
hist(B8, breaks = 30)
dB8<-c(Bmean=mean(B8),quantile(B8,c(0.025,0.975)),n=nrow(MaxiWid_angolensis))
dB8

MaxiWid_crassirostris<-subset(MaxiWid,Species=="Sporophila crassirostris")
#B9: MaxiWid crassirostris
R<-5000
B9<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(MaxiWid_crassirostris,.(Catalog.Number))
  boot.sample <- sample(MaxiWid_crassirostris$Medida, replace = TRUE)
  B9[kk] <- mean(boot.sample)
  print(B9)
}
boxplot(B9)
quantile(B9,c(0.025,0.975))
hist(B9, breaks = 30)
dB9<-c(Bmean=mean(B9),quantile(B9,c(0.025,0.975)),n=nrow(MaxiWid_crassirostris))
dB9

MaxiWid_maximiliani<-subset(MaxiWid,Species=="Sporophila maximiliani")
MaxiWid_max_mean<-mean(MaxiWid_maximiliani$Medida)
MaxiWid_max_sd<-sd(MaxiWid_maximiliani$Medida)

#MandWid####
MandWid<-subset(data2,Variable=="Mandible.width")
table(MandWid$Species)

MandWid_funerea<-subset(MandWid,Species=="Sporophila funerea")
#B10: MandWid funerea
R<-5000
B10<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(MandWid_funerea,.(Catalog.Number))
  boot.sample <- sample(MandWid_funerea$Medida, replace = TRUE)
  B10[kk] <- mean(boot.sample)
  print(B10)
}
boxplot(B10)
quantile(B10,c(0.025,0.975))
hist(B10, breaks = 30)
dB10<-c(Bmean=mean(B10),quantile(B10,c(0.025,0.975)),n=nrow(MandWid_funerea))
dB10

MandWid_angolensis<-subset(MandWid,Species=="Sporophila angolensis")
#B11: MandWid angolensis
R<-5000
B11<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(MandWid_angolensis,.(Catalog.Number))
  boot.sample <- sample(MandWid_angolensis$Medida, replace = TRUE)
  B11[kk] <- mean(boot.sample)
  print(B11)
}
boxplot(B11)
quantile(B11,c(0.025,0.975))
hist(B11, breaks = 30)
dB11<-c(Bmean=mean(B11),quantile(B11,c(0.025,0.975)),n=nrow(MandWid_angolensis))
dB11

MandWid_crassirostris<-subset(MandWid,Species=="Sporophila crassirostris")
#B12: MandWid crassirostris
R<-5000
B12<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(MandWid_crassirostris,.(Catalog.Number))
  boot.sample <- sample(MandWid_crassirostris$Medida, replace = TRUE)
  B12[kk] <- mean(boot.sample)
  print(B12)
}
boxplot(B12)
quantile(B12,c(0.025,0.975))
hist(B12, breaks = 30)
dB12<-c(Bmean=mean(B12),quantile(B12,c(0.025,0.975)),n=nrow(MandWid_crassirostris))
dB12

MandWid_maximiliani<-subset(MandWid,Species=="Sporophila maximiliani")
MandWid_max_mean<-mean(MandWid_maximiliani$Medida)
MandWid_max_sd<-sd(MandWid_maximiliani$Medida)

#Bill.height###
APico<-subset(data2,Variable=="Bill.height")
table(APico$Species)

APico_funerea<-subset(APico,Species=="Sporophila funerea")
#B13: APico funerea
R<-5000
B13<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(APico_funerea,.(Catalog.Number))
  boot.sample <- sample(APico_funerea$Medida, replace = TRUE)
  B13[kk] <- mean(boot.sample)
  print(B13)
}
boxplot(B13)
quantile(B13,c(0.025,0.975))
hist(B13, breaks = 30)
dB13<-c(Bmean=mean(B13),quantile(B13,c(0.025,0.975)),n=nrow(APico_funerea))
dB13

APico_angolensis<-subset(APico,Species=="Sporophila angolensis")
#B14: APico angolensis
R<-5000
B14<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(APico_angolensis,.(Catalog.Number))
  boot.sample <- sample(APico_angolensis$Medida, replace = TRUE)
  B14[kk] <- mean(boot.sample)
  print(B14)
}
boxplot(B14)
quantile(B14,c(0.025,0.975))
hist(B14, breaks = 30)
dB14<-c(Bmean=mean(B14),quantile(B14,c(0.025,0.975)),n=nrow(APico_angolensis))
dB14

APico_crassirostris<-subset(APico,Species=="Sporophila crassirostris")
#B15: APico crassirostris
R<-5000
B15<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(APico_crassirostris,.(Catalog.Number))
  boot.sample <- sample(APico_crassirostris$Medida, replace = TRUE)
  B15[kk] <- mean(boot.sample)
  print(B15)
}
boxplot(B15)
quantile(B15,c(0.025,0.975))
hist(B15, breaks = 30)
dB15<-c(Bmean=mean(B15),quantile(B15,c(0.025,0.975)),n=nrow(APico_crassirostris))
dB15

APico_maximiliani<-subset(APico,Species=="Sporophila maximiliani")
APico_max_mean<-mean(APico_maximiliani$Medida)
APico_max_sd<-sd(APico_maximiliani$Medida)

#Wing####
LAla<-subset(data2,Variable=="Wing")
table(LAla$Species)

LAla_funerea<-subset(LAla,Species=="Sporophila funerea")
#B16: LAla funerea
R<-5000
B16<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(LAla_funerea,.(Catalog.Number))
  boot.sample <- sample(LAla_funerea$Medida, replace = TRUE)
  B16[kk] <- mean(boot.sample)
  print(B16)
}
boxplot(B16)
quantile(B16,c(0.025,0.975))
hist(B16, breaks = 30)
dB16<-c(Bmean=mean(B16),quantile(B16,c(0.025,0.975)),n=nrow(LAla_funerea))
dB16

LAla_angolensis<-subset(LAla,Species=="Sporophila angolensis")
#B17: LAla angolensis
B17<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(LAla_angolensis,.(Catalog.Number))
  boot.sample <- sample(LAla_angolensis$Medida, replace = TRUE)
  B17[kk] <- mean(boot.sample)
  print(B17)
}
boxplot(B17)
quantile(B17,c(0.025,0.975))
hist(B17, breaks = 30)
dB17<-c(Bmean=mean(B17),quantile(B17,c(0.025,0.975)),n=nrow(LAla_angolensis))
dB17

LAla_crassirostris<-subset(LAla,Species=="Sporophila crassirostris")
#B18: LAla crassirostris
B18<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(LAla_crassirostris,.(Catalog.Number))
  boot.sample <- sample(LAla_crassirostris$Medida, replace = TRUE)
  B18[kk] <- mean(boot.sample)
  print(B18)
}
boxplot(B18)
quantile(B18,c(0.025,0.975))
hist(B18, breaks = 30)
dB18<-c(Bmean=mean(B18),quantile(B18,c(0.025,0.975)),n=nrow(LAla_crassirostris))
dB18

LAla_maximiliani<-subset(LAla,Species=="Sporophila maximiliani")
LAla_max_mean<-mean(LAla_maximiliani$Medida)
LAla_max_sd<-sd(LAla_maximiliani$Medida)

#Tail####
LCola<-subset(data2,Variable=="Tail")
table(LCola$Species)

LCola_funerea<-subset(LCola,Species=="Sporophila funerea")
#B19: LCola funerea
B19<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(LCola_funerea,.(Catalog.Number))
  boot.sample <- sample(LCola_funerea$Medida, replace = TRUE)
  B19[kk] <- mean(boot.sample)
  print(B19)
}
boxplot(B19)
quantile(B19,c(0.025,0.975))
hist(B19, breaks = 30)
dB19<-c(Bmean=mean(B19),quantile(B19,c(0.025,0.975)),n=nrow(LCola_funerea))
dB19

LCola_angolensis<-subset(LCola,Species=="Sporophila angolensis")
#B20: LCola angolensis
B20<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(LCola_angolensis,.(Catalog.Number))
  boot.sample <- sample(LCola_angolensis$Medida, replace = TRUE)
  B20[kk] <- mean(boot.sample)
  print(B20)
}
boxplot(B20)
quantile(B20,c(0.025,0.975))
hist(B20, breaks = 30)
dB20<-c(Bmean=mean(B20),quantile(B20,c(0.025,0.975)),n=nrow(LCola_angolensis))
dB20

LCola_crassirostris<-subset(LCola,Species=="Sporophila crassirostris")
#B21: LCola crassirostris
B21<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(LCola_crassirostris,.(Catalog.Number))
  boot.sample <- sample(LCola_crassirostris$Medida, replace = TRUE)
  B21[kk] <- mean(boot.sample)
  print(B21)
}
boxplot(B21)
quantile(B21,c(0.025,0.975))
hist(B21, breaks = 30)
dB21<-c(Bmean=mean(B21),quantile(B21,c(0.025,0.975)),n=nrow(LCola_crassirostris))
dB21

LCola_maximiliani<-subset(LCola,Species=="Sporophila maximiliani")
LCola_max_mean<-mean(LCola_maximiliani$Medida)
LCola_max_sd<-sd(LCola_maximiliani$Medida)

#Tarsus####
LTarso<-subset(data2,Variable=="Tarsus")
table(LTarso$Species)

LTarso_funerea<-subset(LTarso,Species=="Sporophila funerea")
#B22: LTarso funerea
B22<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(LTarso_funerea,.(Catalog.Number))
  boot.sample <- sample(LTarso_funerea$Medida, replace = TRUE)
  B22[kk] <- mean(boot.sample)
  print(B22)
}
boxplot(B22)
quantile(B22,c(0.025,0.975))
hist(B22, breaks = 30)
dB22<-c(Bmean=mean(B22),quantile(B22,c(0.025,0.975)),n=nrow(LTarso_funerea))
dB22

LTarso_angolensis<-subset(LTarso,Species=="Sporophila angolensis")
#B23: LTarso angolensis
B23<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(LTarso_angolensis,.(Catalog.Number))
  boot.sample <- sample(LTarso_angolensis$Medida, replace = TRUE)
  B23[kk] <- mean(boot.sample)
  print(B23)
}
boxplot(B23)
quantile(B23,c(0.025,0.975))
hist(B23, breaks = 30)
dB23<-c(Bmean=mean(B23),quantile(B23,c(0.025,0.975)),n=nrow(LTarso_angolensis))
dB23

LTarso_crassirostris<-subset(LTarso,Species=="Sporophila crassirostris")
#B24: LTarso crassirostris
B24<-rep(0,R);
for(kk in 1:R){
Overall<-ddply(LTarso_crassirostris,.(Catalog.Number))
  boot.sample <- sample(LTarso_crassirostris$Medida, replace = TRUE)
  B24[kk] <- mean(boot.sample)
  print(B24)
}
boxplot(B24)
quantile(B24,c(0.025,0.975))
hist(B24, breaks = 30)
dB24<-c(Bmean=mean(B24),quantile(B24,c(0.025,0.975)),n=nrow(LTarso_crassirostris))
dB24

LTarso_maximiliani<-subset(LTarso,Species=="Sporophila maximiliani")
LTarso_max_mean<-mean(LTarso_maximiliani$Medida)
LTarso_max_sd<-sd(LTarso_maximiliani$Medida)

#Join results
#Resume data of Bootstrap (Bootstrap mean, confidence limits[2.5%-97.5%],n)
dbR=data.frame(dB1,dB2,dB3,dB4,dB5,dB6,
               dB7,dB8,dB9,dB10,dB11,dB12,
               dB13,dB14,dB15,dB16,dB17,dB18,
               dB19,dB20,dB21,dB22,dB23,dB24)
t.dbR<-t(dbR)
datboots<-as.data.frame(t.dbR)
head(datboots)
datboots$Especie<-rep(c("funerea","angolensis","crassirostris"),8)
datboots$Variable<-c(rep("Culmen",3),rep("Bill..tip.to.nostril.",3),
                     rep("Maxilla.width",3),rep("Mandible.width",3),
                     rep("Bill.height",3),rep("Wing",3),
                     rep("Tail",3),rep("Tarsus",3))
colnames(datboots)<-c("Bmean","Lower","Upper","n","Especie","Variable")

write.csv(datboots,"BootstrapSporophilas.csv")

#datos crudos ####
#Pero quiero cambiar el nombre de cada variable y unificar los titulos de ejes
# Entonces debo crear un vector que me ayude a cambiar ese aspecto estatico
variable_nombre<-list(
  'Culmen'="Culmen total",
  'Bill..tip.to.nostril.'="Largo del pico (narina-punta)",
  'Maxilla.width'="Ancho de la maxila",
  'Mandible.width'="Ancho de la mandíbula",
  'Bill.height'="Alto del pico",
  'Wing'="Largo del ala",
  'Tail'="Largo de la cola",
  'Tarsus'="Largo del Tarso")

variable_labeller <- function(variable, value){
  return(variable_nombre[value])
}

data2 = within (data2,Especie <- ifelse (Species =="Sporophila funerea", "funerea",
                               ifelse (Species =="Sporophila angolensis", "angolensis",
                               ifelse (Species =="Sporophila crassirostris", "crassirostris",
                               ifelse (Species =="Sporophila maximiliani", "maximiliani",
                               ifelse (Species =="Sporophila atrirostris", "atrirostris",NA))))))
table(data2$Especie)
#y luego si grafico, incluyendo los nombres para ejes unificados
data2$Especie = factor(data2$Especie, levels=c('funerea',
                                               'angolensis',
                                               'crassirostris',
                                               'atrirostris',
                                               'maximiliani'))
my_x_title <- expression(paste("Especie de ", italic("Sporophila")))

ggplot(data2,aes(x=Especie,y=Medida))+
  geom_hline(data=meanGraph,aes(yintercept=meanValues),linetype = "solid",colour="darkgray")+
  geom_hline(data=meanGraph,aes(yintercept=meanValues+sdValues),linetype = "dashed",colour="lightgray")+
  geom_hline(data=meanGraph,aes(yintercept=meanValues-sdValues),linetype = "dashed",colour="lightgray")+
#  geom_violin(alpha=0.3)+
  geom_jitter(alpha=0.7,width = 0.25,color="gray", size=1)+
  geom_boxplot(outlier.shape=1, alpha=0.3)+
#  geom_pointrange(data=datboots,aes(y=Bmean,ymin=Lower,ymax=Upper), color="blue",size=0.01,fatten=10) +
  facet_wrap(~Variable, scales = "free_y",labeller = variable_labeller, ncol=2)+
  labs(x=my_x_title,y="Valor de la medida (mm)")+
  scale_fill_tanagr(palette_name = "dacnis_berlepschi")+
#  coord_flip()+
  theme_bw()+
  theme(axis.text.x = element_text(face="italic",angle=60,hjust=1),
        panel.grid = element_blank(),
        strip.background = element_blank())
#Guardar esta figura en buena resolución .tiff a 300 dpi
ggsave("Boxplot Medidas~Sporophila spp.tiff", units="mm", width=100, height=115, dpi=300)

#Otras opciones con el valor de bootstraps - NO USADAS en publicacion####
meanGraph<-data.frame(Variable = c("Culmen","Bill..tip.to.nostril.",
                                   "Maxilla.width","Mandible.width",
                                   "Bill.height","Wing",
                                   "Tail","Tarsus"), 
                      meanValues = c(cul_max_mean,LPico_max_mean,
                                     MaxiWid_max_mean,MandWid_max_mean,
                                     APico_max_mean,LAla_max_mean, 
                                     LCola_max_mean,LTarso_max_mean),
                      sdValues = c(cul_max_sd,LPico_max_sd,
                                     MaxiWid_max_sd,MandWid_max_sd,
                                     APico_max_sd,LAla_max_sd, 
                                     LCola_max_sd,LTarso_max_sd))
variable_name<-list(
  'Culmen'="Culmen total",
  'Bill..tip.to.nostril.'="Largo del pico (narina-punta)",
  'Maxilla.width'="Ancho de la maxila",
  'Mandible.width'="Ancho de la mandíbula",
  'Bill.height'="Alto del pico",
  'Wing'="Largo del ala",
  'Tail'="Largo de la cola",
  'Tarsus'="Largo del Tarso")

variable_labeller <- function(variable, value){
  return(variable_name[value])
}
datboots$Especie = factor(datboots$Especie, levels=c('funerea',
                                               'angolensis',
                                               'crassirostris'))

my_y_title <- expression(paste("Especie de ", italic("Sporophila")))

ggplot(datboots,aes(x=Especie,y=Bmean))+
  geom_hline(data=meanGraph,aes(yintercept=meanValues),linetype = "dashed",colour="darkgray")+
  geom_hline(data=meanGraph,aes(yintercept=meanValues+sdValues),linetype = "dotted",colour="lightgray")+
  geom_hline(data=meanGraph,aes(yintercept=meanValues-sdValues),linetype = "dotted",colour="lightgray")+
  facet_wrap(.~Variable, scales = "free_y",ncol=2, labeller = variable_labeller)+
  geom_pointrange(aes(ymin=Lower,ymax=Upper), size=0.4) +
  geom_text(aes(label=n),hjust=2, size=2.5,
            position = position_dodge(width = 0.5), colour="black")+
  labs(title="", y="Valor de la medida remuestreada (mm)", x = my_y_title)+  
  theme_bw()+ #Para eliminar el color gris del fondo
  theme(panel.grid.major = element_blank(), #elimina las lineas grandes dentro del plot
        panel.grid.minor = element_blank(), #elimina las lineas pequenas dentro del plot
        axis.text.x = element_text(face="italic", angle=60, hjust = 1),
        strip.background = element_blank(),
        legend.position = "none")  #Posicion de la leyenda
#Guardar esta figura en buena resolución .tiff a 300 dpi
ggsave("Fig_bootstrap.tiff", units="mm", width=120, height=150, dpi=300)

