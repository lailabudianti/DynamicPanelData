library(rsample)
library(DataExplorer)
library(sjPlot)
library(lmtest)
library(plm)
library(readxl)
library(splm)
library(pls)
library(car)
library(plm)
library(lmtest)
library(sjPlot)
library(DataExplorer)
library(rsample)
library(rgdal)
library(GWmodel)
library(raster)
library(spdep)
library(dplyr)

setwd("D:/Kuliah S2/Anreg")
data.uas=read_excel("DATA UAS ANREG 2010-2022.xlsx")
str(data.uas)
#Model Panel Dinamis : sargan nya nggak terpenuhi
fd.gmm<- pgmm(log(PDRB)~lag(log(PDRB))+log(Populasi)+
                (RASIO)+RLS+log(ELEC)+log(ROAD)+log(LOAD+0.5)
              +OPEN+log(CSGRL)+log(YAGR) | lag(log(PDRB),2:5),
              data=data.uas, 
              effect = "individual", model = c("onestep"), transformation = "d",
              index = c("Provinsi", "Tahun"))
summary(fd.gmm)


#1. Cek Multikolinearitas
#manual
cor_matrix= cor(data.uas[,4:12])
inverse_cor_matrix <- solve(cor_matrix) ; inverse_cor_matrix

#function
cekVIF=function(data){
  corr=as.matrix(cor(data))
  VIF=diag(solve(corr))
  return(VIF)
}
var_cek=data.uas[,4:12]
cekVIF(var_cek)

#1. Cek Normalitas
residuals <- fd.gmm$residuals

# Uji normalitas menggunakan tes statistik Shapiro-Wilk
res=(unlist(t(residuals)))
res
mean1= mean(res)
sd1=sd(res)
ks.test(res,"pnorm", mean=mean1, sd=sd1)

#outlier
boxplot(res)

#Model Data Panel
#2. FEM
FEM = plm(log(PDRB)~lag(log(PDRB))+log(Populasi)+
            (RASIO)+RLS+log(ELEC)+log(ROAD)+log(LOAD+0.5)
          +OPEN+log(CSGRL)+log(YAGR), data=data.uas, 
          effect="individual", index=c('Provinsi','Tahun'), model="within")
summary(FEM)

#2. CEM 
CEM = plm(log(PDRB)~lag(log(PDRB))+log(Populasi)+
            (RASIO)+RLS+log(ELEC)+log(ROAD)+log(LOAD+0.5)
          +OPEN+log(CSGRL)+log(YAGR) , data=data.uas,  
          effect="individual", index=c('Provinsi','Tahun'), model="pooling")
summary(CEM)

#Pemilhan Model menggunakan uji Chow
pooltest(CEM,FEM)  # Tolak H0 - > Gunakan FEM

#3. REM
REM = plm(log(PDRB)~lag(log(PDRB))+log(Populasi)+
            (RASIO)+RLS+log(ELEC)+log(ROAD)+log(LOAD+0.5)
          +OPEN+log(CSGRL)+log(YAGR) , data=data.uas,  
          effect="individual", index=c('Provinsi','Tahun'), model="random")
summary(REM)

#Pemilhan Model menggunakan uji Hausman
phtest(REM, FEM) #tolak H0 : FEM

#ketidakbiasaan: liat dari estimasi lag(log(PDRB)). harus diatas fem dibawah pls
#gmm: 0.5083211 , fem: 0.0033107 , pls: 0.1779500  --> diatas fem dan pls


#Melakukan pemodelan dengan transformasi full
fd.gmm.s <- pgmm(log(PDRB)~lag(log(PDRB))+log(Populasi)+
                   (RASIO)+RLS+log(ELEC)+log(ROAD)+log(LOAD+0.5)
                 +OPEN+log(CSGRL)+log(YAGR) | lag(log(PDRB),2:5),
                 data=data.uas, 
                 effect = "individual", model = c("onestep"), transformation = "ld",
                 index = c("Provinsi", "Tahun"))
summary(fd.gmm.s)
#ketidakbiasaan: liat dari estimasi lag(log(PDRB)). harus diatas fem dibawah pls
#gmm.s: 0.9014969 , fem: 0.0033107 , pls: 0.1779500 
#--> diatas fem dan pls

#1. Cek Normalitas
residuals <- fd.gmm.s$residuals

# Uji normalitas menggunakan tes statistik Shapiro-Wilk
ress=(unlist(t(residuals)))
ress
mean1= mean(res)
sd1=sd(res)
ks.test(res,"pnorm", mean=mean1, sd=sd1)
shapiro.test(res)

boxplot(ress)

lagy <- lag(data.uas$PDRB)
lagy[1] <- Inf
multi <- cbind(lagy,data.uas[4:12])
multi
str(multi)
cor(multi)
cekVIF=function(data){
  corr=as.matrix(cor(data))
  VIF=diag(solve(corr))
  return(VIF)
}
cekVIF(multi)

vif(fd.gmm)
VIF(fd.gmm.s)


library(dplyr)
lag_y <- data.uas %>%
  select(Provinsi, Tahun, PDRB) %>%
  filter(Tahun !=2022) %>%
  select(lag_y=PDRB)

data_lag <- data.uas %>%
  select(Provinsi, Tahun, PDRB,Populasi,RASIO,RLS,ELEC,ROAD,LOAD,OPEN,CSGRL,YAGR) %>%
  filter(Tahun !=2010) %>%
  cbind(lag_y)
head(data_lag)
cekVIF=function(data){
  corr=as.matrix(cor(data))
  VIF=diag(solve(corr))
  return(VIF)
}
var_cek <- data_lag[4:13]
str(var_cek)
cekVIF(var_cek)
