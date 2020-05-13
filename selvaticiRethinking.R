#remove.packages("rstan")
#if (file.exists(".RData")) file.remove(".RData")
#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
#library(devtools)
#install.packages(c("mvtnorm","loo","coda"), repos="https://cloud.r-project.org/",dependencies=TRUE)
#options(repos=c(getOption('repos'), rethinking='http://xcelab.net/R'))
#install.packages('rethinking',type='source')
library(readxl)
library(dplyr)
library(gplots)
library(ggplot2)
library(tidyr)      
library(scales)     
library(ggthemes)
library(DescTools)
library(rstanarm)
library(bayesplot)
library(coda)
library(loo)
library(DataExplorer)
library(emmeans)
library(ggeffects)
library(rethinking)
library(tidyverse)
library(ggridges)
library(RColorBrewer)

df <- read_excel("C://Users//mcrotta4//Desktop//teaching & courses//UNIMI//lavoro selvatici//df.xlsx")
cols <- c("specie", "HEV","tgondi","governorate", "area", "sex", "age_class", "year", "sector")


wildboar_b<-filter(df, specie=="WILD BOAR")
wildboar_b<-select(wildboar_b, c("tgondi","HEV","year","age_class","sex","year"))
wildboar_b<-wildboar_b[complete.cases(wildboar_b),]
df1.wb.tg<-wildboar_b

df1.wb.tg$tgondi<-as.numeric(as.factor(df1.wb.tg$tgondi))
df1.wb.tg$tgondi<-ifelse(df1.wb.tg$tgondi==1, 0, df1.wb.tg$tgondi-1)
df1.wb.tg$age_class<-as.numeric(as.factor(df1.wb.tg$age_class))
df1.wb.tg$sex<-as.numeric(as.factor(df1.wb.tg$sex))
df1.wb.tg$HEV<-as.numeric(as.factor(df1.wb.tg$HEV))
df1.wb.tg$year<-as.numeric(as.factor(df1.wb.tg$year))

dat_list1.1<-list(
  tgondi=as.integer(df1.wb.tg$tgondi), 
  age_class=as.integer(df1.wb.tg$age_class), 
  sex=as.integer(df1.wb.tg$sex), 
  HEV=as.integer(df1.wb.tg$HEV),
  year=as.integer(df1.wb.tg$year)
)

m1.1<-ulam(
  alist(
    tgondi ~ dbinom( 1, p ) ,
    logit(p)<-b1[age_class]+b2[sex]+b3[HEV]+b4[year],
    b1[age_class] ~ dnorm(0,0.5),
    b2[sex]~ dnorm(0,0.5),
    b3[HEV]~ dnorm(0,0.5),
    b4[year]~ dnorm(0,0.5)
    ), data=dat_list1.1, chains = 5, iter=10000,  log_lik = TRUE, control=list(max_treedepth=13, adapt_delta=0.99))

#results
traceplot_ulam(m1.1)
labs1.1<-c("Age_class 1", "Age_class 2", "Female", "Male","HEV POS", "HEV NEG" ,"2017","2018")
plot(precis(m1.1, depth = 2), labels=labs1.1, xlab="")
#contrasts
post<-extract.samples(m1.1)
post<-data.frame(post)
names(post)<-labs1.1
diffs<-list(
  ageClass2_Vs_AgeClass1=inv_logit(post$`Age_class 2`)-inv_logit(post$`Age_class 1`),
  HEVNeg_Vs_HEVPos=inv_logit(post$`HEV NEG`)-inv_logit(post$`HEV POS`),
  Female_Vs_Male=inv_logit(post$Female)-inv_logit(post$Male),
  y2017_Vs_y2018=inv_logit(post$`2017`)-inv_logit(post$`2018`))
par(mfrow=c(2,2))
dens(diffs$ageClass2_Vs_AgeClass1, main = "Contrasts AGE_CLASS (2/1)", xlab="Value")
dens(diffs$HEVNeg_Vs_HEVPos, main = "Contrasts HEV (NEG/POS)", xlab="Value")
dens(diffs$Female_Vs_Male, main = "Contrasts SEX (Female/Male)", xlab="Value")
dens(diffs$y2017_Vs_y2018, main = "Contrasts YEAR (2017/2018)", xlab="Value")
#P Plots
post %>%  
  pivot_longer(cols=1:8, names_to = "factor", values_to = "logit") %>% 
  mutate("prevalence"=inv_logit(logit))%>% 
  ggplot(aes(x = prevalence, y=factor,fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Prevalence", option = "D")  + theme_ridges()+
  # scale_fill_brewer(palette = 2) + 
  labs(x="Posterior Bayesian Estimated Uncertainty Probability of T.gondii in Wild Boars",y="")

####WILD BOAR HEV####

        df1.wb.hv<-wildboar_b
        df1.wb.hv<-df1.wb.hv[complete.cases(df1.wb.hv),]
        df1.wb.hv$HEV<-as.numeric(as.factor(df1.wb.hv$HEV))
        df1.wb.hv$HEV<-ifelse(df1.wb.hv$HEV==1, 0, df1.wb.hv$HEV-1)
        df1.wb.hv$age_class<-as.numeric(as.factor(df1.wb.hv$age_class))
        df1.wb.hv$sex<-as.numeric(as.factor(df1.wb.hv$sex))
        df1.wb.hv$tgondi<-as.numeric(as.factor(df1.wb.hv$tgondi))
        df1.wb.hv$year<-as.numeric(as.factor(df1.wb.hv$year))
        
        dat_list1.2<-list(
          HEV=as.integer(df1.wb.hv$HEV), 
          age_class=as.integer(df1.wb.hv$age_class), 
          sex=as.integer(df1.wb.hv$sex), 
          tgondi=as.integer(df1.wb.hv$tgondi),
          year=as.integer(df1.wb.hv$year)
        )
        
        m1.2<-ulam(
          alist(
            HEV ~ dbinom( 1, p ) ,
            logit(p)<-b1[age_class]+b2[sex]+b3[tgondi]+b4[year],
            b1[age_class] ~ dnorm(0,0.5),
            b2[sex]~ dnorm(0,0.5),
            b3[tgondi]~ dnorm(0,0.5),
            b4[year]~ dnorm(0,0.5)
          ), data=dat_list1.2, chains = 5, iter=10000,  log_lik = TRUE, control=list(max_treedepth=13, adapt_delta=0.99))
      
        #results
        traceplot_ulam(m1.2)
        par(mfrow=c(1,1))
        labs1.2<-c("Age_class 1", "Age_class 2", "Female", "Male","Tgondii POS", "Tgondii NEG" ,"2017","2018")
        plot(precis(m1.2, depth = 2), labels=labs1.2, xlab="")
        #contrasts
        post1.2<-extract.samples(m1.2)
        post1.2<-data.frame(post1.2)
        names(post1.2)<-labs1.2
        diffs1.2<-list(
          ageClass2_Vs_AgeClass1=inv_logit(post1.2$`Age_class 2`)-inv_logit(post1.2$`Age_class 1`),
          TgondiiNeg_VsP_TgondiiPos=inv_logit(post1.2$`Tgondii NEG`)-inv_logit(post1.2$`Tgondii POS`),
          Female_Vs_Male=inv_logit(post1.2$Female)-inv_logit(post1.2$Male),
          y2017_Vs_y2018=inv_logit(post1.2$`2017`)-inv_logit(post1.2$`2018`))
        par(mfrow=c(2,2))
        dens(diffs1.2$ageClass2_Vs_AgeClass1, main = "Contrasts AGE_CLASS (2/1)", xlab="Value")
        dens(diffs1.2$TgondiiNeg_VsP_TgondiiPos, main = "Contrasts Tgondii (NEG/POS)", xlab="Value")
        dens(diffs1.2$Female_Vs_Male, main = "Contrasts SEX (Female/Male)", xlab="Value")
        dens(diffs1.2$y2017_Vs_y2018, main = "Contrasts YEAR (2017/2018)", xlab="Value")
        
        post1.2<-data.frame(post1.2)
        names(post1.2)<-labs1.2
        post1.2 %>%  
          pivot_longer(cols=1:8, names_to = "factor", values_to = "logit") %>% 
          mutate("prevalence"=inv_logit(logit))%>% 
          ggplot(aes(x = prevalence, y=factor,fill = stat(x))) +
          geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
          scale_fill_viridis_c(name = "Prevalence", option = "D")  + theme_ridges()+
          # scale_fill_brewer(palette = 2) + 
          labs(x="Posterior Bayesian Estimated Prevalence of Hepatitis E in Wild Boars",y="")

#################
#######DEER######
#################
deer_b<-filter(df, specie=="DEER")
deer_b<-select(deer_b, c("tgondi","age_class","sex","area","year"))
deer_b<-deer_b[complete.cases(deer_b),]

    deer_b$tgondi<-as.numeric(as.factor(deer_b$tgondi))
    deer_b$tgondi<-ifelse(deer_b$tgondi==1, 0, deer_b$tgondi-1)
    deer_b$age_class<-as.numeric(as.factor(deer_b$age_class))
    deer_b$sex<-as.numeric(as.factor(deer_b$sex))
    deer_b$area<-as.numeric(as.factor(deer_b$area))
    deer_b$year<-as.numeric(as.factor(deer_b$year))
    
    
  dat_list2.1<-list(
    tgondi=as.integer(deer_b$tgondi), 
    age_class=as.integer(deer_b$age_class), 
    sex=as.integer(deer_b$sex), 
    area1=as.integer(deer_b$area),
    year=as.integer(deer_b$year),
    area=deer_b$area
  )
  
  
  m2.1<-ulam(
    alist(
      tgondi~dbinom(1,p),
      logit(p)<- x[area]*sigma_g + b1[age_class] + b2[sex] + b3[area1] + b4[year],
      b1[age_class]~dnorm(0,1),
      b2[sex]~dnorm(0,0.5),
      b3[area1]~dnorm(0,1),
      b4[year]~dnorm(0,0.5),
      ##adaptive prior
      x[area]~dnorm(0,0.5),
      ##hyper_priors
      sigma_g~dexp(7)
 #     gq>vector[area]:g<<-x*sigma_g
    ), data=dat_list2.1, control=list(adapt_delta=0.99), iter = 10000,  chains=4,log_lik=TRUE)
  
  #results
  traceplot_ulam(m2.1)
  par(mfrow=c(1,1))
  labs2.1<-c("Age_class 1", "Age_class 2", "Age_class 3", "Female", "Male", "Valley1", "Valley2", "Valley3", "Valley4","2017","2018")
  plot(precis(m2.1, depth = 2), labels=labs2.1, xlab="")
  #contrasts
  post2.1<-extract.samples(m2.1)
  post2.1<-data.frame(post2.1)
  post2.1<-post2.1%>%
    select(1:11)
  names(post2.1)<-labs2.1
  diffs2.1<-list(
    ageClass3_Vs_AgeClass2=inv_logit(post2.1$`Age_class 3`)-inv_logit(post2.1$`Age_class 2`),
    ageClass3_Vs_AgeClass1=inv_logit(post2.1$`Age_class 3`)-inv_logit(post2.1$`Age_class 1`),
    ageClass2_Vs_AgeClass1=inv_logit(post2.1$`Age_class 2`)-inv_logit(post2.1$`Age_class 1`),
    Valley4_Vs_Valley3=inv_logit(post2.1$Valley4)-inv_logit(post2.1$Valley3),
    Valley4_Vs_Valley2=inv_logit(post2.1$Valley4)-inv_logit(post2.1$Valley2),
    Valley4_Vs_Valley1=inv_logit(post2.1$Valley4)-inv_logit(post2.1$Valley1),
    Valley2_Vs_Valley3=inv_logit(post2.1$Valley2)-inv_logit(post2.1$Valley3),
    Valley2_Vs_Valley1=inv_logit(post2.1$Valley2)-inv_logit(post2.1$Valley1),
    Valley3_Vs_Valley1=inv_logit(post2.1$Valley3)-inv_logit(post2.1$Valley1),
    Female_Vs_Male=inv_logit(post2.1$Female)-inv_logit(post2.1$Male),
    y2017_Vs_y2018=inv_logit(post2.1$`2017`)-inv_logit(post2.1$`2018`))
  par(mfrow=c(3,4))
  dens(diffs2.1$ageClass3_Vs_AgeClass2, main = "Contrasts AGE_CLASS (3/2)", xlab="Value")
  dens(diffs2.1$ageClass3_Vs_AgeClass1, main = "Contrasts AGE_CLASS (3/1)", xlab="Value")  
  dens(diffs2.1$ageClass2_Vs_AgeClass1, main = "Contrasts AGE_CLASS (2/1)", xlab="Value")
  dens(diffs2.1$Valley4_Vs_Valley3, main = "Contrasts VALLEY (4/3)", xlab="Value")
  dens(diffs2.1$Valley4_Vs_Valley2, main = "Contrasts VALLEY (4/2)", xlab="Value")
  dens(diffs2.1$Valley4_Vs_Valley1, main = "Contrasts VALLEY (4/1)", xlab="Value")
  dens(diffs2.1$Valley2_Vs_Valley3, main = "Contrasts VALLEY (2/3)", xlab="Value")
  dens(diffs2.1$Valley2_Vs_Valley1, main = "Contrasts VALLEY (2/1)", xlab="Value")
  dens(diffs2.1$Valley3_Vs_Valley1, main = "Contrasts VALLEY (3/1)", xlab="Value")
  dens(diffs2.1$Female_Vs_Male, main = "Contrasts SEX (F/M)", xlab="Value")
  dens(diffs2.1$y2017_Vs_y2018, main = "Contrasts YEAR (2017/2018)", xlab="Value")
  

   post2.1 %>%  
    pivot_longer(cols=1:11,names_to = "factor", values_to = "logit") %>% 
    mutate("prevalence"=inv_logit(logit))%>% 
    ggplot(aes(x = prevalence, y=factor,fill = stat(x))) + geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = "Prevalence", option = "D")  + theme_ridges()+
   # scale_fill_brewer(palette = 2) + 
    labs(x="Posterior Bayesian Estimated Prevalence of T.gondii in Deer",y="")
  
################
####CHAMOIS#####
################
   chamois_b<-filter(df, specie=="CHAMOIS")
   chamois_b<-select(chamois_b, c("tgondi","HEV","age_class","sex","area","year"))
   chamois_btg<-select(chamois_b,c("tgondi","age_class","sex","area","year") )
   chamois_bhev<-select(chamois_b,c("tgondi","HEV","age_class","sex","area","year") )
   chamois_btg<-chamois_btg[complete.cases(chamois_btg),]
   chamois_bhev<-chamois_bhev[complete.cases(chamois_bhev),]
   str(chamois_btg)
   str(chamois_bhev)
   
chamois_btg$tgondi<-as.numeric(as.factor(chamois_btg$tgondi))
chamois_btg$tgondi<-ifelse(chamois_btg$tgondi==1, 0, chamois_btg$tgondi-1)
chamois_btg$age_class<-as.numeric(as.factor(chamois_btg$age_class))
chamois_btg$sex<-as.numeric(as.factor(chamois_btg$sex))
chamois_btg$area<-as.numeric(as.factor(chamois_btg$area))
chamois_btg$year<-as.numeric(as.factor(chamois_btg$year))
   
   
dat_list3.1<-list(
   tgondi=as.integer(chamois_btg$tgondi), 
   age_class=as.integer(chamois_btg$age_class), 
   sex=as.integer(chamois_btg$sex), 
   area1=as.integer(chamois_btg$area),
   year=as.integer(chamois_btg$year),
   area=chamois_btg$area
 )
   
   
   m3.1<-ulam(
     alist(
       tgondi~dbinom(1,p),
       logit(p)<- x[area]*sigma_g + b1[age_class] + b2[sex] + b3[area1] + b4[year],
       b1[age_class]~dnorm(0,2),
       b2[sex]~dnorm(0,2),
       b3[area1]~dnorm(0,2),
       b4[year]~dnorm(0,2),
       ##adaptive prior
       x[area]~dnorm(0,2),
       ##hyper_priors
       sigma_g~dunif(0,1)
   #    gq>vector[area]:g<<-x*sigma_g
     ), data=dat_list3.1, control=list(adapt_delta=0.99), iter =10000,  chains=4,log_lik=TRUE)
   
   #results
   traceplot_ulam(m3.1)
   par(mfrow=c(1,1))
   labs3.1<-c("Age_class 1", "Age_class 2", "Age_class 3", "Female", "Male", "Valley2", "Valley3", "Valley4","2017","2018")
   plot(precis(m3.1, depth = 2), labels=labs3.1, xlab="")
   post3.1<-extract.samples(m3.1)
   post3.1<-data.frame(post3.1)
   post3.1<-post3.1%>%
     select(1:10)
   names(post3.1)<-labs3.1
   diffs3.1<-list(
     ageClass1_Vs_AgeClass2=inv_logit(post3.1$`Age_class 1`)-inv_logit(post3.1$`Age_class 2`),
     ageClass1_Vs_AgeClass3=inv_logit(post3.1$`Age_class 1`)-inv_logit(post3.1$`Age_class 3`),
     ageClass3_Vs_AgeClass2=inv_logit(post3.1$`Age_class 3`)-inv_logit(post3.1$`Age_class 2`),
     Valley3_Vs_Valley2=inv_logit(post3.1$Valley3)-inv_logit(post3.1$Valley2),
     Valley3_Vs_Valley4=inv_logit(post3.1$Valley3)-inv_logit(post3.1$Valley4),
     Valley2_Vs_Valley4=inv_logit(post3.1$Valley2)-inv_logit(post3.1$Valley4),
     Female_Vs_Male=inv_logit(post3.1$Female)-inv_logit(post3.1$Male),
     y2017_Vs_y2018=inv_logit(post3.1$`2017`)-inv_logit(post3.1$`2018`))
   par(mfrow=c(3,3))
   dens(diffs3.1$ageClass1_Vs_AgeClass2, main = "Contrasts AGE_CLASS (1/2)", xlab="Value")
   dens(diffs3.1$ageClass1_Vs_AgeClass3, main = "Contrasts AGE_CLASS (1/3)", xlab="Value")  
   dens(diffs3.1$ageClass3_Vs_AgeClass2, main = "Contrasts AGE_CLASS (3/2)", xlab="Value")
   dens(diffs3.1$Valley3_Vs_Valley2, main = "Contrasts VALLEY (3/2)", xlab="Value")
   dens(diffs3.1$Valley3_Vs_Valley4, main = "Contrasts VALLEY (3/4)", xlab="Value")
   dens(diffs3.1$Valley2_Vs_Valley4, main = "Contrasts VALLEY (2/4)", xlab="Value")
   dens(diffs3.1$Female_Vs_Male, main = "Contrasts SEX (F/M)", xlab="Value")
   dens(diffs3.1$y2017_Vs_y2018, main = "Contrasts YEAR (2017/2018)", xlab="Value")
   
   
   post3.1 %>%  
     pivot_longer(cols=1:10,names_to = "factor", values_to = "logit") %>% 
     mutate("prevalence"=inv_logit(logit))%>% 
     ggplot(aes(x = prevalence, y=factor,fill = stat(x))) + geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
     scale_fill_viridis_c(name = "Prevalence", option = "D")  + theme_ridges()+
     # scale_fill_brewer(palette = 2) + 
     labs(x="Posterior Bayesian Estimated Prevalence of T.gondii in Chamois",y="")


#####chamois HEV   
   
chamois_bhev$HEV<-as.numeric(as.factor(chamois_bhev$HEV))
chamois_bhev$HEV<-ifelse(chamois_bhev$HEV==1, 0, chamois_bhev$HEV-1)
chamois_bhev$age_class<-as.numeric(as.factor(chamois_bhev$age_class))
chamois_bhev$sex<-as.numeric(as.factor(chamois_bhev$sex))
chamois_bhev$area<-as.numeric(as.factor(chamois_bhev$area))
chamois_bhev$year<-as.numeric(as.factor(chamois_bhev$year))
chamois_bhev$tgondi<-as.numeric(as.factor(chamois_bhev$tgondi))
   
dat_list3.2<-list(
     HEV=as.integer(chamois_bhev$HEV), 
     age_class=as.integer(chamois_bhev$age_class), 
     sex=as.integer(chamois_bhev$sex), 
     area1=as.integer(chamois_bhev$area),
     year=as.integer(chamois_bhev$year),
     tgondi=as.integer(chamois_bhev$tgondi),
     area=chamois_bhev$area
   )
   
   
m3.2<-ulam(
     alist(
       HEV~dbinom(1,p),
       logit(p)<- x[area]*sigma_g + b1[age_class] + b2[sex] + b3[area1] + b4[year]+ b5[tgondi],
       b1[age_class]~dnorm(0,2),
       b2[sex]~dnorm(0,0.5),
       b3[area1]~dnorm(0,2),
       b4[year]~dnorm(0,0.5),
       b5[tgondi]~dnorm(0,2),
       ##adaptive prior
       x[area]~dnorm(0,2),
       ##hyper_priors
       sigma_g~dunif(0,1)
       #    gq>vector[area]:g<<-x*sigma_g
     ), data=dat_list3.2, control=list(adapt_delta=0.99), iter = 10000,  chains=4,log_lik=TRUE)
   
   
#results
traceplot_ulam(m3.2)
par(mfrow=c(1,1))
labs3.2<-c("Age_class 1", "Age_class 2", "Age_class 3", "Female", "Male", "Valley2", "Valley3", "Valley4","2017","2018", "TgondiiPOS","TgondiiNEG")
post3.2<-extract.samples(m3.2)
post3.2<-data.frame(post3.2)
plot(precis(m3.2, depth = 2), labels=labs3.2, xlab="")
post3.2<-post3.2%>%
  select(1:12)
names(post3.2)<-labs3.2
diffs3.2<-list(
  ageClass1_Vs_AgeClass2=inv_logit(post3.2$`Age_class 1`)-inv_logit(post3.2$`Age_class 2`),
  ageClass1_Vs_AgeClass3=inv_logit(post3.2$`Age_class 1`)-inv_logit(post3.2$`Age_class 3`),
  ageClass3_Vs_AgeClass2=inv_logit(post3.2$`Age_class 3`)-inv_logit(post3.2$`Age_class 2`),
  Valley3_Vs_Valley2=inv_logit(post3.2$Valley3)-inv_logit(post3.2$Valley2),
  Valley3_Vs_Valley4=inv_logit(post3.2$Valley3)-inv_logit(post3.2$Valley4),
  Valley2_Vs_Valley4=inv_logit(post3.2$Valley2)-inv_logit(post3.2$Valley4),
  Female_Vs_Male=inv_logit(post3.2$Female)-inv_logit(post3.2$Male),
  y2017_Vs_y2018=inv_logit(post3.2$`2017`)-inv_logit(post3.2$`2018`),
  TgindiiNEG_Vs_TgondiiPOS=inv_logit(post3.2$TgondiiNEG)-inv_logit(post3.2$TgondiiPOS))


par(mfrow=c(3,3))
dens(diffs3.2$ageClass1_Vs_AgeClass2, main = "Contrasts AGE_CLASS (1/2)", xlab="Value")
dens(diffs3.2$ageClass1_Vs_AgeClass3, main = "Contrasts AGE_CLASS (1/3)", xlab="Value")  
dens(diffs3.2$ageClass3_Vs_AgeClass2, main = "Contrasts AGE_CLASS (3/2)", xlab="Value")
dens(diffs3.2$Valley3_Vs_Valley2, main = "Contrasts VALLEY (3/2)", xlab="Value")
dens(diffs3.2$Valley3_Vs_Valley4, main = "Contrasts VALLEY (3/4)", xlab="Value")
dens(diffs3.2$Valley2_Vs_Valley4, main = "Contrasts VALLEY (2/4)", xlab="Value")
dens(diffs3.2$Female_Vs_Male, main = "Contrasts SEX (F/M)", xlab="Value")
dens(diffs3.2$y2017_Vs_y2018, main = "Contrasts YEAR (2017/2018)", xlab="Value")
dens(diffs3.2$TgindiiNEG_Vs_TgondiiPOS, main = "Contrasts Tgondii (NEG/POS)", xlab="Value")

   post3.2 %>%  
     pivot_longer(cols=1:12,names_to = "factor", values_to = "logit") %>% 
     mutate("prevalence"=inv_logit(logit))%>% 
     ggplot(aes(x = prevalence, y=factor,fill = stat(x))) + geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
     scale_fill_viridis_c(name = "Prevalence", option = "D")  + theme_ridges()+
     # scale_fill_brewer(palette = 2) + 
     labs(x="Posterior Bayesian Estimated Prevalence of T.gondii in Chamois",y="")
ftable(chamois_bhev$HEV,chamois_bhev$area)   

################
####ROE#####
################
roe_b<-filter(df, specie=="ROE")
roe_b<-select(roe_b, c("tgondi","age_class","sex","area","year"))
roe_b<-roe_b[complete.cases(roe_b),]

roe_b$tgondi<-as.numeric(as.factor(roe_b$tgondi))
roe_b$tgondi<-ifelse(roe_b$tgondi==1, 0, roe_b$tgondi-1)
roe_b$age_class<-as.numeric(as.factor(roe_b$age_class))
roe_b$sex<-as.numeric(as.factor(roe_b$sex))
roe_b$area<-as.numeric(as.factor(roe_b$area))
roe_b$year<-as.numeric(as.factor(roe_b$year))


dat_list4.1<-list(
  tgondi=as.integer(roe_b$tgondi), 
  age_class=as.integer(roe_b$age_class), 
  sex=as.integer(roe_b$sex), 
  area1=as.integer(roe_b$area),
  year=as.integer(roe_b$year),
  area=roe_b$area
)


m4.1<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<- x[area]*sigma_g + b1[age_class] + b2[sex] + b3[area1] + b4[year],
    b1[age_class]~dnorm(0,1.5),
    b2[sex]~dnorm(0,1.5),
    b3[area1]~dnorm(0,1.5),
    b4[year]~dnorm(0,1.5),
    ##adaptive prior
    x[area]~dnorm(0,1.5),
    ##hyper_priors
    sigma_g~dunif(0,1)
    #    gq>vector[area]:g<<-x*sigma_g
  ), data=dat_list4.1, control=list(adapt_delta=0.99), iter = 10000,  chains=4,log_lik=TRUE)


#results
traceplot_ulam(m4.1)
par(mfrow=c(1,1))
labs4.1<-c("Age_class 1", "Age_class 2", "Age_class 3", "Female", "Male", "Valley1","Valley2", "Valley3", "Valley4","2017","2018")
post4.1<-extract.samples(m4.1)
post4.1<-data.frame(post4.1)
plot(precis(m4.1, depth = 2), labels=labs4.1, xlab="")
post4.1<-post4.1%>%
  select(1:11)
names(post4.1)<-labs4.1
diffs4.1<-list(
  ageClass3_Vs_AgeClass2=inv_logit(post4.1$`Age_class 3`)-inv_logit(post4.1$`Age_class 2`),
  ageClass3_Vs_AgeClass1=inv_logit(post4.1$`Age_class 3`)-inv_logit(post4.1$`Age_class 1`),
  ageClass2_Vs_AgeClass1=inv_logit(post4.1$`Age_class 2`)-inv_logit(post4.1$`Age_class 1`),
  Valley1_Vs_Valley2=inv_logit(post4.1$Valley1)-inv_logit(post4.1$Valley2),
  Valley1_Vs_Valley3=inv_logit(post4.1$Valley1)-inv_logit(post4.1$Valley3),
  Valley1_Vs_Valley4=inv_logit(post4.1$Valley1)-inv_logit(post4.1$Valley4),
  Valley4_Vs_Valley3=inv_logit(post4.1$Valley4)-inv_logit(post4.1$Valley3),
  Valley4_Vs_Valley2=inv_logit(post4.1$Valley4)-inv_logit(post4.1$Valley2),
  Valley3_Vs_Valley2=inv_logit(post4.1$Valley3)-inv_logit(post4.1$Valley2),
  Female_Vs_Male=inv_logit(post4.1$Female)-inv_logit(post4.1$Male),
  y2017_Vs_y2018=inv_logit(post4.1$`2017`)-inv_logit(post4.1$`2018`))
par(mfrow=c(3,4))
dens(diffs4.1$ageClass3_Vs_AgeClass2, main = "Contrasts AGE_CLASS (3/2)", xlab="Value")
dens(diffs4.1$ageClass3_Vs_AgeClass1, main = "Contrasts AGE_CLASS (3/1)", xlab="Value")  
dens(diffs4.1$ageClass2_Vs_AgeClass1, main = "Contrasts AGE_CLASS (2/1)", xlab="Value")
dens(diffs4.1$Valley1_Vs_Valley2, main = "Contrasts VALLEY (1/2)", xlab="Value")
dens(diffs4.1$Valley1_Vs_Valley3, main = "Contrasts VALLEY (1/3)", xlab="Value")
dens(diffs4.1$Valley1_Vs_Valley4, main = "Contrasts VALLEY (1/4)", xlab="Value")
dens(diffs4.1$Valley4_Vs_Valley3, main = "Contrasts VALLEY (4/3)", xlab="Value")
dens(diffs4.1$Valley4_Vs_Valley2, main = "Contrasts VALLEY (4/2)", xlab="Value")
dens(diffs4.1$Valley3_Vs_Valley2, main = "Contrasts VALLEY (3/2)", xlab="Value")
dens(diffs4.1$Female_Vs_Male, main = "Contrasts SEX (F/M)", xlab="Value")
dens(diffs4.1$y2017_Vs_y2018, main = "Contrasts YEAR (2017/2018)", xlab="Value")

post4.1 %>%  
  pivot_longer(cols=1:11,names_to = "factor", values_to = "logit") %>% 
  mutate("prevalence"=inv_logit(logit))%>% 
  ggplot(aes(x = prevalence, y=factor,fill = stat(x))) + geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Prevalence", option = "D")  + theme_ridges()+
  # scale_fill_brewer(palette = 2) + 
  labs(x="Posterior Bayesian Estimated Uncertainty Probability of T.gondii in Roe",y="")

#################
####MOUFLONS#####
#################
mouf_b<-filter(df, specie=="MOUFLONS")
mouf_b<-select(mouf_b, c("tgondi","age_class","sex","area","year"))
mouf_b<-mouf_b[complete.cases(mouf_b),]

mouf_b$tgondi<-as.numeric(as.factor(mouf_b$tgondi))
mouf_b$tgondi<-ifelse(mouf_b$tgondi==1, 0, mouf_b$tgondi-1)
mouf_b$age_class<-as.numeric(as.factor(mouf_b$age_class))
mouf_b$sex<-as.numeric(as.factor(mouf_b$sex))
mouf_b$area<-as.numeric(as.factor(mouf_b$area))
mouf_b$year<-as.numeric(as.factor(mouf_b$year))


dat_list5.1<-list(
  tgondi=as.integer(mouf_b$tgondi), 
  age_class=as.integer(mouf_b$age_class), 
  sex=as.integer(mouf_b$sex), 
  area1=as.integer(mouf_b$area),
  year=as.integer(mouf_b$year),
  area=mouf_b$area
)


m5.1<-ulam(
  alist(
    tgondi~dbinom(1,p),
    logit(p)<- x[area]*sigma_g + b1[age_class] + b2[sex] + b3[area1] + b4[year],
    b1[age_class]~dnorm(0,1.5),
    b2[sex]~dnorm(0,1.5),
    b3[area1]~dnorm(0,1.5),
    b4[year]~dnorm(0,1.5),
    ##adaptive prior
    x[area]~dnorm(0,1.5),
    ##hyper_priors
    sigma_g~dunif(0,1)
    #    gq>vector[area]:g<<-x*sigma_g
  ), data=dat_list5.1, control=list(adapt_delta=0.99), iter = 10000,  chains=4,log_lik=TRUE)


#results
traceplot_ulam(m5.1)
par(mfrow=c(1,1))
labs5.1<-c("Age_class 1", "Age_class 2", "Age_class 3", "Female", "Male", "Valley1","Valley2", "Valley3","2017","2018")
post5.1<-extract.samples(m5.1)
post5.1<-data.frame(post5.1)
plot(precis(m5.1, depth = 2), labels=labs5.1, xlab="")
post5.1<-post5.1%>%
  select(1:10)
names(post5.1)<-labs5.1
diffs5.1<-list(
  ageClass3_Vs_AgeClass2=inv_logit(post5.1$`Age_class 3`)-inv_logit(post5.1$`Age_class 2`),
  ageClass3_Vs_AgeClass1=inv_logit(post5.1$`Age_class 3`)-inv_logit(post5.1$`Age_class 1`),
  ageClass2_Vs_AgeClass1=inv_logit(post5.1$`Age_class 2`)-inv_logit(post5.1$`Age_class 1`),
  Valley2_Vs_Valley1=inv_logit(post5.1$Valley2)-inv_logit(post5.1$Valley1),
  Valley2_Vs_Valley3=inv_logit(post5.1$Valley2)-inv_logit(post5.1$Valley3),
  Valley1_Vs_Valley3=inv_logit(post5.1$Valley1)-inv_logit(post5.1$Valley3),
  Female_Vs_Male=inv_logit(post5.1$Female)-inv_logit(post5.1$Male),
  y2018_Vs_y2017=inv_logit(post5.1$`2018`)-inv_logit(post5.1$`2017`))
par(mfrow=c(3,4))
dens(diffs5.1$ageClass3_Vs_AgeClass1, main = "Contrasts AGE_CLASS (3/1)", xlab="Value")
dens(diffs5.1$ageClass3_Vs_AgeClass2, main = "Contrasts AGE_CLASS (3/2)", xlab="Value")  
dens(diffs5.1$ageClass2_Vs_AgeClass1, main = "Contrasts AGE_CLASS (2/3)", xlab="Value")
dens(diffs5.1$Valley2_Vs_Valley1, main = "Contrasts VALLEY (1/2)", xlab="Value")
dens(diffs5.1$Valley2_Vs_Valley3, main = "Contrasts VALLEY (1/3)", xlab="Value")
dens(diffs5.1$Valley1_Vs_Valley3, main = "Contrasts VALLEY (1/4)", xlab="Value")
dens(diffs5.1$Female_Vs_Male, main = "Contrasts SEX (F/M)", xlab="Value")
dens(diffs5.1$y2018_Vs_y2017, main = "Contrasts YEAR (2017/2018)", xlab="Value")

post5.1 %>%  
  pivot_longer(cols=1:10,names_to = "factor", values_to = "logit") %>% 
  mutate("prevalence"=inv_logit(logit))%>% 
  ggplot(aes(x = prevalence, y=factor,fill = stat(x))) + geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Prevalence", option = "D")  + theme_ridges()+
  # scale_fill_brewer(palette = 2) + 
  labs(x="Posterior Bayesian Estimated Uncertainty Probability of T.gondii in Mouflons",y="")
