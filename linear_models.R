library("ggplot2")
library("dplyr")


data<-readRDS("input/train_data.RDS")


################################################################
#
# Creating linear models to reflect car geometry and constraints
#
################################################################

#looping over frequency bands

models<-data.frame("model"=0,"lm_cmd"=as.character("null"),freq=0)

models$lm_cmd<-as.character(models$lm_cmd)

for(i in 1:5){
  
  model1=1
  cmd1<-paste("azp_1r_",i," ~ az_1r_",i," + azp_1l_",i," + azp_2r_",i," + azp_2l_",i," + azs_1_",i,sep="")
  models<-add_row(models,model = model1, lm_cmd = cmd1, freq = i)
  
  model1=2
  cmd1<-paste("azp_2r_",i," ~ az_2r_",i," + azp_2l_",i," + azp_1r_",i," + azp_1l_",i," + azs_1_",i,sep="")
  models<-add_row(models,model = model1, lm_cmd = cmd1, freq = i)
  
  model1=3
  cmd1<-paste("azp_1l_",i," ~ az_1l_",i," + azp_1r_",i," + azp_2r_",i," + azp_2l_",i," + azs_1_",i,sep="")
  models<-add_row(models,model = model1, lm_cmd = cmd1, freq = i)
  
  model1=4
  cmd1<-paste("azp_2l_",i," ~ az_2l_",i," + azp_2r_",i," + azp_1r_",i," + azp_1l_",i," + azs_1_",i,sep="")
  models<-add_row(models,model = model1, lm_cmd = cmd1, freq = i)
  
  model1=5
  cmd1<-paste("azp_3r_",i," ~ az_3r_",i," + azp_3l_",i," + azp_4r_",i," + azp_4l_",i," + azs_2_",i,sep="")
  models<-add_row(models,model = model1, lm_cmd = cmd1, freq = i)
  
  model1=6
  cmd1<-paste("azp_4r_",i," ~ az_4r_",i," + azp_4l_",i," + azp_3r_",i," + azp_3l_",i," + azs_2_",i,sep="")
  models<-add_row(models,model = model1, lm_cmd = cmd1, freq = i)
  
  model1=7
  cmd1<-paste("azp_3l_",i," ~ az_3l_",i," + azp_3r_",i," + azp_4r_",i," + azp_4l_",i," + azs_2_",i,sep="")
  models<-add_row(models,model = model1, lm_cmd = cmd1, freq = i)
  
  model1=8
  cmd1<-paste("azp_4l_",i," ~ az_4l_",i," + azp_4r_",i," + azp_3l_",i," + azp_3r_",i," + azs_2_",i,sep="")
  models<-add_row(models,model = model1, lm_cmd = cmd1, freq = i)
  
  model1=9
  cmd1<-paste("azs_1_",i," ~ azp_1r_",i," + azp_1l_",i," + azp_2r_",i," + azp_2l_",i," + azs_2_",i,sep="")
  models<-add_row(models,model = model1, lm_cmd = cmd1, freq = i)
  
  model1=10
  cmd1<-paste("azs_2_",i," ~ azp_3r_",i," + azp_3l_",i," + azp_4r_",i," + azp_4l_",i," + azs_1_",i,sep="")
  models<-add_row(models,model = model1, lm_cmd = cmd1, freq = i)
}



fit1 <- lm(models$lm_cmd[2],data[data$ExperimentID==1,])

fit1.coefficients

summary(fit1)
v<-coef(fit1)
v[2]

results<-data.frame("model"=0,cmd="null",freq=0,ID=0,c1=0,c2=0,c3=0,c4=0,c5=0,c6=0)

for( n in 2:nrow(models)){
  
  for(k in 1:200){
    fit1 <-  lm(models$lm_cmd[n],data[data$ExperimentID==k,])
    v<-coef(fit1)
    results <- add_row(results, 
                       model = models$model[n], 
                       cmd = models$lm_cmd[n], 
                       freq = models$freq[n],
                       ID = k,
                       c1=v[1],
                       c2=v[2],
                       c3=v[3],
                       c4=v[4],
                       c5=v[5],
                       c6=v[6])
    
  }
  
  
}

results %>% filter(model==1) %>%
  ggplot(.,aes(x=freq,y=c2))+geom_boxplot(aes(fill=as.factor(freq)))


results %>% filter(model==2) %>%
  ggplot(.,aes(x=freq,y=c2))+geom_boxplot(aes(fill=as.factor(freq)))

results %>% filter(model==3) %>%
  ggplot(.,aes(x=freq,y=c2))+geom_boxplot(aes(fill=as.factor(freq)))

results %>% filter(model==4) %>%
  ggplot(.,aes(x=freq,y=c2))+geom_boxplot(aes(fill=as.factor(freq)))

results %>% filter(model==9) %>%
  ggplot(.,aes(x=freq,y=c2))+geom_boxplot(aes(fill=as.factor(freq)))


