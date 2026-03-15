setwd("D:/SEM 3/R/Mohit Term paper/DDI-IND-CSO-PLFS-2022-23")

library(dplyr)

hhv1 <- read.delim("hhv1.txt",header=F,sep='\t')

names(hhv1)
hhv1 <- hhv1[c("V3","V4","V13","V14","V15"
               ,"V16","V5","V22","V23"
               ,"V24","V25","V21")]
names(hhv1)
str(hhv1) #this is unique to this
names(hhv1)=c("quater","visit","fsu","sub_block",
              "sss","sample_hhno","sector","HH_type",
              "religion","social_grp","HH_mce", "HH_size")

hhv1<-hhv1[c("quater","visit","fsu","sub_block",
             "sss","sample_hhno","sector","HH_type",
             "religion","social_grp","HH_mce", "HH_size")]
str(hhv1)

a_hhv1 <- hhv1 %>% mutate(
  sample_hhno = sprintf("%02d", sample_hhno)
)

str(a_hhv1)

b_hhv1 <- a_hhv1 %>% mutate(
  hh_id = paste0(quater,visit,fsu,sub_block,sss,sample_hhno)
)
str(b_hhv1)

c_hhv1 <- b_hhv1[c("hh_id","HH_type","sector","religion","social_grp","HH_mce",
                   "HH_size")]
str(c_hhv1) 
c_hhv1 <- na.omit(c_hhv1)
str(c_hhv1) # this is final data for hhv1
hhv1 <- c_hhv1
rm(list=c("a_hhv1","b_hhv1","c_hhv1"))

#starting perv1

a = read.delim(
  "perv1.txt",
  sep="\t",
  header=F
)

names(a)
a <- a[c("V3","V4","V13","V14","V15",
         "V16","V17","V19","V20",
         "V21","V22","V134","V32","V131")]

names(a)=c("quater","visit","fsu","sub_block",
             "sss","sample_hhno","serial_no","gender",
           "age","marital","edu","wage","status","week_status")
str(a)

b <- a[a$week_status %in% c(31,71,72),]
str(b)
b <- b %>% mutate(
  sample_hhno =sprintf("%02d",sample_hhno)
)
str(b)
c<-b %>% mutate(
  hh_id = paste0(quater,visit,fsu,sub_block,sss,sample_hhno),
  person_id = paste0(quater,visit,fsu,sub_block,sss,sample_hhno,serial_no)
)
str(c)
d<-c[c("person_id","hh_id","gender","age","marital",
       "edu","wage","status","week_status")]
d <-na.omit(d)
perv1 = d #final perv1 data
rm(list=c("a","d","c","b"))

visit1 <- merge(perv1,hhv1,
                by= "hh_id",
                all.x=T)
names(visit1)

#for hhrv
a <- read.delim(
  "hhrv.txt",
  sep="\t",
  header =F
)
names(a)
a <- a[c("V3","V4","V13","V14","V15"
               ,"V16","V5","V22","V23"
               ,"V24","V25","V21")]

names(a)=c("quater","visit","fsu","sub_block",
              "sss","sample_hhno","sector","HH_type",
              "religion","social_grp","HH_mce", "HH_size")

str(a)

a <- a %>% mutate(
  sample_hhno = sprintf("%02d", sample_hhno)
)

str(a)

b <- a %>% mutate(
  hh_id = paste0(quater,visit,fsu,sub_block,sss,sample_hhno)
)
str(b)

c <- b[c("hh_id","HH_type","sector","religion","social_grp","HH_mce",
                   "HH_size")]
str(c) 
c <- na.omit(c)
str(c) # this is final data for hhrv
hhrv <- c
rm(list=c("a","b","c"))

#for perrv

a = read.delim(
  "perrv.txt",
  sep="\t",
  header=F
)

names(a)
a <- a[c("V3","V4","V13","V14","V15",
         "V16","V17","V19","V20",
         "V21","V22","V99","V96")]

names(a)=c("quater","visit","fsu","sub_block",
           "sss","sample_hhno","serial_no","gender",
           "age","marital","edu","wage","week_status")
str(a)

b <- a[a$week_status %in% c(31,71,72),]
str(b)
b <- b %>% mutate(
  sample_hhno =sprintf("%02d",sample_hhno)
)
str(b)
c<-b %>% mutate(
  hh_id = paste0(quater,visit,fsu,sub_block,sss,sample_hhno),
  person_id = paste0(quater,visit,fsu,sub_block,sss,sample_hhno,serial_no)
)
str(c)
d<-c[c("person_id","hh_id","gender","age","marital",
       "edu","wage","week_status")]
d <-na.omit(d)
perrv = d #final perrv data
rm(list=c("a","d","c","b"))

revisit <- merge(perrv,hhrv,
                by= "hh_id",
                all.x=T)
names(revisit) #revisit final

names(revisit)
names(visit1)

e <- visit1 %>% select(
  -status
)
names(e)
visit1 <- e
rm(e)

data_comb = bind_rows(visit1,revisit)
data_comb=na.omit(data_comb)

names(data_comb)
str(data_comb)

#considering only formal sector:
hello <- data_comb[data_comb$sector==2 | data_comb$gender==3,]

hello = hello %>% mutate(
  gender = case_when(
    gender==1~"male",
    TRUE ~"Female"
  ),
  sector = "ubran"
)

hello = hello %>% mutate(
  marital = case_when(
    marital==1 ~ "Never",
    marital==2~"currently_married",
    marital==3~ "Widowed",
    TRUE ~ "Divorced"
  )
)

hello = hello %>% mutate(
  edu= case_when(
    edu==5~"below_primary",
    edu==6~"primary",
    edu==7~ "middle",
    edu==8~"secondary",
    edu==10~"high_sec",
    edu==11~"diploma",
    edu==12~"graduate",
    edu==13~"Postgrad_abv",
    TRUE ~ "without_formal_sch"
    
  )
)


hello = hello %>% mutate(
  religion=case_when(
    religion==1~"Hindu",
    religion==2~"Islam",
    religion==3~"Christian",
    religion==5~"Buddhism",
    TRUE~"others"
  )
)

hello = hello %>% mutate(
  social_grp=case_when(
    social_grp==1~"ST",
    social_grp==2~"SC",
    social_grp==3~"OBC",
    TRUE ~ "others"
  )
)

str(hello)
unique(hello$HH_type)
hello = hello %>%  mutate(
  HH_type = case_when(
    HH_type==1~"self_employed",
    HH_type==2~"salary_earning",
    HH_type==3~"casual_labour",
    TRUE ~ "others"
  )
)
str(hello)
unique(hello$sec)

hi= hello %>% mutate(
  edu=as.factor(edu),
  gender =as.factor(gender),
  marital =as.factor(marital),
  week_status = as.factor(week_status),
  religion = as.factor(religion),
  social_grp=as.factor(social_grp),
  HH_type = as.factor(HH_type),
  log_wage = case_when(
    wage==0 ~ 0,
    TRUE ~ log(wage)
  ),
  log_mce = case_when(
    HH_mce==0 ~ 0,
    TRUE~ log(HH_mce)
  )
)

hi<-na.omit(hi)
length(hi)
for(i in 1:16){
  j=i
  print(sum(is.na(hi[j])))
}
summary(hi)
str(hi)

log(exp(1))
attach(hi)
plot(hi$edu)
plot(hi$gender)
plot(hi$marital)
plot(hi$HH_type)
plot(religion)
plot(social_grp)


plot(hi$gender,hi$log_wage)
plot(marital,log_wage)
plot(log_mce,log_wage)
plot(edu,log_wage)
plot(religion,log_wage)
plot(social_grp,log_wage)

detach(hi)

names(hi)

# Install packages (only run once)
install.packages("estimatr") 
# install.packages("tidyverse") 

# Load the packages
library(estimatr)
library(tidyverse)

str(data_comb)
data_comb_copy = data_comb

names(hi)
model <- lm(
  log_wage ~ gender + age + marital + edu + HH_type +  
    HH_size+religion + social_grp + log_mce+
    edu * gender + gender*marital + social_grp *gender + age*gender, 
  data = hi
)

summary(model)

# Use lm_robust and specify the cluster variable in the 'clusters' argument
model_clustered <- lm_robust(
  log_wage ~ gender + age + marital + edu + HH_type +  
    HH_size+religion + social_grp + log_mce+
    edu * gender + gender*marital + social_grp *gender + age*gender, 
  data = hi,
  clusters = hh_id 
)

summary(model_clustered)
