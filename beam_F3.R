## beamF3 지원하기 위한 r script
rm(list=ls())
library(readxl)
voteyes <- read_excel("~/Dropbox/book/final/voteyes.xlsx")
g1<-ggplot(data =voteyes, aes(x=qr, y=turnout, group=country))
g2<-g1+geom_line()+geom_point()+xlab("")+ylab("")
g3<-g2+facet_wrap(~country, nrow = 3, as.table = FALSE)+
  theme_bw(base_family = "NanumGothic")+guides(colour=guide_legend(title=NULL))+
  ggtitle("소득별 투표율")
print(g3)
library(readxl)
vote.occu <- read_excel("~/Dropbox/book/final/voteyes.xlsx", 
                        sheet = "직업")
str(vote.occu)
vote.occu$turnout<-100*vote.occu$vote/(vote.occu$no+vote.occu$vote)
g1<-ggplot(data =vote.occu, aes(x=직업, y=turnout, group=country))
g2<-g1+geom_line()+geom_point()+xlab("")+ylab("")+
  scale_x_continuous(breaks = c(1,3,6,9))
g3<-g2+facet_wrap(~country, nrow = 3, as.table = FALSE)+
  theme_bw(base_family = "NanumGothic")+guides(colour=guide_legend(title=NULL))+
  ggtitle("직업별 투표율")
print(g3)

load("~/Dropbox/now/allr1.RData") # 
require(broom) # for tidy()
require(knitr) # for kable()
mod.1<-glm(투표~당파+직업1+소득+성+나이+교육기간+가족, data = allr1, family = binomial) 
exp(coef(mod.1))
out <- tidy(mod.1)
kable(out)
library(dplyr) # for pipe (%>%) command
library(pixiedust)
s.mod<-glm(투표~당파+직업1+재분배+소득격차+소득증세+소득,data = allr1,family = binomial)
exp(coef(s.mod))
library(texreg)
texreg(list(mod.1,s.mod), use.packages=FALSE, scriptsize=FALSE,
       strong.signif=TRUE)
library(stargazer)
library(MASS)
pol.1<-polr(소득격차~직업1+소득+성+나이+학력+소득+당파, data = allr1, Hess = TRUE) 
#summary(pol.1)
exp(coef(pol.1))
pol.2<-polr(소득격차~직업1+교육기간+소득증세+소득, data = allr1, Hess = TRUE) 
exp(coef(pol.2))
stargazer(mod.1, s.mod, type="text",style = "ajps",align = TRUE)
stargazer(pol.1,pol.2, type="text",style = "ajps",
          dep.var.caption  = "소득격차해소정책에 대한 입장",align = TRUE)
texreg(list(pol.1,pol.2), use.packages=FALSE, label="tab:4", scriptsize=FALSE,
       strong.signif=TRUE)

screenreg(list(pol.1, pol.2))
htmlreg(list(pol.1, pol.2))

texreg(list(mod.1,s.mod), use.packages=FALSE, label="tab:4", scriptsize=FALSE,
       strong.signif=TRUE)


library(readxl)
voteys <- read_excel("~/Dropbox/book/final/voteys.xlsx", 
                     sheet = "시트1")
voteQ1<-subset(voteys, income=="1분위", select = c(country, income,turnout))
library(ggplot2)

ggplot(data = voteQ1, aes(x=country, y=turnout))+
  geom_bar(stat = "identity")+xlab("")

voteQ2<-subset(voteys, income=="q2", select = c(country, income,turnout))
ggplot(data = voteQ2, aes(x=country, y=turnout))+
  geom_bar(stat = "identity")+xlab("")

voteQ5<-subset(voteys, income=="5분위", select = c(country, income,turnout))
ggplot(data = voteQ5, aes(x=country, y=turnout))+
  geom_bar(stat = "identity")+xlab("")

voteus<-subset(voteys, country=="미국", select = c(country, income,turnout))
ggplot(data = voteus, aes(x=income, y=turnout))+theme_bw(base_family = "NanumGothic")+
  geom_bar(stat = "identity")+ggtitle("미국의 소득별 투표율 1987-2009")+
  geom_text(aes(label=turnout), vjust=1.6, color="white", size=5.5)+
  xlab("")+ylab("")+coord_cartesian(ylim = c(0,90)) 

votede<-subset(voteys, country=="독일", select = c(country, income,turnout))
ggplot(data = votede, aes(x=income, y=turnout))+theme_bw(base_family = "NanumGothic")+
  geom_bar(stat = "identity")+ggtitle("독일의 소득별 투표율 1987-2009")+
  geom_text(aes(label=turnout), vjust=1.6, color="white", size=5.5)+
  xlab("")+ylab("")+coord_cartesian(ylim = c(0,90)) 
