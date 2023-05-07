# These are the libraries used in the project. 
library(psych)
library(pscl)
library(AER)
library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lmtest)
library(flexmix)
library(moments)

lit=read.csv("finale.csv",header = 1)
print(lit)
# X1999,X2003,X2011,X2015,X2015 were divided by 1000 to give E99,E03,E11,E15,E19 
summary(lit$X1999)
sum(lit$X1999)
table(lit$X1999)
range(lit$X1999)
IQR(lit$X1999)
quantile(lit$X1999)
var(lit$X1999)
sd(lit$X1999)
skewness(lit$X1999)
kurtosis(lit$X1999)
summary(lit$X2003)
sum(lit$X2003)
table(lit$X2003)
range(lit$X2003)
IQR(lit$X2003)
quantile(lit$X2003)
var(lit$X2003)
sd(lit$X2003)
skewness(lit$X2003)
kurtosis(lit$X2003)
summary(lit$X2011)
sum(lit$X2011)
table(lit$X2011)
range(lit$X2011)
IQR(lit$X2011)
quantile(lit$X2011)
var(lit$X2011)
sd(lit$X2011)
skewness(lit$X2011)
kurtosis(lit$X2011)
summary(lit$X2015)
sum(lit$X2015)
table(lit$X2015)
range(lit$X2015)
IQR(lit$X2015)
quantile(lit$X2015)
var(lit$X2015)
sd(lit$X2015)
skewness(lit$X2015)
kurtosis(lit$X2015)
summary(lit$X2019)
sum(lit$X2019)
table(lit$X2019)
range(lit$X2019)
IQR(lit$X2019)
quantile(lit$X2019)
var(lit$X2019)
sd(lit$X2019)
skewness(lit$X2019)
kurtosis(lit$X2019)
#i need to bring out the decimal values so it will be easy to visualize the data
STATES=lit$ESTATES
sta=lit$E99
tis=lit$E03
tic=lit$E11
cat=lit$E15
pat=lit$E19
lac=data.frame(STATES,sta,tis,tic,cat,pat)
print(lac)
#i want to start with 1999 elections. this will plot 1999 election result against the states
lop=data.frame(STATES,sta)
print(lop)
ggplot(data = lop ,aes(x=STATES,y=sta, fill=STATES))+
  geom_bar(stat="identity")+
  geom_text(aes(label=sta),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  labs(title = "1999 VOTES",x="STATES",y="VOTES")
cis=data.frame(STATES,tis)
print(cis)
ggplot(data = cis ,aes(x=STATES,y=tis, fill=STATES))+
  geom_bar(stat="identity")+
  geom_text(aes(label=tis),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  labs(title = "2003 VOTES",x="STATES",y="VOTES")
iti=data.frame(STATES,tic)
print(iti)
ggplot(data = iti ,aes(x=STATES,y=tic, fill=STATES))+
  geom_bar(stat="identity")+
  geom_text(aes(label=tic),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  labs(title = "2011 VOTES",x="STATES",y="VOTES")
ati=data.frame(STATES,cat)
print(ati)
ggplot(data = ati ,aes(x=STATES,y=cat, fill=STATES))+
  geom_bar(stat="identity")+
  geom_text(aes(label=cat),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  labs(title = "2015 VOTES",x="STATES",y="VOTES")
tat=data.frame(STATES,pat)
print(tat)
ggplot(data = tat ,aes(x=STATES,y=pat, fill=STATES))+
  geom_bar(stat="identity")+
  geom_text(aes(label=pat),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  labs(title = "2019 VOTES",x="STATES",y="VOTES")
#i want visualise geographical zones
tii=data.frame(STATES=c("JIGAWA","KADUNA","KANO","KASTINA","KEBBI","SOKOTO","ZAMFARA"),
               var1=c(548.60,1676.03,904.71,1193.40,512.23,354.43,380.08),
               var2=c(1147.95,2192.25,2339.79,1711.21,879.83,1017.85,1106.57),
               var3=c(1140.77,2569.96,2673.23,1639.53,924.10,909.81,942.68),
               var4=c(1071.89,1650.20,2172.45,1481.71,715.12,876.37,780.18),
               var5=c(1149.92,1709.01,1964.75,1619.19,803.76,925.94,597.22))
til=tii %>%gather(key=var,value=Value,var1:var5)
print(til)
h=ggplot(data=til,aes(x=var,y=Value,fill=STATES))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=Value),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "NORTHWEST GEOPOLITICAL ZONE",x="YEARS",y="VOTES")
print(h)
h + theme(plot.background = element_rect(fill="pink"),panel.background = element_rect(fill="brown",colour = "black"))
act=data.frame(STATES=c("ADAMAWA","BAUCHI","BORNO","GOMBE","TARABA","YOBE"),
               var1=c(845.11,1176.54,915.98,844.54,871.04,311.58),
               var2=c(994.03,1739.51,1336.48,1010.18,923.60,643.39),
               var3=c(907.71,1610.09,1177.65,770.02,739.07,622.12),
               var4=c(661.21,1039.78,515.01,473.44,607.72,491.77),
               var5=c(860.76,1061.96,955.21,580.65,741.56,586.14))
ipo=act %>%gather(key=var,value=Value,var1:var5)
print(ipo)
a=ggplot(data=ipo,aes(x=var,y=Value,fill=STATES))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=Value),vjust=1.6,color="white",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "NORTHEAST GEOPOLITICAL ZONE",x="YEARS",y="VOTES")
print(a)
a + theme(plot.background = element_rect(fill="white"),panel.background = element_rect(fill="black",colour = "white"))
opi=data.frame(STATES=c("BENUE","FCT","KOGI","KWARA","NASSARAWA","NIGER","PLATEAU"),
               var1=c(1252.96,99.02,984.71,659.60,597.01,871.13,672.44),
               var2=c(1248.90,274.62,898.69,624.70,741.29,1052.79,1120.93),
               var3=c(1047.71,398.09,561.78,414.75,694.53,1019.17,1411.12),
               var4=c(703.13,316.02,439.29,461.40,521.64,844.68,1000.69),
               var5=c(763.87,451.41,553.50,486.25,599.40,896.98,1062.86))
oip=opi %>%gather(key=var,value=Value,var1:var5)
print(oip)
b=ggplot(data=oip,aes(x=var,y=Value,fill=STATES))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=Value),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "NORTHCENTRAL GEOPOLITICAL ZONE",x="YEARS",y="VOTES")
print(b)
b + theme(plot.background = element_rect(fill="orange"),panel.background = element_rect(fill="blue",colour = "black"))
cit=data.frame(STATES=c("AKWAIBOM","BAYELSA","CROSSRIVER","DELTA","EDO","RIVERS"),
               var1=c(883.28,610.03,876.16,816.57,679.78,1565.60),
               var2=c(1308.33,742.92,1238.18,1171.87,1182.32,2171.22),
               var3=c(1232.40,506.69,726.34,1398.58,621.29,1854.12),
               var4=c(1028.55,371.74,465.91,1284.85,522.79,1584.77),
               var5=c(605.14,335.86,446.05,882.25,599.23,666.59))
lip=cit %>%gather(key=var,value=Value,var1:var5)
print(lip)
c=ggplot(data=lip,aes(x=var,y=Value,fill=STATES))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=Value),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "SOUTHSOUTH GEOPOLITICAL ZONE",x="YEARS",y="VOTES")
print(c)
c + theme(plot.background = element_rect(fill="red"),panel.background = element_rect(fill="green",colour = "black"))
sit=data.frame(STATES=c("EKITI","LAGOS","OGUN","ONDO","OSUN","OYO"),
               var1=c(713.69,1751.98,475.90,801.80,794.64,921.81),
               var2=c(424.06,1939.19,1365.37,995.08,783.91,1083.81),
               var3=c(261.86,1945.04,543.72,486.84,512.71,863.54),
               var4=c(309.45,1495.98,559.61,582.44,663.37,928.61),
               var5=c(393.71,1156.59,605.94,586.83,731.88,891.08))
alc=sit %>%gather(key=var,value=Value,var1:var5)
print(alc)
e=ggplot(data=alc,aes(x=var,y=Value,fill=STATES))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=Value),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "SOUTHWEST GEOPOLITICAL ZONE",x="YEARS",y="VOTES")
print(e)
e + theme(plot.background = element_rect(fill="grey"),panel.background = element_rect(fill="purple",colour = "black"))
sss=data.frame(STATES=c("ABIA","ANAMBRA","EBONYI","ENUGU","IMO"),
               var1=c(535.92,833.18,345.92,835.59,736.11),
               var2=c(769.17,897.25,807.77,1144.89,1052.89),
               var3=c(1188.33,1157.24,502.89,814.01,1409.85),
               var4=c(401.05,703.41,393.34,585.63,731.92),
               var5=c(344.47,625.04,379.39,451.06,542.78))
sap=sss %>%gather(key=var,value=Value,var1:var5)
print(sap)
f=ggplot(data=sap,aes(x=var,y=Value,fill=STATES))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=Value),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "SOUTHEAST GEOPOLITICAL ZONE",x="YEARS",y="VOTES")
print(f)
f + theme(plot.background = element_rect(fill="yellow"),panel.background = element_rect(fill="cyan",colour = "black"))






#for states
YEARS= c("1999","2003","2011","2015","2019")
ics=c(535.92,769.17,1188.33,401.05,344.47)
cis=data.frame(YEARS,ics)
print(cis)
g=ggplot(data=cis,aes(x=YEARS,y=ics,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=ics),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "ABIA STATE",x="YEARS",y="VOTES")
print(g)
g + theme(plot.background = element_rect(fill="yellow"),panel.background = element_rect(fill="cyan",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
tas=c(833.18,897.25,1157.24,703.41,625.04)
cap=data.frame(YEARS,tas)
print(cap)
i=ggplot(data=cap,aes(x=YEARS,y=tas,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=tas),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "ANAMBRA STATE",x="YEARS",y="VOTES")
print(i)
i + theme(plot.background = element_rect(fill="yellow"),panel.background = element_rect(fill="cyan",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
ebo=c(345.92,807.77,502.89,393.34,379.39)
nyi=data.frame(YEARS,ebo)
print(nyi)
j=ggplot(data=nyi,aes(x=YEARS,y=ebo,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=ebo),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  
  theme_minimal()+
  labs(title = "EBONYI STATE",x="YEARS",y="VOTES")
print(j)
j + theme(plot.background = element_rect(fill="yellow"),panel.background = element_rect(fill="cyan",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
enu=c(835.59,1144.89,814.01,585.63,451.06)
ngu=data.frame(YEARS,enu)
print(ngu)
k=ggplot(data=ngu,aes(x=YEARS,y=enu,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=enu),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  
  theme_minimal()+
  labs(title = "ENUGU STATE",x="YEARS",y="VOTES")
print(k)
k + theme(plot.background = element_rect(fill="yellow"),panel.background = element_rect(fill="cyan",colour = "white"))
YEARS= c("1999","2003","2011","2015","2019")
imo=c(736.11,1052.89,1409.85,731.92,542.78)
owe=data.frame(YEARS,imo)
print(owe)
l=ggplot(data=owe,aes(x=YEARS,y=imo,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=imo),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  
  theme_minimal()+
  labs(title = "IMO STATE",x="YEARS",y="VOTES")
print(l)
l + theme(plot.background = element_rect(fill="yellow"),panel.background = element_rect(fill="cyan",colour = "black"))

YEARS= c("1999","2003","2011","2015","2019")
eki=c(713.69,424.06,261.86,309.45,393.71)
iti=data.frame(YEARS,eki)
print(iti)
m=ggplot(data=iti,aes(x=YEARS,y=eki,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=eki),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  
  theme_minimal()+
  labs(title = "EKITI STATE",x="YEARS",y="VOTES")
print(m)
m + theme(plot.background = element_rect(fill="grey"),panel.background = element_rect(fill="purple",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
lag=c(1751.98,1939.19,1945.04,1495.98,1156.59)
gos=data.frame(YEARS,lag)
print(gos)
n=ggplot(data=gos,aes(x=YEARS,y=lag,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=lag),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "LAGOS STATE",x="YEARS",y="VOTES")
print(n)
n + theme(plot.background = element_rect(fill="grey"),panel.background = element_rect(fill="purple",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
ogu=c(475.90,1365.37,543.72,559.61,605.94)
gun=data.frame(YEARS,ogu)
print(gun)
o=ggplot(data=gun,aes(x=YEARS,y=ogu,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=ogu),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "OGUN STATE",x="YEARS",y="VOTES")
print(o)
o + theme(plot.background = element_rect(fill="grey"),panel.background = element_rect(fill="purple",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
osu=c(794.64,783.91,512.71,663.37,731.88)
sun=data.frame(YEARS,osu)
print(sun)
p=ggplot(data=sun,aes(x=YEARS,y=osu,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=osu),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "OSUN STATE",x="YEARS",y="VOTES")
print(p)
p + theme(plot.background = element_rect(fill="grey"),panel.background = element_rect(fill="purple",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
ond=c(801.80,995.08,486.84,582.44,586.83)
ndo=data.frame(YEARS,ond)
print(ndo)
q=ggplot(data=ndo,aes(x=YEARS,y=ond,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=ond),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "ONDO STATE",x="YEARS",y="VOTES")
print(q)
q + theme(plot.background = element_rect(fill="grey"),panel.background = element_rect(fill="purple",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
oyo=c(921.18,1083.81,863.54,928.61,891.08)
iba=data.frame(YEARS,oyo)
print(iba)
r=ggplot(data=iba,aes(x=YEARS,y=oyo,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=oyo),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "OYO STATE",x="YEARS",y="VOTES")
print(r)
r + theme(plot.background = element_rect(fill="grey"),panel.background = element_rect(fill="purple",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
akw=c(883.28,1308.33,1232.40,1028.55,605.14)
bom=data.frame(YEARS,akw)
print(bom)
s=ggplot(data=iba,aes(x=YEARS,y=akw,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=akw),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "AKWAIBOM STATE",x="YEARS",y="VOTES")
print(s)
s + theme(plot.background = element_rect(fill="red"),panel.background = element_rect(fill="green",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
bay=c(610.03,742.92,506.69,371.74,335.86)
lsa=data.frame(YEARS,bay)
print(lsa)
t=ggplot(data=lsa,aes(x=YEARS,y=bay,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=bay),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "BAYELSA STATE",x="YEARS",y="VOTES")
print(t)
t + theme(plot.background = element_rect(fill="red"),panel.background = element_rect(fill="green",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
crs=c(876.16,1238.18,726.34,465.91,446.05)
riv=data.frame(YEARS,crs)
print(riv)
u=ggplot(data=iba,aes(x=YEARS,y=crs,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=crs),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "CROSSRIVER STATE",x="YEARS",y="VOTES")
print(u)
u + theme(plot.background = element_rect(fill="red"),panel.background = element_rect(fill="green",colour = "black"))

YEARS= c("1999","2003","2011","2015","2019")
del=c(816.57,1171.87,1398.58,1284.85,882.25)
lta=data.frame(YEARS,del)
print(lta)
v=ggplot(data=lta,aes(x=YEARS,y=del,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=del),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "DELTA STATE",x="YEARS",y="VOTES")
print(v)
v + theme(plot.background = element_rect(fill="red"),panel.background = element_rect(fill="green",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
edo=c(679.78,1118.32,621.29,522.79,599.23)
ben=data.frame(YEARS,edo)
print(ben)
w=ggplot(data=ben,aes(x=YEARS,y=edo,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=edo),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "EDO STATE",x="YEARS",y="VOTES")
print(w)
w + theme(plot.background = element_rect(fill="red"),panel.background = element_rect(fill="green",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
riv=c(1565.60,2171.22,1854.12,1584.77,666.59)
har=data.frame(YEARS,riv)
print(har)
x=ggplot(data=har,aes(x=YEARS,y=riv,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=riv),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "RIVERS STATE",x="YEARS",y="VOTES")
print(x)
x + theme(plot.background = element_rect(fill="red"),panel.background = element_rect(fill="green",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
ben=c(1252.96,1248.90,1047.71,703.13,763.87)
nue=data.frame(YEARS,ben)
print(nue)
y=ggplot(data=nue,aes(x=YEARS,y=ben,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=ben),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "BENUE STATE",x="YEARS",y="VOTES")
print(y)
y + theme(plot.background = element_rect(fill="orange"),panel.background = element_rect(fill="blue",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
fct=c(99.02,274.62,398.09,316.02,451.41)
abu=data.frame(YEARS,fct)
print(abu)
z=ggplot(data=abu,aes(x=YEARS,y=fct,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=fct),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  
  theme_minimal()+
  labs(title = "FEDERAL CAPITAL TERRITORY",x="YEARS",y="VOTES")
print(z)
z + theme(plot.background = element_rect(fill="orange"),panel.background = element_rect(fill="blue",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
kog=c(984.71,898.69,561.78,439.29,553.50)
lok=data.frame(YEARS,kog)
print(lok)
A=ggplot(data=lok,aes(x=YEARS,y=kog,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=kog),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "KOGI STATE",x="YEARS",y="VOTES")
print(A)
A + theme(plot.background = element_rect(fill="orange"),panel.background = element_rect(fill="blue",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
kwa=c(659.60,624.70,414.75,461.40,486.25)
ilo=data.frame(YEARS,kwa)
print(ilo)
B=ggplot(data=ilo,aes(x=YEARS,y=kwa,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=kwa),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "KWARA STATE",x="YEARS",y="VOTES")
print(B)
B + theme(plot.background = element_rect(fill="orange"),panel.background = element_rect(fill="blue",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
nas=c(597.01,741.29,694.53,521.64,599.40)
laf=data.frame(YEARS,nas)
print(laf)
C=ggplot(data=laf,aes(x=YEARS,y=nas,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=nas),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "NASSARAWA STATE",x="YEARS",y="VOTES")
print(C)
C + theme(plot.background = element_rect(fill="orange"),panel.background = element_rect(fill="blue",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
nig=c(871.13,1052.79,1019.17,844.68,896.98)
min=data.frame(YEARS,nig)
print(min)
D=ggplot(data=min,aes(x=YEARS,y=nig,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=nig),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "NIGER STATE",x="YEARS",y="VOTES")
print(D)
D + theme(plot.background = element_rect(fill="orange"),panel.background = element_rect(fill="blue",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
pla=c(672.44,1120.93,1411.12,1000.69,1062.86)
jos=data.frame(YEARS,pla)
print(jos)
E=ggplot(data=jos,aes(x=YEARS,y=pla,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=pla),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "PLATEAU STATE",x="YEARS",y="VOTES")
print(E)
E + theme(plot.background = element_rect(fill="orange"),panel.background = element_rect(fill="blue",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
ada=c(845.11,994.03,907.71,661.21,860.76)
yol=data.frame(YEARS,ada)
print(yol)
F=ggplot(data=yol,aes(x=YEARS,y=ada,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=ada),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "ADAMAWA STATE",x="YEARS",y="VOTES")
print(F)
F + theme(plot.background = element_rect(fill="white"),panel.background = element_rect(fill="black",colour = "white"))
YEARS= c("1999","2003","2011","2015","2019")
bau=c(1176.54,1739.51,1610.09,1039.78,1061.96)
chi=data.frame(YEARS,bau)
print(chi)
G=ggplot(data=chi,aes(x=YEARS,y=bau,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=bau),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "BAUCHI STATE",x="YEARS",y="VOTES")
print(G)
G + theme(plot.background = element_rect(fill="white"),panel.background = element_rect(fill="black",colour = "white"))
YEARS= c("1999","2003","2011","2015","2019")
bor=c(915.98,1336.48,1177.65,515.01,955.21)
mad=data.frame(YEARS,bor)
print(mad)
H=ggplot(data=mad,aes(x=YEARS,y=bor,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=bor),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "BORNO STATE",x="YEARS",y="VOTES")
print(H)
H + theme(plot.background = element_rect(fill="white"),panel.background = element_rect(fill="black",colour = "white"))
YEARS= c("1999","2003","2011","2015","2019")
gom=c(844.54,1010.18,770.02,473.44,580.65)
mbe=data.frame(YEARS,gom)
print(mbe)
I=ggplot(data=mbe,aes(x=YEARS,y=gom,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=gom),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  
  theme_minimal()+
  labs(title = "GOMBE STATE",x="YEARS",y="VOTES")
print(I)
I + theme(plot.background = element_rect(fill="white"),panel.background = element_rect(fill="black",colour = "white"))
YEARS= c("1999","2003","2011","2015","2019")
tar=c(871.04,923.60,739.07,607.72,741.56)
jal=data.frame(YEARS,tar)
print(jal)
J=ggplot(data=jal,aes(x=YEARS,y=tar,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=tar),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "TARABA STATE",x="YEARS",y="VOTES")
print(J)
J + theme(plot.background = element_rect(fill="white"),panel.background = element_rect(fill="black",colour = "white"))
YEARS= c("1999","2003","2011","2015","2019")
yob=c(311.58,643.39,622.12,491.77,586.14)
dam=data.frame(YEARS,yob)
print(dam)
K=ggplot(data=dam,aes(x=YEARS,y=yob,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=yob),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "YOBE STATE",x="YEARS",y="VOTES")
print(K)
K + theme(plot.background = element_rect(fill="white"),panel.background = element_rect(fill="black",colour = "white"))
YEARS= c("1999","2003","2011","2015","2019")
jig=c(548.60,1147.95,1140.77,1071.89,1149.92)
dut=data.frame(YEARS,jig)
print(dut)
L=ggplot(data=dut,aes(x=YEARS,y=jig,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=jig),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "JIGAWA STATE",x="YEARS",y="VOTES")
print(L)
L + theme(plot.background = element_rect(fill="pink"),panel.background = element_rect(fill="brown",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
kad=c(1676.03,2192.25,2569.96,1650.20,1709.01)
una=data.frame(YEARS,kad)
print(una)
M=ggplot(data=una,aes(x=YEARS,y=kad,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=kad),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "KADUNA STATE",x="YEARS",y="VOTES")
print(M)
M + theme(plot.background = element_rect(fill="pink"),panel.background = element_rect(fill="brown",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
kan=c(904.71,2339.79,2673.23,2172.45,1964.75)
ano=data.frame(YEARS,kan)
print(ano)
N=ggplot(data=ano,aes(x=YEARS,y=kan,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=kan),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "KANO STATE",x="YEARS",y="VOTES")
print(N)
N + theme(plot.background = element_rect(fill="pink"),panel.background = element_rect(fill="brown",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
kas=c(1193.40,1711.21,1639.53,1481.71,1619.19)
ina=data.frame(YEARS,kas)
print(ina)
O=ggplot(data=ina,aes(x=YEARS,y=kas,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=kas),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "KASTINA STATE",x="YEARS",y="VOTES")
print(O)
O + theme(plot.background = element_rect(fill="pink"),panel.background = element_rect(fill="brown",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
keb=c(512.23,879.83,924.10,715.12,803.76)
bke=data.frame(YEARS,keb)
print(bke)
P=ggplot(data=bke,aes(x=YEARS,y=keb,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=keb),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "KEBBI STATE",x="YEARS",y="VOTES")
print(P)
P + theme(plot.background = element_rect(fill="pink"),panel.background = element_rect(fill="brown",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
sok=c(354.43,1017.85,909.81,876.37,925.94)
oto=data.frame(YEARS,sok)
print(oto)
Q=ggplot(data=oto,aes(x=YEARS,y=sok,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=sok),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "SOKOTO STATE",x="YEARS",y="VOTES")
print(Q)
Q + theme(plot.background = element_rect(fill="pink"),panel.background = element_rect(fill="brown",colour = "black"))
YEARS= c("1999","2003","2011","2015","2019")
zam=c(380.08,1106.57,942.68,780.18,597.22)
afa=data.frame(YEARS,zam)
print(afa)
R=ggplot(data=afa,aes(x=YEARS,y=zam,fill=YEARS))+
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label=zam),vjust=1.6,color="black",position = position_dodge(0.9),size=3.5)+
  theme_minimal()+
  labs(title = "ZAMFARA STATE",x="YEARS",y="VOTES")
print(R)
R + theme(plot.background = element_rect(fill="pink"),panel.background = element_rect(fill="brown",colour = "black"))

#poisson and negative binomial model
gaf=read.csv("mode.csv",header = 1)
print(gaf)


app=glm(formula = VOTES~PARTY+REGVOTERS, family ="poisson", data=gaf)
print(summary(app))

pap=glm.nb(VOTES~PARTY+REGVOTERS, data = gaf)
print(summary(pap))

BIC(app)
BIC(pap)

#Residual plot for Poisson regression
p_res <- resid(app)
plot(fitted(app), p_res, col='steelblue', pch=16,
     xlab='Predicted Votes', ylab='Standardized Residuals', main='Poisson')
abline(0,0)

#Residual plot for negative binomial regression 
nb_res <- resid(pap)
plot(fitted(pap), nb_res, col='steelblue', pch=16,
     xlab='Predicted Votes', ylab='Standardized Residuals', main='Negative Binomial')
abline(0,0)

#likelihood ratio test
lrtest(pap,app)








