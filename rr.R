#1번
cow=read.csv("cow_data.csv",header=T)
head(cow)
str(cow)
'my_function' <- function(cow)
{ ifelse (cow$age>=50 & (cow$grade==3 | cow$grade=="등외"),"폐기용","식용")}
my_function(cow)->is_edible
as.data.frame(is_edible)->is.edible
cbind(cow,is_edible)->cow_data
head(cow_data)

#2번
attach(cow)
as.character(address)->a
strsplit(a," ")->b
c<-list()
for (i in 1:length(b))
  (ifelse (length(b[[i]])==4,paste(b[[i]][1],b[[i]][2]),NA))->c[i]
unlist(c)->d
as.data.frame(cow$grade)->grade1
cow1<-as.data.frame(cbind(d,grade1))
names(cow1)<-c("address1","grade1")
cow2<-na.omit(cow1[cow1$grade1=="1++",])
cow3<-with(cow2,summary(address1))
head(as.data.frame(cow3),n=3)

#3ㅂ
as.character(address)->a
strsplit(a," ")->b
c<-list()
for (i in 1:length(b))
  (ifelse (length(b[[i]])==4,paste(b[[i]][1],b[[i]][2]),NA))->c[i]
unlist(c)->d
as.data.frame(as.numeric(cow$price))->price1
cow5<-as.data.frame(cbind(d,price1))
names(cow5)<-c("address1","price1")
cow6<-subset(cow5,(cow5$address1=="전라북도 정읍시" |
                     cow5$address1=="전라남도 고흥군" |
                     cow5$address1=="경기도 안성시") )
na.omit(aggregate(as.numeric(cow6$price1), by=list(cow6$address1), mean) )

#4번
month=as.data.frame(substr(cow$slaughter_date,5,6))
as.character(address)->a
strsplit(a," ")->b
c<-list()
for (i in 1:length(b))
  (ifelse (length(b[[i]])==4,paste(b[[i]][1],b[[i]][2]),NA))->c[i]
unlist(c)->d
cow8<-as.data.frame(cbind(d,month))
names(cow8)<-c("address1","month")
cow9<-subset(cow8,(cow8$address1=="전라북도 정읍시" |
                     cow8$address1=="전라남도 고흥군" |
                     cow8$address1=="경기도 안성시") )
freq<-summary(cow9$month)
plot(freq,xlab = " ",ylab=" ",type="l",main="월 별 가축 수")



