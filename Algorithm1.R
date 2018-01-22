
#a.게임에 대한 승률
k<-10000
i<-0
j<-0

while(k>0){sample(2:12,1,replace=T)->x
  
  if (x==7|x==11)
  {k=k+1
  i=i+1
  j=j+1}
  else if (x==2|x==3|x==12)
  {k=k-1 
  i=i+1} 
  else if (is.null(repeat{sample(2:12,1,replace=T)->y
    if (y==x) break})
    &(y!=7))
  {k=k+1
  i=i+1
  j=j+1}
  
  else (y==7)
  
  {k=k-1 
    i=i+1}  
}
k
i
j
(1-j/i)->"승률"
승률

#b.KY, SY의 초기 코인 수가 각각 12, 9라 할 때
k<-12
i<-0
j<-0

while(k>0&k<=21){sample(2:12,1,replace=T)->x
  
  if (x==7|x==11)
  {k=k+1
  i=i+1
  j=j+1}
  else if (x==2|x==3|x==12)
  {k=k-1 
  i=i+1} 
  else if (is.null(repeat{sample(2:12,1,replace=T)->y
    if (y==x) break})
    &(y!=7))
  {k=k+1
  i=i+1
  j=j+1}
  
  else (y==7)
  
  {k=k-1 
    i=i+1}  
}
k
i
j
(1-j/i)->"승률"
승률

#b.KY, SY의 초기 코인 수가 각각 20, 9라 할 때
k<-20
i<-0
j<-0

while(k>0&k<=29){sample(2:12,1,replace=T)->x
  
  if (x==7|x==11)
  {k=k+1
  i=i+1
  j=j+1}
  else if (x==2|x==3|x==12)
  {k=k-1 
  i=i+1} 
  else if (is.null(repeat{sample(2:12,1,replace=T)->y
    if (y==x) break})
    &(y!=7))
  {k=k+1
  i=i+1
  j=j+1}
  
  else (y==7)
  
  {k=k-1 
    i=i+1}  
}
k
i
j
(1-j/i)->"승률"
승률