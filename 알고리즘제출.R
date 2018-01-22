
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

#2번
Mat1<-matrix(c(1,0,1,1,1,
               0,0,0,1,1,
               0,1,1,1,1,
               0,1,1,1,1,
               0,1,1,1,1),5,5,byrow=T)
Mat2<-matrix(c(1,0,1,1,1,
               1,1,1,1,1,
               0,1,1,1,1,
               0,1,1,1,1,
               0,1,1,1,1),5,5,byrow=T)

'Largest' <- function(mat)
{  x<-numeric(25)
s<-0
k<-0
t<-0
u<-0
for (a in 1:5 )
{for (b in 1:5) mat[a,b]->x[(a-1)*5+b]
}

if (sum(x)==25 ) 25 
else if (is.null( for (c in 1:2)
{ for (d in 1:2) s = s + ifelse((mat[c,d]==1 &sum(mat[c,d],mat[c+1,d],mat[c+2,d],mat[c+3,d],
                                                  mat[c,d+1],mat[c+1,d+1],mat[c+2,d+1],mat[c+3,d+1],
                                                  mat[c,d+2],mat[c+1,d+2],mat[c+2,d+2],mat[c+3,d+2],
                                                  mat[c,d+3],mat[c+1,d+3],mat[c+2,d+3],mat[c+3,d+3])==16),1,0)})&(s>=1)) 16
else if  (is.null( for (c in 1:3)
{ for (d in 1:3) k = k + ifelse((mat[c,d]==1 &sum(mat[c,d],mat[c+1,d],mat[c+2,d],
                                                  mat[c,d+1],mat[c+1,d+1],mat[c+2,d+1],
                                                  mat[c,d+2],mat[c+1,d+2],mat[c+2,d+2])==9),1,0)})&(k>=1))    9
else if  (is.null( for (c in 1:4)
{ for (d in 1:4) t = t + ifelse((mat[c,d]==1 &sum(mat[c,d],mat[c+1,d],
                                                  mat[c,d+1],mat[c+1,d+1])==4),1,0)})&(t>=1))    4
else if  (is.null( for (c in 1:5)
{ for (d in 1:5) u = u + ifelse((mat[c,d]==1 &sum(mat[c,d])==1),1,0)})&(u>=1))    1
else 0
}   


Largest(Mat1)
Largest(Mat2)

#3ㅂ
'chopchop'<-function(a,b,c,d,e)
{min((a*b*c+a*c*d+a*d*e),(a*b*c+c*d*e+a*c*e),(b*c*d+b*d*e+a*b*d),(b*c*d+a*b*d+a*d*e),(c*d*e+b*c*e+a*b*e))}
chopchop(10,20,5,30,15)

#4
'solveEquation'<-function(A,x,b)
{  n<-10
B<-matrix(1,n,1)
if (is.null(b)&is.null( 
  for (i in 1:n)
  {0->s
    for (j in 1:n)
    {(s=s+A[i,j]*x[j,1])
      s->B[i,1]
    }})) B
ifelse(  (is.null( 
  for (i in 1:n)
  {0->s
    for (j in 1:n)
    {(s=s+A[i,j]*x[j,1])
      s->B[i,1]
    }})&all.equal(B,b)),"CORRECT","INCORRECT")

}