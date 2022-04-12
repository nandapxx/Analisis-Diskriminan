####### Diskriminan #######

## persiapan data
data=read.csv(file.choose(), sep = ";")
data
n=nrow(data);n
p=ncol(data)-1;p
attach(data)

#### PENGUJIAN ASUMSI #####

## Normalitas Multivariat
# H0: data multivariat normal vs H1: data tidak multivariat normal
library(MVN)
mvn(data[,-1],mvnTest = "hz")

## Varians homogen antar kategori
# H0: varians antar kategori homogen vs H1: varians antar kategori tidak homogen
# H0 ditolak jk pval>alfa
install.packages("biotools")
library("biotools")
boxM(data=data[,-1],group=data[,1])

## Rata-rata antar kategori berbeda
# H0: rata2 kategori 1 sama dengan rata2 kategori 2  
# H1: rata2 kategori 1 tidak sama dengan rata2 kategori 2
k1=data[which(Y==1),-1];k1
k2=data[which(Y==2),-1];k2
n1=nrow(k1);n1
n2=nrow(k2);n2

# rata2 variabel X dari setiap kelompok Y
mu1=colMeans(k1)
mu2=colMeans(k2)

# membangun matriks kovarians gabungan
S1=cov(k1);S1
S2=cov(k2);S2
Spl=((n1-1)*S1+(n2-1)*S2)/(n1+n2-2);Spl

# Statistik Uji
T2=t(mu1-mu2)%*%solve((1/n1+1/n2)*Spl)%*%(mu1-mu2);T2
Ttab=((p*(n-2))/(n-p-1))*qf(1-0.05,p,n-p-1);Ttab
Kes=if(T2 > Ttab) "H0 ditolak" else "H0 diterima";Kes

######## DISKRIMINAN 2 KATEGORI #########
## matriks X dengan Y terurut
X=rbind(k1,k2);X

## kombinasi linier fisher
# urutan dari yang terbesar merupakan urutan kontribusi
a=solve(Spl)%*%(mu1-mu2);a

## skor diskriminan
Z=as.matrix(X)%*%a;Z
Z1=Z[1:n1];Z1
Z2=Z[n1+1:n2];Z2

## cutting score
Z1bar=mean(Z1);Z1bar
Z2bar=mean(Z2);Z2bar
m=(n1*Z2bar+n2*Z1bar)/(n1+n2);m

## hit ratio
kat1=matrix(0,n1,1)
for (i in 1:n1) {
  kat1[i]=if(Z1[i]>=m) 1 else 2
}
kat2=matrix(0,n2,1)
for (i in 1:n2) {
  kat2[i]=if(Z2[i]>=m) 1 else 2
}
kat1
kat2

b=length(kat1[which(kat1==1)]);b
c=length(kat1[which(kat1==2)]);c
d=length(kat2[which(kat2==1)]);d
e=length(kat2[which(kat2==2)]);e
kes=matrix(c(b,c,d,e),2,2);kes
dimnames(kes)=list(Estimasi=c("K1","K2"),Observasi=c("K1","K2"))
kes[c("K1","K2"),c("K1","K2")]

ratio=(b+e)/n 
ratio
#####################################################################

######## DISKRIMINAN k KATEGORI #########
install.packages("MASS")
library(MASS)

#####################################################################
attach(iris)
n=nrow(iris)
y=as.matrix(iris[,5])
y
x=as.matrix(iris[,-5])
x
#linear diskriminan utk liat prior prob, banyaknya baris data perkategori
q=lda(y~x)
q
model=predict(q,data=iris)
model
pred=table(Predicted=model$class, Obeservasi=Species)
pred
ratio=(pred[1,1]+pred[2,2]+pred[3,3])/n
ratio
