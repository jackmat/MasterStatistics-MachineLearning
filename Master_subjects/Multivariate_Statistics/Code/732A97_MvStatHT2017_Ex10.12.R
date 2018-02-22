n<- 70
alpha<-0.05

R<-cbind(
c(1,0.8,0.26,0.67,0.34),
c(0.8,1,0.33,0.59,0.34),
c(0.26,0.33,1,0.37,0.21),
c(0.67,0.59,0.37,1,0.35),
c(0.34,0.34,0.21,0.35,1))

R11<-R[1:2,1:2]
R12<-R[1:2,3:5]
R21<-R[3:5,1:2]
R22<-R[3:5,3:5]



p<-nrow(R11)
q<-nrow(R22)

invR11<-solve(R11)
invR22<-solve(R22)

## a)
Eq10_10matrixR1<-invR11%*%R12%*%invR22%*%R21
eigEq10_10<-eigen(Eq10_10matrixR1)
rho<-sqrt(eigEq10_10$values)

TwolnLambda_Eq10_39<- (-1)*n*log(prod(1-rho^2)) ## H0 Sigma==0
CritValue_Eq10_39<-qchisq(1-alpha,p*q)

if (TwolnLambda_Eq10_39>CritValue_Eq10_39){## H0(1) rho1=/=0, rho2==0
    print("H0 Sigma==0 rejected")
    k<-1
    Eq10_41<-(-1)*(n-1-(p+q+1)/2)*log(prod(1-(rho[(k+1):(length(rho))])^2))
    CritValue_Eq10_41<-qchisq(1-alpha,(p-k)*(q-k))
    if (Eq10_41>CritValue_Eq10_41){print("H0(1) rho1=/=0, rho2==0 rejected")}
    else{print("H0(1) rho1=/=0, rho2==0 not rejected")}
}else{
    print("H0 Sigma==0 not rejected")
}


## b)
Eq10_11_a<-invR11%*%R12%*%invR22%*%R21
Eq10_11_b<-invR22%*%R21%*%invR11%*%R12

eigEq10_11_a<-eigen(Eq10_11_a)
eigEq10_11_b<-eigen(Eq10_11_b)

a1<-eigEq10_11_a$vectors[,1]
## we need to scale a1 so that Var(U1)=1 see p544
a1<-a1/(sqrt(sum(a1^2)+2*prod(a1)*0.8))

b1<-eigEq10_11_b$vectors[,1]
## we need to scale b1 so that Var(V1)=1 see p544
b1<-b1/(sqrt(sum(b1^2)+2*(b1[1]*b1[2]*R22[1,2]+b1[1]*b1[3]*R22[1,3]+b1[2]*b1[3]*R22[2,3])))


## c)
CorrZ11U1<-R11[1,]%*%a1
CorrZ21U1<-R11[2,]%*%a1
CorrZ11V1<-R12[1,]%*%b1
CorrZ21V1<-R12[2,]%*%b1

CorrZ12U1<-R21[1,]%*%a1
CorrZ22U1<-R21[2,]%*%a1
CorrZ32U1<-R21[3,]%*%a1
CorrZ12V1<-R22[1,]%*%b1
CorrZ22V1<-R22[2,]%*%b1
CorrZ32V1<-R22[3,]%*%b1

