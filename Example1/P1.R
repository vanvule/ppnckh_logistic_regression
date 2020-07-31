> #nhap du lieu:
> conc=c(1.35,1.6,1.75,1.85,1.95,2.05,2.15,2.25,2.35)
> like=c(13,19,67,45,71,50,35,7,1)
> dislike=c(0,0,2,5,8,20,31,49,12)
> total=like+dislike
> #tinh xac suat ua thich:
> prob=like/total
> #do thi voi cac diem du lieu thuc te:
> plot(prob~conc,pch=16,xlab="Concentraation")
> 
> #logistic:
> logmodel=glm(prob~conc,family="binomial",weight=total)
> summary(logmodel)

Call:
glm(formula = prob ~ conc, family = "binomial", weights = total)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.78226  -0.69052   0.07981   0.36556   1.36871  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)   22.708      2.266  10.021   <2e-16 ***
conc         -10.662      1.083  -9.849   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 198.7115  on 8  degrees of freedom
Residual deviance:   8.5568  on 7  degrees of freedom
AIC: 37.096

Number of Fisher Scoring iterations: 5

> plot(like/total~conc)
> lines(conc,logmodel~fitted,type="l",col="orange")
Error in xy.coords(x, y) : 'x' and 'y' lengths differ
> lines(conc,logmodel$fitted,type="l",col="orange")
> 