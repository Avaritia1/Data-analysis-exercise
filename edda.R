# Exercise 1 Post-operative nausea
### a)
#YES
df_1 = read.table("nauseatable.txt",head=T)
z=chisq.test(df_1)
residuals(z)
# 80% at least 5
### b)
df_1_vec = unlist(df_1,use.names = FALSE)
nausea =rep(c(0,1), each=3, times = c(df_1_vec))
medicine = rep(c(rownames(df_1),rownames(df_1)), times=c(df_1_vec))
df_1_reorg = data.frame(cbind(nausea,medicine))
xtabs(~medicine+nausea)

B = 1000
tstar = numeric(B)
for (i in 1:B){
    xstar = sample(medicine)
    tstar[i] = chisq.test(xtabs(~xstar+nausea))[[1]]
}
myt = chisq.test(xtabs(~medicine+nausea))[[1]]
pl = sum(tstar<myt)/B
pr = sum(tstar>myt)/B
p_tstar = min(pl,pr)
p_tstar

### c)


# Exercise 2 Airpollution
### a)
df_2 = read.table("airpollution.txt",head=T)
attach(df_2)
boxplot(df_2[,-1])
pairs(df_2[,-1]) # pairs(df_2)
library(corrplot)
cor(df_2)
corrplot(cor(df_2[,-1]))
## cook distance
par(mfrow=c(2,2))
for (i in names(df_2[,-c(1,6)])){
    fit = lm(paste('oxidant', '~', i))
    cook_d = round(cooks.distance(fit),2)
    print(cook_d)
    plot(cooks.distance(fit),type="b")
}
### b)
null = lm(oxidant~1,data=df_2)
lm_fit = lm(oxidant~., data=df_2)
lm_fit1 = lm(oxidant~wind,data=df_2)
par(mfrow=c(1,2))
plot(residuals(lm_fit1),wind)
x=residuals(lm(wind~temperature+humidity+insolation),data=df_2)
y=residuals(lm(oxidant~temperature+humidity+insolation),data=df_2)
plot(x,y,main="Added variable plot for wind",
     xlab="residual of wind", ylab="residual of oxidant")
abline(x,-y,col="blue")
# negative coefficient/related
library(car)
par(mfrow=c(2,2))
avPlots(lm_fit)
par(mfrow=c(1,1))

### c)
step(null,scope=formula(lm_fit),direction = "forward")
step(lm_fit,direction = "backward")
for (i in names(df_2[,-c(1,6)])){
    # cat("For variable:",i)
    print(summary(lm(paste('oxidant', '~', i))))
}
### d)
myfit = lm(formula = oxidant ~ wind + temperature + humidity, data = df_2)
#myfit = lm(formula = oxidant ~ wind + temperature, data = df_2)
newdata = data.frame("wind"=33,"temperature"=54,"humidity"=77,"insolation"=21)
predict(myfit, newdata, interval='prediction', level=0.95)
predict(myfit, newdata, interval='confidence', level=0.95)

# Exercise 3 Fruit flies
df_3 = read.table("fruitflies.txt",head=T)
df_3$longevity = log(df_3$longevity)
df_3$activity = as.factor(df_3$activity)
attach(df_3)
qqnorm(longevity, pch = 1, frame = FALSE)
qqline(longevity, col = "steelblue", lwd = 2)
plot(thorax,longevity)
boxplot(df_3)
plot(longevity~thorax,pch=c(15,16,17),col=c('green','blue','red'),
     main="scatter plot of thorax and longevity with activity")
legend(0.63, 4.6, legend=c("high", "low", "isolated"), pch= c(15, 16, 17), col=c("green", "red", "blue"))
m_l = lm(longevity~thorax,data=df_3[activity=='low',])
m_h = lm(longevity~thorax,data=df_3[activity=="high",])
m_i = lm(longevity~thorax,data=df_3[activity=="isolated",])
abline(m_l, col = "red")
abline(m_h, col = "green")
abline(m_i, col = "blue")
m1 = lm(longevity~activity,data=df_3)
anova(m1) 
summary(m1)
coeff = summary(m1)$coefficients[1:3]
c1 = coeff[1];c2 = c1+coeff[2];c3 = c1+coeff[3]
est = exp(c(c1,c2,c3));est
### b)
mod = lm(longevity~.,data=df_3)
anova(mod) # last factor real p-value
summary(mod)
drop1(mod, test = "F") # real p-value for anova
# isolated is the highest
flies_i = df_3[activity=="isolated",][[1]][c(1,25)]
flies_l = df_3[activity=="low",][[1]][c(1,25)]
flies_h = df_3[activity=="high",][[1]][c(1,25)]
evaule_l = predict(mod,data.frame('thorax'=flies_l,'activity'="low"))
evaule_h = predict(mod,data.frame('thorax'=flies_h,'activity'="high"))
evaule_i = predict(mod,data.frame('thorax'=flies_i,'activity'="isolated"))
exp(evaule_l)
exp(evaule_h)
exp(evaule_i)
## global
flies = c(min(thorax),max(thorax))
evaule_l = predict(mod,data.frame('thorax'=flies,'activity'="low"))
evaule_h = predict(mod,data.frame('thorax'=flies,'activity'="high"))
evaule_i = predict(mod,data.frame('thorax'=flies,'activity'="isolated"))
exp(evaule_l)
exp(evaule_h)
exp(evaule_i)
### c)
mod2 = lm(longevity~activity*thorax,data=df_3)
drop1(mod2,test="Chisq") # not reject
mod2 = lm(longevity~activity+thorax,data=df_3)
drop1(mod2,test="Chisq")
anova(mod2)
summary(mod2)

par(mfrow=c(2,2))
plot(mod2)
#mod2.res = resid(mod2)
#plot(thorax, mod2.res, 
#     ylab="Residuals", xlab="thorax",
#     main="Residual plot") 
#abline(0, 0, col='red')  
### d)
# with thorax
# Factor type is significant, but one-way ANOVA with only factor type is not correct!
### e)
df_33 = read.table("fruitflies.txt",head=T)
df_33$activity = as.factor(df_33$activity)
attach(df_33)
mod3 = lm(longevity~activity+thorax,data=df_33)
summary(mod3)
plot(mod3)
# not wise

par(mfrow=c(1,1))
# Exercise 4 Personalized system of instruction
### a)
df_4 = read.table("psi.txt",head=T)
attach(df_4)
lr = glm(passed~psi+gpa+psi:gpa, data=df_4, family = binomial)
summary(lr)
drop1(lr,test = "Chisq")
lr = glm(passed~psi+gpa, data=df_4, family = binomial)
summary(lr)
drop1(lr,test = "Chisq")
### b)
new_stu1 = data.frame(psi=1,gpa=3.0)
new_stu2 = data.frame(psi=0,gpa=3.0)
predict(lr,new_stu1,type="response")
predict(lr,new_stu2,type="response")
### c)
coef = summary(lr)$coefficients[2]
exp(coef)
# 无关
#n_pass = length(passed[passed==1])
#n_notpass = length(passed[passed==0])
#n_psi_pass = dim(df_4[(passed==1)&(psi==1),])[1]
#n_psi_not_pass = dim(df_4[(passed==0)&(psi==1),])[1]
#odds = n_pass/n_notpass # 0.5238
#odds_psi = n_psi_pass/n_psi_not_pass # 1.3333
#exp(odds_psi/odds)
### d)
df_4_p = df_4[,-3]
num_psi = df_4_p[df_4_p$psi==1,][[1]]
num_not_psi = df_4_p[df_4_p$psi==0,][[1]]
t1 = table(num_psi)
t2 = table(num_not_psi)
m = matrix(c(t1,t2),nrow=2,byrow = TRUE)
fisher.test(m)
### e)
# gpa is significant, but 2x2 fisher test is exactly p-value
# contigency table make no assumption


# Exercise 5 School awards
### a)
df_5 = read.table("awards.txt",head=T)
attach(df_5)
df_5$prog = as.factor(df_5$prog)
pr=glm(num_awards~prog,family=poisson,data=df_5)
summary(pr)
drop1(pr,test = "Chisq")

coefs = summary(pr)$coefficients[1:3]
c_1 = coefs[1];c_2 = c_1+coefs[2];c_3 = c_1+coefs[3]
esti = exp(c(c_1,c_2,c_3));esti
### b)
kruskal.test(num_awards,prog,data = df_5)
# can be used, all n_i > 5
### c)
lm5_fit = glm(num_awards~prog*math,family=poisson,data=df_5)
summary(lm5_fit)
drop1(lm5_fit,test='Chisq') # adopt interactioin
lm5_fit = glm(num_awards~prog+math,family=poisson,data=df_5)
summary(lm5_fit)
# prog3 best
new_stu_5 = data.frame("prog"=1,"math"=55)
new_stu_5$prog = as.factor(new_stu_5$prog)
predict(lm5_fit,new_stu_5,type="response")
