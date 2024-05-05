######question 1#######

hpprice <- wpull('hprice1')
model1 <- lm(price~assess+bdrms+lotsize+sqrft+colonial,data=hpprice)
model2 <- lm(price~assess+bdrms,data=hpprice)
model3 <- lm(price~assess+bdrms+lotsize,data=hpprice)
model4 <- lm(price~assess+bdrms+lotsize+sqrft,data=hpprice)
model6 <- lm(price~assess+bdrms+I(bdrms^2)+lotsize+sqrft+I(sqrft^2)+colonial,data=hpprice)
c(AIC(model1,model2,model3,model4,model6),BIC(model1,model2,model3,model4,model6))

#############question2 #######
gpa2 <- wpull('gpa2')
# index variable.name   type format                   variable.label
# 1      0           sat    int %10.0g               combined SAT score
# 2      1        tothrs    int %10.0g  total hours through fall semest
# 3      2        colgpa  float  %9.0g          GPA after fall semester
# 4      3       athlete   byte  %8.0g                    =1 if athlete
# 5      4      verbmath  float  %9.0g            verbal/math SAT score
# 6      5         hsize double %10.0g           size grad. class, 100s
# 7      6        hsrank    int %10.0g              rank in grad. class
# 8      7        hsperc  float  %9.0g high school percentile, from top
# 9      8        female   byte  %9.0g                     =1 if female
# 10     9         white   byte  %9.0g                      =1 if white
# 11    10         black   byte  %9.0g                      =1 if black
model1 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+female+athlete,data=gpa2)
summary(model1)
model2 <- lm(colgpa~hsize+hsperc+sat+female+athlete,data=gpa2)
model3 <- lm(colgpa~hsize+I(hsize^2)+hsperc+I(hsperc^2)+sat+I(sat^2)+female+athlete,data=gpa2)
model4 <- lm(colgpa~hsize+I(hsize^2)+hsperc+I(sat^2)+sat+female+athlete,data=gpa2)
c(AIC(model1,model2,model3,model4),BIC(model1,model2,model3,model4))

######question 3########
mlb1 <- wpull('mlb1')

model1 <- lm( log(salary) ~ teamsal+nl+years +games +atbats+runs  +hits  +doubles   +
                triples +hruns +rbis  +bavg  +bb+so+sbases+fldperc   +frstbase  +scndbase  +shrtstop  +
                thrdbase  +outfield  +catcher   +yrsallst  +hispan+black +whitepop  +blackpop  +hisppop   +pcinc +
                gamesyr   +hrunsyr   +atbatsyr  +allstar   +
                slugavg   +rbisyr+sbasesyr  +runsyr+
                percwhte  +percblck  +perchisp  +blckpb+
                hispph+whtepw+blckph+hisppb, data = mlb1)

model1_AIC <- step(model1)
model1_BIC <- step(model1,k=log(nrow(mlb1)))


########question 4 ########
rent <- wpull('rental')
# index variable.name  type format              variable.label
# 1     0          city  byte  %9.0g         city label, 1 to 64
# 2     1          year  byte  %9.0g                    80 or 90
# 3     2           pop float  %9.0g             city population
# 4     3        enroll float  %9.0g # college students enrolled
# 5     4          rent   int  %9.0g                average rent
# 6     5        rnthsg float  %9.0g       renter occupied units
# 7     6        tothsg float  %9.0g      occupied housing units
# 8     7        avginc float  %9.0g           per capita income
head(rent)
#1
rent <- pdata.frame(rent,index=c('city','year'))
rent$pctstu <- (rent$enroll/rent$pop)*100
model1 <- plm(log(rent)~year+log(pop)+log(avginc)+pctstu,model="pooling",data=rent)
summary(model1)
tidy(model1)
#3
model2  <-  plm(log(rent)~log(pop)+log(avginc)+(pctstu),model='fd',data=rent)
summary(model2)
#4
model3 <- plm(log(rent)~factor(year)+log(pop)+log(avginc)+(pctstu),model="within",effect="individual",data=rent)
summary(model3)

#####Question 5#########
murder <- wpull('murder')
# index variable.name  type format                 variable.label
# 1     0            id  byte  %8.0g               state identifier
# 2     1         state  str2    %9s                    postal code
# 3     2          year  byte  %8.0g                  87, 90, or 93
# 4     3        mrdrte float  %9.0g     murders per 100,000 people
# 5     4          exec  byte  %8.0g total executions, past 3 years
# 6     5          unem float  %9.0g              annual unem. rate
#2
murder <- murder %>% filter(year==93 | year ==90)
murder <- pdata.frame(murder,index=c('state','year'))
model1 <- plm(mrdrte~exec+unem,model="pooling",data=murder)
summary(model1)
#3
model2 <- plm(mrdrte~exec+unem,model="fd",data=murder)
summary(model2)
#4
tidyw(model1)
#5
max(murder$exec)
murder$exec
#6
murder <- murder %>%  filter(state!='TX')
model3 <- plm(mrdrte~exec+unem,model="fd",data=murder)
tidyw(model3)
tidy(model3)
#7
murder1 <- wpull('murder')
murder1 <- pdata.frame(murder1,index=c('state','year'))
model4 <- plm(mrdrte~year+exec+unem,model="within",data=murder1)
model5 <- plm(mrdrte~year+exec+unem,model="within",data=murder)
summary(model4)
summary(model5)

##########question 6#############
fare <- wpull('airfare')
# index variable.name  type format                   variable.label
# 1     0          year   int  %9.0g           1997, 1998, 1999, 2000
# 2     1            id   int  %9.0g                 route identifier
# 3     2          dist   int  %9.0g               distance, in miles
# 4     3        passen   int  %9.0g          avg. passengers per day
# 5     4          fare   int  %9.0g             avg. one-way fare, $
# 6     5       bmktshr float  %9.0g fraction market, biggest carrier
# 7     6         ldist float  %9.0g                    log(distance)
#1
fare <- pdata.frame(fare,index=c('id','year'))
model1 <- plm(log(fare)~year+bmktshr+ldist+I(ldist^2),model='pooling',data=fare)
summary(model1)
tidyw(model1)
#3
min(fare$dist)
#4
model2 <- plm(log(fare)~year+bmktshr+ldist+I(ldist^2),model='within',data=fare)
summary(model2)

######question 7#############
loanapp <- wpull('loanapp')
#1
model1 <- glm(approve~white,data=loanapp,family=binomial())
summary(model1)
model <- lm(approve~white,data=loanapp)
summary(model)
#2
model2 <- glm(approve~white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1+mortlat2+vr,
            data=loanapp,family=binomial())
tidy(model2)
######question 8######
alco <- wpull('alcohol')
# index variable.name  type format                                     variable.label
# 1      0         abuse  byte  %9.0g                                =1 if abuse alcohol
# 2      1        status  byte  %9.0g out of workforce = 1; unemployed = 2, employed = 3
# 3      2      unemrate float  %9.0g                            state unemployment rate
# 4      3           age  byte  %9.0g                                       age in years
# 5      4          educ  byte  %9.0g                                 years of schooling
# 6      5       married  byte  %9.0g                                      =1 if married
# 7      6       famsize  byte  %9.0g                                        family size
# 8      7         white  byte  %9.0g                                        =1 if white
# 9      8      exhealth  byte  %9.0g                          =1 if in excellent health
# 10     9      vghealth  byte  %9.0g                          =1 if in very good health
# 11    10    goodhealth  byte  %9.0g                               =1 if in good health
# 12    11    fairhealth  byte  %9.0g                               =1 if in fair health
# 13    12     northeast  byte  %9.0g                            =1 if live in northeast
# 14    13       midwest  byte  %9.0g                              =1 if live in midwest
# 15    14         south  byte  %9.0g                                =1 if live in south
# 16    15      centcity  byte  %9.0g                  =1 if live in central city of MSA
# 17    16     outercity  byte  %9.0g                         =1 if in outer city of MSA
# 18    17          qrt1  byte  %9.0g                 =1 if interviewed in first quarter
# 19    18          qrt2  byte  %9.0g                =1 if interviewed in second quarter
# 20    19          qrt3  byte  %9.0g                 =1 if interviewed in third quarter
# 21    20       beertax float  %9.0g                     state excise tax, $ per gallon
# 22    21        cigtax float  %9.0g                state cigarette tax, cents per pack
# 23    22       ethanol float  %9.0g               state per-capita ethanol consumption
# 24    23       mothalc  byte  %9.0g                          =1 if mother an alcoholic
# 25    24       fathalc  byte  %9.0g                          =1 if father an alcoholic
# 26    25       livealc  byte  %9.0g                         =1 if lived with alcoholic
# 27    26          inwf  byte  %9.0g                                   =1 if status > 1
# 28    27        employ  byte  %9.0g                                     =1 if employed
#1
sum(alco$employ==1)
8822/9822
sum(alco$abuse==1)
974/9822
#2
model1 <- lm(employ~abuse,data=alco)
tidyw(model1)
summary(model1)
#3
model2 <- glm(employ~abuse,data=alco,family=binomial())
summary(model2)
#4
fitted(model1)
fitted(model2)
#5
model3 <- lm(employ~abuse+age+I(age^2)+educ+I(educ^2)
          +married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3,data=alco)
summary(model3)
#6
model4 <- glm(employ~abuse+age+I(age^2)+educ+I(educ^2)+married+famsize+white+northeast+midwest+
                south+centcity+outercity+qrt1+qrt2+qrt3,data=alco,family=binomial())
summary(model4)
#7
model5 <- lm(abuse~age+I(age^2)+educ+I(educ^2)+married+famsize+white+northeast+midwest
            +south+centcity+outercity+qrt1+qrt2+qrt3+mothalc+fathalc,data=alco)
summary(model5)
############question 9########
fert <- wpull('fertil1')
# index variable.name type format             variable.label
# 1      0          year byte  %9.0g             72 to 84, even
# 2      1          educ byte  %9.0g         years of schooling
# 3      2         meduc byte  %9.0g         mother's education
# 4      3         feduc byte  %9.0g         father's education
# 5      4           age byte  %9.0g                   in years
# 6      5          kids byte  %9.0g       # children ever born
# 7      6         black byte  %9.0g               = 1 if black
# 8      7          east byte  %9.0g = 1 if lived in east at 16
# 9      8      northcen byte  %9.0g   = 1 if lived in nc at 16
# 10     9          west byte  %9.0g = 1 if lived in west at 16
# 11    10          farm byte  %9.0g       = 1 if on farm at 16
# 12    11      othrural byte  %9.0g   = 1 if other rural at 16
# 13    12          town byte  %9.0g = 1 if lived in town at 16
# 14    13        smcity byte  %9.0g = 1 if in small city at 16
#1
model1 <- glm(kids~as.factor(year)+educ+age+I(age^2)+black+east+
                northcen+west+farm+othrural+town+smcity,data=fert,family=poisson())
summary(model1)
#3
residuals(model1)
r <- glm(kids~as.factor(year)+educ+age+I(age^2)+black+east+
           northcen+west+farm+othrural+town+smcity,data=fert,family=poisson()) %>%
  predict(type="response") %>% cor(fert$kids)
r
model2 <- lm(kids~as.factor(year)+educ+age+I(age^2)+black+east+
                northcen+west+farm+othrural+town+smcity,data=fert,family=poisson())
summary(model2)
