##########question 1#########
#voteA = B0 + B1 ln[expendA] + B2 ln[expendB] + B3 prystrA + u
vote <- wpull('vote1')
# index variable.name  type format              variable.label
# 1     0         state  str2    %9s           state postal code
# 2     1      district  byte  %3.0f      congressional district
# 3     2        democA  byte  %3.2f         =1 if A is democrat
# 4     3         voteA  byte  %5.2f          percent vote for A
# 5     4       expendA float  %8.2f camp. expends. by A, $1000s
# 6     5       expendB float  %8.2f camp. expends. by B, $1000s
# 7     6      prtystrA  byte  %5.2f        % vote for president

#3
modlea <- lm(voteA~log(expendA)+log(expendB)+prtystrA,data=vote)
summary(modlea)
tidyw(modlea)
#4
summary(lm(voteA~log(expendA)+I(log(expendB)-log(expendA))+prtystrA,data=vote))

#####question 2######
lawsch <-wpull('lawsch85')
# index variable.name  type format             variable.label
# 1      0          rank   int  %9.0g         law school ranking
# 2      1        salary float  %9.0g     median starting salary
# 3      2          cost   int  %9.0g            law school cost
# 4      3          LSAT   int  %9.0g          median LSAT score
# 5      4           GPA float  %9.0g         median college GPA
# 6      5        libvol   int  %9.0g no. volumes in lib., 1000s
# 7      6       faculty   int  %9.0g             no. of faculty
# 8      7           age   int  %9.0g     age of law sch., years
# 9      8        clsize   int  %9.0g     size of entering class
# 10     9         north  byte  %9.0g     =1 if law sch in north
# 11    10         south  byte  %9.0g     =1 if law sch in south
# 12    11          east  byte  %9.0g      =1 if law sch in east
# 13    12          west  byte  %9.0g      =1 if law sch in west

# ln[salary] = b0 + b1LSAT + b2GP A + b3 ln[libvol] + b4 ln[cost] + b5rank + u
head(lawsch)
model <- lm(log(salary)~ LSAT + GPA +log(libvol) +log(cost) +rank ,data=lawsch)
tidy(model)
#2
lawsch2<-na.omit(lawsch)  # removing the na values 
model1 <- lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank,data=lawsch[!is.na(faculty)&!is.na(clsize)])
modela <- lm(log(salary)~ LSAT+GPA+log(libvol)+log(cost)+rank,data=lawsch2)
tidy(modela)
summary(modela)
modelb <- lm(log(salary)~ log(libvol)+log(cost)+rank,data=lawsch2)
summary(modelb)
anova(modela,modelb)
#3
modelc <- lm(log(salary)~ LSAT + GPA +log(libvol) +log(cost) +rank+ clsize+faculty,data=lawsch2)
summary(modelc)
modeld <- lm(log(salary)~ LSAT + GPA +log(libvol) +log(cost) +rank,data=lawsch2)
summary(modeld)
anova(modelc,modeld)
#4
lawsch[,.(salary,rank,east,west,north,south)]
modele <- lm(log(salary)~ rank+ north+south+east+west,data=lawsch)
tidy(modele)

######question 3 ########
hprice <- wpull('hprice1')
# index variable.name  type format               variable.label
# 1     0         price float  %9.0g          house price, $1000s
# 2     1        assess float  %9.0g       assessed value, $1000s
# 3     2         bdrms  byte  %9.0g              number of bdrms
# 4     3       lotsize float  %9.0g   size of lot in square feet
# 5     4         sqrft   int  %9.0g size of house in square feet
# 6     5      colonial  byte  %9.0g =1 if home is colonial style


hprice$sqrft
#ln[price] = b0 + b1sqrf t + b2bdrms + u
model <- lm(log(price)~sqrft+bdrms,data=hprice)
tidy(model)
summary(model)
#1
model1 <- lm(log(price)~I(sqrft-(150*bdrms))+bdrms,data=hprice)
tidy(model1,conf.int=TRUE)
confint(model1)
summary(model1)
#2
model2 <- lm(log(price)~I(sqrft-bdrms)+bdrms,data=hprice)
tidy(model2,conf.int=TRUE)
summary(model2)

########question 4 ###########
wage2 <- wpull('wage2')
# index variable.name type format                variable.label
# 1      0          wage  int  %9.0g              monthly earnings
# 2      1         hours byte  %9.0g          average weekly hours
# 3      2            IQ  int  %9.0g                      IQ score
# 4      3           KWW byte  %9.0g knowledge of world work score
# 5      4          educ byte  %9.0g            years of education
# 6      5         exper byte  %9.0g      years of work experience
# 7      6        tenure byte  %9.0g   years with current employer
# 8      7           age byte  %9.0g                  age in years
# 9      8       married byte  %9.0g                 =1 if married
# 10     9         black byte  %9.0g                   =1 if black
# 11    10         south byte  %9.0g           =1 if live in south
# 12    11         urban byte  %9.0g            =1 if live in SMSA
# 13    12          sibs byte  %9.0g            number of siblings
# 14    13       brthord byte  %9.0g                   birth order
# 15    14         meduc byte  %9.0g            mother's education
# 16    15         feduc byte  %9.0g            father's education

#1 ln[wage] = ??0 + ??1educ + ??2exper + ??3tenure + u.
model <- lm(log(wage)~educ+exper+tenure,data=wage2)
tidyw(model)
#2  b2=b3
modela <- lm(log(wage)~educ+exper+I(exper+tenure),data=wage2)
tidy(modela,conf.int = T)

#########question 5###########
subs <- wpull('401ksubs')
# index variable.name  type format               variable.label
# 1      0         e401k  byte  %9.0g     =1 if eligble for 401(k)
# 2      1           inc float  %9.0g        annual income, $1000s
# 3      2          marr  byte  %9.0g                =1 if married
# 4      3          male  byte  %9.0g        =1 if male respondent
# 5      4           age  byte  %9.0g                     in years
# 6      5         fsize  byte  %9.0g                  family size
# 7      6        nettfa float  %9.0g net total fin. assets, $1000
# 8      7         p401k  byte  %9.0g  =1 if participate in 401(k)
# 9      8          pira  byte  %9.0g               =1 if have IRA
# 10     9         incsq float  %9.0g                        inc^2
# 11    10         agesq   int  %9.0g                        age^2

#1
head(subs1)
table(subs[,.(fsize==1)])
#2 nettf a = ??0 + ??1inc + ??2age + u
subs1 <- subs %>% filter(fsize ==1)
model <- lm(nettfa~inc+age,data=subs1)
summary(model)
augment(model)
subs1$newy <- log(augment(model)$.resid^2)
model2 <- lm(newy~inc+age,data=subs1)
summary(model2)
subs1$h <- exp(augment(model2)$.fitted)
model3 <-lm(nettfa~inc+age,weights=1/h,data=subs1)
tidyw(model3)
#3
min(subs1$age)
min(subs1$inc)
#4 b2=1
tstat <-(0.84266-1)/0.09202
tstat
tstat <-(0.84266-0.5)/0.09202
tstat
pnorm(-abs(tstat))*2
#5
model1 <- lm(nettfa~inc,data=subs1)
summary(model1)
cor(subs1$inc,subs1$age)

#######question 6#######
kie <- wpull('kielmc')

# index variable.name  type format                  variable.label
# 1      0          year   int  %9.0g                    1978 or 1981
# 2      1           age   int  %9.0g                    age of house
# 3      2         agesq float  %9.0g                           age^2
# 4      3           nbh  byte  %9.0g               neighborhood, 1-6
# 5      4           cbd float  %9.0g dist. to cent. bus. dstrct, ft.
# 6      5         intst float  %9.0g        dist. to interstate, ft.
# 7      6        lintst float  %9.0g                      log(intst)
# 8      7         price float  %9.0g                   selling price
# 9      8         rooms  byte  %9.0g                # rooms in house
# 10     9          area   int  %9.0g         square footage of house
# 11    10          land float  %9.0g              square footage lot
# 12    11         baths  byte  %9.0g                     # bathrooms
# 13    12          dist float  %9.0g dist. from house to incin., ft.
# 14    13         ldist float  %9.0g                       log(dist)
# 15    14          wind  byte  %9.0g  prc. time wind incin. to house
# 16    15        lprice float  %9.0g                      log(price)
# 17    16           y81  byte  %9.0g              =1 if year == 1981
# 18    17         larea float  %9.0g                       log(area)
# 19    18         lland float  %9.0g                       log(land)
# 20    19      y81ldist float  %9.0g                       y81*ldist
# 21    20      lintstsq float  %9.0g                        lintst^2
# 22    21       nearinc  byte  %9.0g             =1 if dist <= 15840
# 23    22      y81nrinc  byte  %9.0g                     y81*nearinc
# 24    23        rprice float  %9.0g             price, 1978 dollars
# 25    24       lrprice float  %9.0g                     log(rprice)

kielmc <- kie %>% filter(year==1981)
head(kielmc)
#1 ln[price] = ??0 + ??1 ln[dist] + u
model <- lm(log(price)~ldist,data=kielmc)
tidy(model)
summary(model)
#2
model2 <- lm(log(price)~log(dist)+log(intst)+log(area)+ log(land)+ rooms+ baths+ age,data=kielmc) 
summary(model2)
#3
model3 <- lm(log(price)~log(dist)+log(intst)+log(area)+ log(land)+ rooms+ baths+ age + I(log(intst)^2),data=kielmc) 
summary(model3)
cor(kielmc$lintst,kielmc$dist)
#4
model4 <- lm(log(price)~log(dist)+log(intst)+log(area)+ log(land)+ rooms+ baths+ age + I(log(intst)^2)+I(ldist^2)
             ,data=kielmc)
summary(model4)

#############question 7#############
wage1 <- wpull('wage1')
# index variable.name  type format                  variable.label
# 1      0          wage float  %8.2g         average hourly earnings
# 2      1          educ  byte  %8.0g              years of education
# 3      2         exper  byte  %8.0g      years potential experience
# 4      3        tenure  byte  %8.0g     years with current employer
# 5      4      nonwhite  byte  %8.0g                  =1 if nonwhite
# 6      5        female  byte  %8.0g                    =1 if female
# 7      6       married  byte  %8.0g                   =1 if married
# 8      7        numdep  byte  %8.0g            number of dependents
# 9      8          smsa  byte  %8.0g              =1 if live in SMSA
# 10     9      northcen  byte  %8.0g =1 if live in north central U.S
# 11    10         south  byte  %8.0g   =1 if live in southern region
# 12    11          west  byte  %8.0g    =1 if live in western region
# 13    12      construc  byte  %8.0g  =1 if work in construc. indus.
# 14    13       ndurman  byte  %8.0g  =1 if in nondur. manuf. indus.
# 15    14      trcommpu  byte  %8.0g  =1 if in trans, commun, pub ut
# 16    15         trade  byte  %8.0g    =1 if in wholesale or retail
# 17    16      services  byte  %8.0g        =1 if in services indus.
# 18    17      profserv  byte  %8.0g     =1 if in prof. serv. indus.
# 19    18       profocc  byte  %8.0g    =1 if in profess. occupation
# 20    19       clerocc  byte  %8.0g    =1 if in clerical occupation
# 21    20       servocc  byte  %8.0g     =1 if in service occupation

#1 ln[wage] = b0 + b1educ + b2exper + b3exper2 + u 
model <- lm(log(wage)~educ+exper+I(exper^2),data=wage1)
summary(model)
#4
count_exp <- wage1 %>% filter(exper >=29)
nrow(count_exp)

#########question 8 ############
wage2 <- wpull('wage2')
# index variable.name type format                variable.label
# 1      0          wage  int  %9.0g              monthly earnings
# 2      1         hours byte  %9.0g          average weekly hours
# 3      2            IQ  int  %9.0g                      IQ score
# 4      3           KWW byte  %9.0g knowledge of world work score
# 5      4          educ byte  %9.0g            years of education
# 6      5         exper byte  %9.0g      years of work experience
# 7      6        tenure byte  %9.0g   years with current employer
# 8      7           age byte  %9.0g                  age in years
# 9      8       married byte  %9.0g                 =1 if married
# 10     9         black byte  %9.0g                   =1 if black
# 11    10         south byte  %9.0g           =1 if live in south
# 12    11         urban byte  %9.0g            =1 if live in SMSA
# 13    12          sibs byte  %9.0g            number of siblings
# 14    13       brthord byte  %9.0g                   birth order
# 15    14         meduc byte  %9.0g            mother's education
# 16    15         feduc byte  %9.0g            father's education

#1 log(wage) = b0 + b1educ + b2exper + b3educ × exper + u.
model <- lm(log(wage)~educ+exper+I(educ*exper),data=wage2)
tidy(model)
#5.95+0.0440 educ-0.0215 exper+0.00320 (educ*exper)
#2
cor(wage2$educ,wage2$exper)
#3
model <- lm(log(wage)~educ+exper+I(educ*exper),data=wage2)
tidy(model)
#4
modelb <- lm(log(wage)~educ+exper+I(educ*(-10+exper)),data=wage2)
tidy(modelb,conf.int = TRUE)

########question 9######
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

#1 sat = ??0 + ??1hsize + ??2hsize^2 + u
head(gpa2)
#1
model <- lm(sat~hsize+I(hsize^2),data=gpa2)
tidy(model)
summary(model)
#4 
modela <- lm(log(sat)~hsize+I(hsize^2),data=gpa2)
tidy(modela)

##########Question 10##########
hprice <- wpull('hprice1')
# index variable.name  type format               variable.label
# 1     0         price float  %9.0g          house price, $1000s
# 2     1        assess float  %9.0g       assessed value, $1000s
# 3     2         bdrms  byte  %9.0g              number of bdrms
# 4     3       lotsize float  %9.0g   size of lot in square feet
# 5     4         sqrft   int  %9.0g size of house in square feet
# 6     5      colonial  byte  %9.0g =1 if home is colonial style

#1
summary(lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=hprice))
#2
model <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=hprice)
predict_model <- predict(model,data.frame(lotsize = 20000,sqrft = 2500,bdrms = 4))
exp(predict_model)
#3
summary(lm(price~lotsize+sqrft+bdrms,data=hprice,scipen=999))

