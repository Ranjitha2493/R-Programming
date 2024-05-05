#################question 1 ####################
con <- dbConnect(SQLite(),'wooldridge2.db')
dbListTables(con)
wage <- dbReadTable(con,'wage1')
wage <- data.table(wage)
dbReadTable(con,'wage1_labels')
dbDisconnect(con)
rm(con)
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

summary(wage$educ)# 1)Mean : 12.56, min : 0.00, max : 18.00
mean(wage$wage) #2) 5.91 it is seems low
#3)I considered report of 2011 CIP index for 1967 is 56.9 and for 2010 is 218.056
#4) hourly wage in 1967 is 5.91 ratio is 9.63 Hourly wage in 2010 will be 22.64 which is high than avg hourly rate in 2010= 12.5
female <- table(wage$female)
female          #5)274 men and 252 female

##############question2###########
con <- dbConnect(SQLite(),'wooldridge2.db')
dbListTables(con)
meap <- dbReadTable(con,'meap01')
meap <- data.table(meap)
dbReadTable(con,'meap01_labels')
dbDisconnect(con)
rm(con)
# index variable.name  type format                                variable.label
# 1     0         dcode float  %9.0g                                 district code
# 2     1         bcode   int  %9.0g                                 building code
# 3     2         math4 float  %9.0g       % students satisfactory, 4th grade math
# 4     3         read4 float  %9.0g    % students satisfactory, 4th grade reading
# 5     4         lunch float  %9.0g % students eligible for free or reduced lunch
# 6     5        enroll   int  %9.0g                             school enrollment
# 7     6        expend float  %9.0g                             total spending, $
# 8     7         exppp float  %9.0g         expenditures per pupil: expend/enroll

summary(meap$math4)
range(meap$math4) 
#1) min: 0.00 max :100 .This doesn't add much information as the range value 100 shows 100% students have satisfactory math grade which is not true
perfect_pass <- meap %>% filter(math4 == 100)
count(perfect_pass)  
(count(perfect_pass)/count(meap))*100 
# 2) 38 schools have perfect pass rate it is 2.1% of total sample
math_pass <- meap %>% filter(math4 == 50)
nrow(math_pass) # 3) 17 schools have math pass rate of 50%
mean(meap$math4)
mean(meap$read4) # 4) Reading is harder to pass
corelation <- cor(meap$math4,meap$read4)
corelation # 5) 0.8427281 math4 and read4 are strong positively related
mean(meap$exppp) 
sd(meap$exppp)
# 6) 5194.865 average expenditure, 1091.89 is standard deviation. SD shows wide variation in spending per pupil
((6000-5500)/6000)*100 # 7) School A spends 8.33% more than School B
100*(log(6000)-log(5500)) #8.701138

############question 3 ####################
con <- dbConnect(SQLite(),'wooldridge2.db')
dbListTables(con)
plank <- dbReadTable(con,'401k')
plank <- data.table(plank)
dbReadTable(con,'401k_labels')
dbDisconnect(con)
rm(con)
# index variable.name  type format                  variable.label
# 1     0         prate float  %7.0g     participation rate, percent
# 2     1         mrate float  %7.0g            401k plan match rate
# 3     2       totpart float  %7.0g         total 401k participants
# 4     3        totelg float  %7.0g    total eligible for 401k plan
# 5     4           age  byte  %7.0g                age of 401k plan
# 6     5        totemp float  %7.0g  total number of firm employees
# 7     6          sole  byte  %7.0g = 1 if 401k is firm's sole plan
# 8     7       ltotemp float  %9.0g                   log of totemp
mean(plank$mrate) 
mean(plank$prate)
#1) #average of plan match rate is 0.7315 and average participation rate is 87.363
model <- lm(prate~mrate,plank) 
summary(model) 
#2) prate= 83.0756 + 5.8612*mrate n=1534 R-squared = 0.0747
#3) Intercept implies that when mrate=0 participation rate is 83.1%. match rate increase by 1$ makes the participation rate increase by 5.8612 % points
83.0756 + (5.8612 * 3.5)
# 4)prate is 103.59% which is not possible as maximum can be 100% and around 10% of 1534 firms will have prate >100%  this simple regression model is giving 
#strange predictions for extreme values.
#5) mrate has R-squared value 7.41% variation in prate 

##############question 4#############
con <- dbConnect(SQLite(),'wooldridge2.db')
dbListTables(con)
ceosa <- dbReadTable(con,'ceosal2')
ceosa <- data.table(ceosa)
dbReadTable(con,'ceosal2_labels')
dbDisconnect(con)
rm(con)
# index variable.name  type format                 variable.label
# 1     0        salary   int  %9.0g      1990 compensation, $1000s
# 2     1           age  byte  %9.0g                       in years
# 3     2       college  byte  %9.0g         =1 if attended college
# 4     3          grad  byte  %9.0g =1 if attended graduate school
# 5     4        comten  byte  %9.0g             years with company
# 6     5        ceoten  byte  %9.0g      years as ceo with company
# 7     6         sales float  %9.0g      1990 firm sales, millions
# 8     7       profits   int  %9.0g         1990 profits, millions
# 9     8        mktval float  %9.0g market value, end 1990, mills.
mean(ceosa$salary) 
mean(ceosa$ceoten)
# 1) average salary is  865.8644 ie., 865864$ and average tenure with company 7.955
firstceo <- ceosa %>% filter (ceoten == 0)
firstceo           
max(ceosa$ceoten) 
#2) 5 CEOs who are in first year  and 37 years is the longest tenure as CEO
model1 <- lm(log(salary)~ceoten,data= ceosa)
summary(model1) 
#3)log(salary) = 6.505 + 0.0097 ceoten, n=177 and R-squared = 0.0132 one more year as CEO change in ceoten =1 implies 0.0097*100=0.97% increase in salary i.e.,
#approximately 1% increase in salary

#######################question 5#####################
con <- dbConnect(SQLite(),'wooldridge2.db')
dbListTables(con)
wage2 <- dbReadTable(con,'wage2')
wage2 <- data.table(wage2)
dbReadTable(con,'wage2_labels')
dbDisconnect(con)
rm(con)

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

summary(wage2) 
sd(wage2$IQ)
#1)average salary 957.9 and average IQ = 101.3 and Standard Deviation of IQ is 15.053
model2 <- lm(wage~IQ, data= wage2)
summary(model2)
8.303 * 15
#2)wage = 116.992 + 8.303 * IQ for n= 935 and R-squared = 0.0955,increase in IQ by 15 points implies increase of 241.537$ in 
#wage which is large impact
model3 <- lm(log(wage)~IQ, data=wage2)
summary(model3)
#3)log(wage) = 5.887 + 0.00881 *IQ  change in IQ by 15 points has 13.22% increase in wage

#######question 6 ##############

con <- dbConnect(SQLite(),'wooldridge2.db')
dbListTables(con)
meap9 <- dbReadTable(con,'meap93')
meap9 <- data.table(meap9)
dbReadTable(con,'meap93_labels')
dbDisconnect(con)
rm(con)

# index variable.name  type format                  variable.label
# 1      0       lnchprg float  %9.0g  perc of studs in sch lnch prog
# 2      1        enroll   int  %9.0g               school enrollment
# 3      2         staff float  %9.0g         staff per 1000 students
# 4      3        expend   int  %9.0g             expend. per stud, $
# 5      4        salary float  %9.0g          avg. teacher salary, $
# 6      5      benefits   int  %9.0g        avg. teacher benefits, $
# 7      6      droprate float  %9.0g       school dropout rate, perc
# 8      7      gradrate float  %9.0g    school graduation rate, perc
# 9      8        math10 float  %9.0g    perc studs passing MEAP math
# 10     9         sci11 float  %9.0g perc studs passing MEAP science
# 11    10       totcomp float  %9.0g               salary + benefits



test <- meap9 %>% select(5,10) %>% filter(expend > 5000)
test  # 1) each dollar spent is having lesser results on math and is having diminishing effect
model14<- lm(math10~log(expend), data= meap9) 
#2) d(math10)/d(expend)=b1/expend .... Here d represents change so b1* d(expend)/expend which is 10/100*b1 i.,e b1/10
summary(model14) #3) math = -69.341 + 11.164 log(expend) n=408 R-squared = 0.0296 
model4 #4) when there is an increase of 10% in expend  as calculated above we get of 1.164 points in math10
summary(meap9) #5)log(expand) > 15.1685 max of expend is 7419 so log(7419)= 8.9118 which is less than 15.1685 so it's not much of worry in data set

#########question 7###############
con <- dbConnect(SQLite(),'wooldridge2.db')
dbListTables(con)
hprice1 <- dbReadTable(con,'hprice1')
hprice1 <- data.table(hprice1)
dbReadTable(con,'hprice1_labels')
dbDisconnect(con)
rm(con)
# index variable.name  type format               variable.label
# 1     0         price float  %9.0g          house price, $1000s
# 2     1        assess float  %9.0g       assessed value, $1000s
# 3     2         bdrms  byte  %9.0g              number of bdrms
# 4     3       lotsize float  %9.0g   size of lot in square feet
# 5     4         sqrft   int  %9.0g size of house in square feet
# 6     5      colonial  byte  %9.0g =1 if home is colonial style

summary(hprice1)
model15 <- lm(price~bdrms+sqrft, data=hprice1) 
summary(model15)#1)price = -19.32+ 0.128 sqrft + 15.2 bdrms n=88 R^2 = 0.6319
#2) price = 15.2 bdrms since house is measured in thousands increase in price of house is 15200
#3) price = 0.128*140 + 15.2 * 1 increase in price of a house will be 33120. there is an increase in price as increase in size of the house
#4) R^2 = 0.632, 63.2% variation is explained by sqrft and bed rooms
#5)price = -19.32+ 0.128 * 2438 + 15.2*4 ==> 353.544*1000 ==> 353544 is the predicted selling price
#6)from above calculation predicted SP is 353544 and buyer bought for 300000, so the buyer underpaid 53544

####### question 8 #######
con <- dbConnect(SQLite(),'wooldridge2.db')
dbListTables(con)
ceosa <- dbReadTable(con,'ceosal2')
ceosa <- data.table(ceosa)
dbReadTable(con,'ceosal2_labels')
dbDisconnect(con)
rm(con)
# index variable.name  type format                 variable.label
# 1     0        salary   int  %9.0g      1990 compensation, $1000s
# 2     1           age  byte  %9.0g                       in years
# 3     2       college  byte  %9.0g         =1 if attended college
# 4     3          grad  byte  %9.0g =1 if attended graduate school
# 5     4        comten  byte  %9.0g             years with company
# 6     5        ceoten  byte  %9.0g      years as ceo with company
# 7     6         sales float  %9.0g      1990 firm sales, millions
# 8     7       profits   int  %9.0g         1990 profits, millions
# 9     8        mktval float  %9.0g market value, end 1990, mills.

summary(ceosa)
model15 <- lm(log(salary)~log(sales)+log(mktval), data= ceosa)
summary(model15)
#1) log(salary) = 4.62 + 0.162 log(sales) + 0.107 log(mkval) n=177 and R^2 =0.299
model16<- lm(log(salary)~log(sales)+log(mktval)+(profits), data = ceosa)
summary(model16)
#2) we cannot add profits as we have -ve profits value, sales and market value represent 30% of variation in salary which is not most
model17 <- lm(log(salary)~log(sales)+log(mktval)+(profits)+(ceoten), data = ceosa)
summary(model17)
#3)percentage return is 1.167 for another year as ceo tenure
cor(log(ceosa$mktval),ceosa$profits)
#4)coefficient correlation is 0.776, yes these 2 variables are highly correlated.profits and ceoten doenot change much on the ceo salary

################question 9 ##########
con <- dbConnect(SQLite(),'wooldridge2.db')
dbListTables(con)
attend1 <- dbReadTable(con,'attend')
attend1 <- data.table(attend1)
dbReadTable(con,'attend_labels')
dbDisconnect(con)
rm(con)

# index variable.name  type format               variable.label
# 1     0        attend  byte  %8.0g   classes attended out of 32
# 2     1       termGPA float  %9.0g                 GPA for term
# 3     2        priGPA float  %9.0g cumulative GPA prior to term
# 4     3           ACT  byte  %8.0g                    ACT score
# 5     4         final  byte  %8.0g             final exam score
# 6     5       atndrte float  %9.0g     percent classes attended
# 7     6         hwrte float  %9.0g   percent homework turned in
# 8     7         frosh  byte  %8.0g               =1 if freshman
# 9     8          soph  byte  %8.0g              =1 if sophomore

attend1$atndrte <- attend1$attend/32
summary(attend1)
#1) atndrte : min = 0.0625 max = 1 and average = 0.8171 
# priGPA : min = 0.857 max = 32 and average = 2.587
# ACT : min = 13 max= 32 and average= 22.51
model18 <- lm(atndrte~priGPA+ACT,data=attend1)
summary(model18)
#2) atndrte = 0.757 + 0.173 (priGPA) -0.0172 (ACT) n= 680 R^2 = 0.2906
# Intercept is 0.757 which means when the GPA prior to term and ACT score is zero the attendance rate is 0.757 however in this sample 
#data set we dont have any record with priGPA and ACT scores = 0
attend1$priGPA == 0
attend1$ACT == 0
#3) if the prior GPA is 1 and ACT is constant, then attendance rate will be 17.3% points more. Similarly with ACT score
# increase by 10 points then the attendance rate is worsen by 17.2 percent points.
#4)atndrte = 0.757 + 0.173 * 3.65 - 0.0172 *20 ==> 1.044 which is 104.4% attendance rate is not possible as we can have max of 100%
test <- attend1 %>% filter(priGPA== 3.65 & ACT == 20)
test
#4) Yes, we have one student with GPA 3.65 and ACT score 20
atndrteA <- 0.757 + 0.173 * 3.1 - 0.0172 *21
atndrteB <- 0.757 + 0.173 * 2.1 - 0.0172 *26
atndrteA - atndrteB 
#5) predicted difference is 0.259 i., 25.9

#############question 10#############
con <- dbConnect(SQLite(),'wooldridge2.db')
dbListTables(con)
htv <- dbReadTable(con,'htv')
htv <- data.table(htv)
dbReadTable(con,'htv_labels')
dbDisconnect(con)
rm(con)
# index variable.name  type format                  variable.label
# 1      0          wage float  %9.0g               hourly wage, 1991
# 2      1          abil float  %9.0g abil. measure, not standardized
# 3      2          educ  byte  %9.0g highest grade completed by 1991
# 4      3            ne  byte  %9.0g        =1 if in northeast, 1991
# 5      4            nc  byte  %9.0g        =1 if in nrthcntrl, 1991
# 6      5          west  byte  %9.0g             =1 if in west, 1991
# 7      6         south  byte  %9.0g            =1 if in south, 1991
# 8      7         exper  byte  %9.0g            potential experience
# 9      8      motheduc  byte  %9.0g           highest grade, mother
# 10     9      fatheduc  byte  %9.0g           highest grade, father
# 11    10      brkhme14  byte  %9.0g       =1 if broken home, age 14
# 12    11          sibs  byte  %9.0g              number of siblings
# 13    12         urban  byte  %9.0g       =1 if in urban area, 1991
# 14    13          ne18  byte  %9.0g             =1 if in NE, age 18
# 15    14          nc18  byte  %9.0g             =1 if in NC, age 18
# 16    15       south18  byte  %9.0g          =1 if in south, age 18
# 17    16        west18  byte  %9.0g           =1 if in west, age 18
# 18    17       urban18  byte  %9.0g     =1 if in urban area, age 18
# 19    18        tuit17 float  %9.0g         college tuition, age 17
# 20    19        tuit18 float  %9.0g         college tuition, age 18
summary(htv$educ)
table(htv$educ == 12)
#1) range is 14, 512 i.e., 41.62% men completed 12th grade but no higher grade, avg edu of men is 13.04 and of parents 12.3
# hence men have higher level of education
model18 <- lm(educ~motheduc+fatheduc, data= htv)
summary(model18)
#2) educ = 6.96 + 0.3042(motheduc) + 0.1903 (fatheduc) n=1230 and R^2= 0.2493
#24.93% of sample variation is explained by parents education, when father's education is fixed the men 
#education is changed by 30.42% by 1 change in mother's education
model19 <- lm(educ~motheduc+fatheduc+abil, data= htv)
summary(model19)
#3) educ = 8.44 + 0.189(motheduc) + 0.111 (fatheduc) + 0.502 (abil) the R^2 value before adding abil was 0.249 and after
# adding abil is 0.43 the relationship is getting stronger hence abil explains variations in edu even after controlling 
#for parent's educations
ablisq <- htv$abil^2
model20 <- lm(educ~motheduc+fatheduc+abil +ablisq , data= htv)
summary(model20)
#4)from above equation edu = 8.24 +0.19(motheduc)+0.108(fatheduc)+ 0.401(abil)+ 0.051(ablisq),
#d(edu)/d(abil)=0,  0 = 0.401+0.051*2(abil) ==> abil = -3.931 
# verifying 2nd derivative is positive as 0.102 >0
abilmen <- htv %>% filter (abil < -3.931) 
nrow(abilmen) #5) there are only 15 men who's ability is less than -3.931 which is small portion
edu_predicted <- 8.24 + 0.19 *12.18 + 0.108* 12.45 +0.401 *htv$abil +0.051*(ablisq)
plot(edu_predicted,htv$abil) #6) plotting scattered graph
