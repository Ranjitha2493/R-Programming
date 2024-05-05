#########question1 #########
mlb1 <- wpull('mlb1')
option()
# index variable.name  type format             variable.label
# 1      0        salary float  %9.0g         1993 season salary
# 2      1       teamsal float %10.0f               team payroll
# 3      2            nl  byte  %9.0g      =1 if national league
# 4      3         years  byte  %9.0g     years in major leagues
# 5      4         games   int  %9.0g        career games played
# 6      5        atbats   int  %9.0g             career at bats
# 7      6          runs   int  %9.0g         career runs scored
# 8      7          hits   int  %9.0g                career hits
# 9      8       doubles   int  %9.0g             career doubles
# 10     9       triples   int  %9.0g             career triples
# 11    10         hruns   int  %9.0g           career home runs
# 12    11          rbis   int  %9.0g      career runs batted in
# 13    12          bavg float  %9.0g     career batting average
# 14    13            bb   int  %9.0g               career walks
# 15    14            so   int  %9.0g         career strike outs
# 16    15        sbases   int  %9.0g        career stolen bases
# 17    16       fldperc   int  %9.0g       career fielding perc
# 18    17      frstbase  byte  %9.0g          = 1 if first base
# 19    18      scndbase  byte  %9.0g          =1 if second base
# 20    19      shrtstop  byte  %9.0g            =1 if shortstop
# 21    20      thrdbase  byte  %9.0g           =1 if third base
# 22    21      outfield  byte  %9.0g             =1 if outfield
# 23    22       catcher  byte  %9.0g              =1 if catcher
# 24    23      yrsallst  byte  %9.0g          years as all-star
# 25    24        hispan  byte  %9.0g             =1 if hispanic
# 26    25         black  byte  %9.0g                =1 if black
# 27    26      whitepop float  %9.0g         white pop. in city
# 28    27      blackpop float  %9.0g         black pop. in city
# 29    28       hisppop float  %9.0g      hispanic pop. in city
# 30    29         pcinc   int  %9.0g     city per capita income
# 31    30       gamesyr float  %9.0g   games per year in league
# 32    31       hrunsyr float  %9.0g         home runs per year
# 33    32      atbatsyr float  %9.0g           at bats per year
# 34    33       allstar float  %9.0g perc. of years an all-star
# 35    34       slugavg float  %9.0g    career slugging average
# 36    35        rbisyr float  %9.0g              rbis per year
# 37    36      sbasesyr float  %9.0g      stolen bases per year
# 38    37        runsyr float  %9.0g       runs scored per year
# 39    38      percwhte float  %9.0g      percent white in city
# 40    39      percblck float  %9.0g      percent black in city
# 41    40      perchisp float  %9.0g   percent hispanic in city
# 42    41        blckpb float  %9.0g             black*percblck
# 43    42        hispph float  %9.0g            hispan*perchisp
# 44    43        whtepw float  %9.0g             white*percwhte
# 45    44        blckph float  %9.0g             black*perchisp
# 46    45        hisppb float  %9.0g            hispan*percblck

#ln(salary) = B0 + B1years + B2gamesyr + B3bavg + B4hrunsyr + B5rbisyr + B6runsyr + B7fldperc
#+ B8allstar + B9frstbase + B10scndbase + B11thrdbase + B12shrtstop + B13catcher + u

#1
model <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar+frstbase+scndbase+thrdbase+
          shrtstop+catcher,data=mlb1)
tidy(model) 
summary(model)
(exp(0.254)-1)*100
#2
modela <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar,data=mlb1)
summary(modela)
anova(model,modela)
##########question 2##########
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
#1
colgpa = ??0 + ??1hsize + ??2hsize^2 + ??3hsperc + ??4sat + ??5female + ??6athlete + u 
model1 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+female+athlete,data=gpa2)
tidy(model1)
summary(model1)
#3
model2 <- lm(colgpa~hsize+I(hsize^2)+hsperc+female+athlete,data=gpa2)
tidy(model2)
summary(model2)
#4
model3 <-lm(colgpa~hsize+I(hsize^2)+hsperc+sat+I(female*athlete)+I((1-female)*(1-athlete))+I((1-female)*athlete),data=gpa2)
tidy(model3)
summary(model3)
#5
model4 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+I(sat*female)+female+athlete,data=gpa2)
tidy(model4)
summary(model4)

#####question 3##########
loanapp <- wpull('loanapp')
# index variable.name  type format                  variable.label
# 1      0           occ  byte  %9.0g                       occupancy
# 2      1       loanamt   int  %9.0g           loan amt in thousands
# 3      2        action  byte  %9.0g            type of action taken
# 4      3           msa   int  %9.0g          msa number of property
# 5      4       suffolk  byte  %9.0g   =1 if property in suffolk co.
# 6      5        appinc   int  %9.0g        applicant income, $1000s
# 7      6         typur  byte  %9.0g       type of purchaser of loan
# 8      7          unit  byte  %9.0g     number of units in property
# 9      8       married  byte  %9.0g         =1 if applicant married
# 10     9           dep  byte  %9.0g            number of dependents
# 11    10           emp  byte  %9.0g  years employed in line of work
# 12    11          yjob  byte  %9.0g               years at this job
# 13    12          self  byte  %9.0g             =1 if self employed
# 14    13       atotinc float  %9.0g            total monthly income
# 15    14      cototinc float  %9.0g      coapp total monthly income
# 16    15          hexp float  %9.0g         propose housing expense
# 17    16         price float  %9.0g                  purchase price
# 18    17         other float  %9.0g         other financing, $1000s
# 19    18           liq float  %9.0g                   liquid assets
# 20    19           rep  byte  %9.0g           no. of credit reports
# 21    20         gdlin   int  %9.0g credit history meets guidelines
# 22    21         lines float  %9.0g  no. of credit lines on reports
# 23    22         mortg  byte  %9.0g credit history on mortgage paym
# 24    23          cons  byte  %9.0g credit history on consumer stuf
# 25    24        pubrec  byte  %9.0g          =1 if filed bankruptcy
# 26    25          hrat float  %9.0g        housing exp, % total inc
# 27    26         obrat float  %9.0g        other oblgs, % total inc
# 28    27        fixadj  byte  %9.0g       fixed or adjustable rate?
# 29    28          term float  %9.0g          term of loan in months
# 30    29           apr float  %9.0g                 appraised value
# 31    30          prop  byte  %9.0g                type of property
# 32    31          inss  byte  %9.0g                      PMI sought
# 33    32         inson  byte  %9.0g                    PMI approved
# 34    33          gift  byte  %9.0g            gift as down payment
# 35    34        cosign  byte  %9.0g             is there a cosigner
# 36    35         unver  byte  %9.0g               unverifiable info
# 37    36        review   int  %9.0g        number of times reviewed
# 38    37          netw float  %9.0g                       net worth
# 39    38          unem float  %9.0g   unemployment rate by industry
# 40    39         min30  byte  %9.0g       =1 if minority pop. > 30%
# 41    40            bd  byte  %9.0g  =1 if boarded-up val > MSA med
# 42    41            mi  byte  %9.0g    =1 if tract inc > MSA median
# 43    42           old  byte  %9.0g   =1 if applic age > MSA median
# 44    43            vr  byte  %9.0g   =1 if tract vac rte > MSA med
# 45    44           sch  byte  %9.0g      =1 if > 12 years schooling
# 46    45         black  byte  %9.0g           =1 if applicant black
# 47    46        hispan  byte  %9.0g        =1 if applicant Hispanic
# 48    47          male  byte  %9.0g            =1 if applicant male
# 49    48        reject  byte  %9.0g               =1 if action == 3
# 50    49       approve  byte  %9.0g          =1 if action == 1 or 2
# 51    50        mortno  byte  %9.0g             no mortgage history
# 52    51      mortperf  byte  %9.0g          no late mort. payments
# 53    52      mortlat1  byte  %9.0g        one or two late payments
# 54    53      mortlat2  byte  %9.0g               > 2 late payments
# 55    54         chist  byte  %9.0g  =0 if accnts deliq. >= 60 days
# 56    55         multi  byte  %9.0g         =1 if two or more units
# 57    56       loanprc float  %9.0g                       amt/price
# 58    57         thick  byte  %9.0g                   =1 if rep > 2
# 59    58         white  byte  %9.0g           =1 if applicant white
#1
sum(as.numeric(loanapp$white==1))#white 1681
sum(as.numeric(loanapp$black==1))#black 191
sum(as.numeric(loanapp$hispan==1))#111
#2
model <- lm(approve~white,data=loanapp)
summary(model)
#3
model <- lm(approve~white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1+mortlat2+vr,
            data=loanapp)
summary(model)
tidy(model)
#4
model <- lm(approve~white + hrat + I(obrat*white) +obrat+ loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1+mortlat2+vr,
            data=loanapp)
summary(model)

############question 4##########
hprice <- wpull('hprice1')
# index variable.name  type format               variable.label
# 1     0         price float  %9.0g          house price, $1000s
# 2     1        assess float  %9.0g       assessed value, $1000s
# 3     2         bdrms  byte  %9.0g              number of bdrms
# 4     3       lotsize float  %9.0g   size of lot in square feet
# 5     4         sqrft   int  %9.0g size of house in square feet
# 6     5      colonial  byte  %9.0g =1 if home is colonial style
#1
model <- lm(price~lotsize+sqrft+bdrms,data=hprice)
summary(model)
tidy(model)
tidyw(model)
#2
model <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=hprice)
tidy(model)
tidyw(model)

##########question 5##########
gpa1 <- wpull('gpa1')
# index variable.name  type format                 variable.label
# 1      0           age  byte  %9.0g                       in years
# 2      1          soph  byte  %9.0g                =1 if sophomore
# 3      2        junior  byte  %9.0g                   =1 if junior
# 4      3        senior  byte  %9.0g                   =1 if senior
# 5      4       senior5  byte  %9.0g        =1 if fifth year senior
# 6      5          male  byte  %9.0g                     =1 if male
# 7      6        campus  byte  %9.0g           =1 if live on campus
# 8      7      business  byte  %9.0g           =1 if business major
# 9      8      engineer  byte  %9.0g        =1 if engineering major
# 10     9        colGPA float  %9.0g                        MSU GPA
# 11    10         hsGPA float  %9.0g                high school GPA
# 12    11           ACT  byte  %9.0g            'achievement' score
# 13    12         job19  byte  %9.0g          =1 if job <= 19 hours
# 14    13         job20  byte  %9.0g          =1 if job >= 20 hours
# 15    14         drive  byte  %9.0g          =1 if drive to campus
# 16    15          bike  byte  %9.0g        =1 if bicycle to campus
# 17    16          walk  byte  %9.0g           =1 if walk to campus
# 18    17       voluntr  byte  %9.0g        =1 if do volunteer work
# 19    18            PC  byte  %9.0g     =1 of pers computer at sch
# 20    19         greek  byte  %9.0g   =1 if fraternity or sorority
# 21    20           car  byte  %9.0g                  =1 if own car
# 22    21      siblings  byte  %9.0g            =1 if have siblings
# 23    22      bgfriend  byte  %9.0g       =1 if boy- or girlfriend
# 24    23         clubs  byte  %9.0g       =1 if belong to MSU club
# 25    24       skipped float  %9.0g   avg lectures missed per week
# 26    25       alcohol float  %9.0g avg # days per week drink alc.
# 27    26        gradMI  byte  %9.0g     =1 if Michigan high school
# 28    27      fathcoll  byte  %9.0g      =1 if father college grad
# 29    28      mothcoll  byte  %9.0g      =1 if mother college grad
#1
model <- lm(colGPA~hsGPA+ACT+skipped+PC,data=gpa1)
summary(model)
tidy(model)
tidyw(model)
res <- residuals(model)
fit <- predict(model)
#2

modeld <- lm(I(res^2)~fit+I(fit^2),data=gpa1) 
h<-predict(modeld)

#3
min(h)
modele <- lm(colGPA~hsGPA+ACT+skipped+PC,weights=(1/h),data=gpa1)
summary(modele)
tidy(modele)
#4
tidyw(modele)
###########Question 6#############
#3
bitcoin <- fread("C:/Ranjitha/R_learning/JasonParker/Assignment/bitcoin.csv")
fred1 <- fread ("C:/Ranjitha/R_learning/JasonParker/Assignment/fred.csv")

# bitcoin.ts <- ts(as.vector(bitcoin),start=c(2014,109),frequency = 365)
# plot(bitcoin.ts, xlim=c(2014,2020), xlab="Time", ylab="Price in USD", 
#      main="Bitcoin Price from 27/4/2013 to 24/02/2019")

fred <- merge(bitcoin,fred1,all=T,by='DATE')
#sorting the date in dataset
fred <- fred[order(as.Date(fred$DATE,format="%m/%d/%Y")),]
fred$DATE <- as.Date(fred$DATE, "%m/%d/%Y")
fred$time <- substr(fred$DATE,0,4)
#converting the values as numeric
fred$Bitcoin <- (as.numeric(fred$Bitcoin))
fred$DCOILWTICO <- (as.numeric(fred$DCOILWTICO))
fred$DEXUSEU <- (as.numeric(fred$DEXUSEU))
fred$GOLDAMGBD228NLBM <- (as.numeric(fred$GOLDAMGBD228NLBM))
fred$SP500 <- (as.numeric(fred$SP500))
fred$time <- (as.numeric(fred$time))
#ommiting NA values
fred <- na.omit(fred)



#4
ts.plot(fred$DCOILWTICO)
ts.plot(fred$DEXUSEU)
ts.plot(fred$GOLDAMGBD228NLBM)
ts.plot(fred$SP500)
ts.plot(fred$Bitcoin)


#5

DCOmodel <- lm(Bitcoin~DCOILWTICO,data=fred)
DEXmodel <- lm(Bitcoin~DEXUSEU,data=fred)
GOLDmodel <- lm(Bitcoin~GOLDAMGBD228NLBM,data=fred)
SPmodel <- lm(Bitcoin~SP500,data=fred)

tidy(DCOmodel)
tidy(GOLDmodel)
tidy(DEXmodel)
tidy(SPmodel)
summary(DCOmodel)
summary(GOLDmodel)
summary(DEXmodel)
summary(SPmodel)

#6
kpss.test(fred$Bitcoin)  
kpss.test(fred$Bitcoin,null="Trend")
kpss.test(diff(fred$Bitcoin)) #bitcoin is level stationary after 1st differencing

kpss.test(fred$DEXUSEU)
kpss.test(fred$DEXUSEU,null="Trend")
kpss.test(diff(fred$DEXUSEU))# euro exchange is level stationary after 1st differencing

rep.kpss(fred$DCOILWTICO) #oil is level stationary after 1st differencing
rep.kpss(fred$DEXUSEU)# euro exchange is level stationary after 1st differencing
rep.kpss(fred$GOLDAMGBD228NLBM)#gold is level stationary after 1st differencing
rep.kpss(fred$SP500)#SP500 is level stationary after 1st differencing

#7
DCOmodel <- lm(diff(Bitcoin)~diff(DCOILWTICO),data=fred)
DEXmodel <- lm(diff(Bitcoin)~diff(DEXUSEU),data=fred)
GOLDmodel <- lm(diff(Bitcoin)~diff(GOLDAMGBD228NLBM),data=fred)
SPmodel <- lm(diff(Bitcoin)~diff(SP500),data=fred)
tidy(DCOmodel)
tidy(DEXmodel)
tidy(GOLDmodel)
tidy(SPmodel)

summary(DCOmodel)
summary(DEXmodel)
summary(GOLDmodel)
summary(SPmodel)


#8
fred2017 <- fred %>% filter(DATE > '2016-12-31')
ts.plot(fred2017$DCOILWTICO)
ts.plot(fred2017$DEXUSEU)
ts.plot(fred2017$GOLDAMGBD228NLBM)
ts.plot(fred2017$SP500)
ts.plot(fred2017$Bitcoin)

#9
ggtsdisplay(fred2017$Bitcoin)
#10
modeld <- arima(fred2017$Bitcoin,c(20,1,20))
summary(modeld)
outp <- matrix(0,4^2,5)
count <- 1
for(i in 0:3){
  for(j in 0:3){
    modeld <- arima(fred2017$Bitcoin,c(i,1,j))
    outp[count,] <- c(i,1,j,AIC(modeld),BIC(modeld))
    count <- count + 1
  }
}
outp <- data.table(outp)
names(outp) <- c('p','d','q','aic','bic')
outp
outp[aic==0,]$aic <- 9999
outp[bic==0,]$bic <- 9999
outp[which.min(outp$aic),,] #   p d q      aic      bic 
#2 1 2 13616.39 13640.46
modeld <- arima(fred2017$Bitcoin,c(2,1,2))
summary(modeld)
#11
modele <- arima(fred2017$Bitcoin,
                c(2,1,2),
                seasonal=list(order=c(1,1,1),period=30))

modelf <- arima(fred2017$Bitcoin,
                c(2,1,2),
                seasonal=list(order=c(0,1,1),period=30),
)
AIC(modele)#13282.68
AIC(modelf)#13276.59

plot(forecast(modelf,h=30))
#12

dbit <- diff(fred2017$Bitcoin)
TSA::periodogram(dbit)#has 15 days spike
1/0.08
dco <- diff(fred2017$DCOILWTICO)
dex <- diff(fred2017$DEXUSEU)
gold <- diff(fred2017$GOLDAMGBD228NLBM)
sp <- diff(fred2017$SP500)
#13
fred2017$weekday <- as.factor(weekdays(fred2017$DATE))
nrows <- nrow(fred2017)
modelw <- lm(diff(Bitcoin)~weekday[2:nrows],data=fred2017)
summary(modelw)
x<- residuals(modelw)
TSA::periodogram(x,log='no',plot=TRUE,ylab="Periodogram", xlab="Frequency",lwd=2)

#14
xdata <- cbind(dbit,dco,dex,gold,sp)
AIC(vars::VAR(xdata,1,type="both"))
AIC(vars::VAR(xdata,2,type="both"))
AIC(vars::VAR(xdata,3,type="both"))# is having lower AIC value hence choosing this one
#15
test <- vars::VAR(xdata,3,type="both")
prd <- predict(test, n.ahead = 30, ci = 0.95, dumvar = NULL)
plot(prd)

