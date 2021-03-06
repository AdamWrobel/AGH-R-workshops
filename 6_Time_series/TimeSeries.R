
#Time series 
#Ladowanie potrzebnych bibliotek 

library(dplyr)
library(ggplot2)

### Pakiet do analizy kointegracji i diagnostyki szeregow czasowych 
install.packages('urca')
library(urca)

###Pakiet do diagnostyki seasonal unit roots (stacjonarnosc szeregow)
install.packages('uroot')
library(uroot)

####Pakiet przydatny do konstukcji modeli ARIMA oraz do prognozowania i diagnostyki prognoz 
install.packages("forecast")
library(forecast)

###Pakiet do modeli VAR (vector autoregressive)
install.packages('vars')
library(vars)



###Sciagniacie danych potrzebnych do analizy  - Wskaznik cen konsumenta HICP z Eurostatu dla Polski, Niemec i Francji
CPI_PL_WE <- read.csv2("https://raw.githubusercontent.com/AdamWrobel/AGH-R-workshops/master/6_Time_series/HICP_PL_WE.csv",sep=",")

####### Ogladamy dane 
head(CPI_PL_WE)

####Ucinamy pierwsza kolumne
CPI_PL_WE<- CPI_PL_WE[,-1]

####Uporzadkowanie Dat - w taki spos�b aby na poczatku byly najwczesniejsze obserwacje

#Pierwszy punkt to przeksztlcenie zmiennej "time" 
#format "Date" wg schematu - year -month- day 
CPI_PL_WE$time <- as.Date(CPI_PL_WE$time)

#Drugi krok to sortowanie danych po czasie, a nastepnie po 
#zmiennej "geo" okreslajacych ksztalt 
CPI_PL_WE <- CPI_PL_WE %>% arrange(time) %>% arrange(geo)

#Przeksztalcenie values - ze zmiennej factor na numeryczne:
#Uwaga - tu trzeba factor przeksztalcic na character i dopiero potem na numeric
CPI_PL_WE<- CPI_PL_WE %>% mutate(value = as.numeric(as.character(values))) %>%
  select(-values)

#Utworzenie zmiennych pomocnicznych okreslajacych poczatek i koniec szeregu czasowego 
MinMaxDate <- CPI_PL_WE %>% group_by(geo)%>% 
  summarize(Begin = min(time), Ends = max(time))

#Utworzenie zmiennych pomocniczych pomocnych przy tworzeniu szereg�w czasowych
MinMaxDate2 <- MinMaxDate %>% group_by(geo)%>%
  mutate(YB = as.numeric(format(Begin, "%Y")),
         MB = as.numeric(format(Begin, "%m")),
         YE = as.numeric(format(Ends, "%Y")),
         ME = as.numeric(format(Ends, "%m"))
         )



##Zlaczenie zbior�w MinMAxDate2 i CPI_PL_WE 
## tak aby latwo utworzyc zmienne klasy time series  

CPI_PL_WE2 <- full_join(CPI_PL_WE, MinMaxDate2)

#Joining jest dokonywane przez  by = "geo"

head(CPI_PL_WE2)

####Wykres liniowy szereg�w czasowych
ggplot(CPI_PL_WE2, aes(x=time,y=value, color=geo))+geom_line(lwd = 1.2) +
  xlab("lata")+ylab("2005=100") +
  ggtitle("Sharmonizowany indeks cen konsumenta (HICP) \n dla Polski, Niemiec i Francji") 

# Zadanie nr 1 - > Przedstaw na wykresie dane od roku 2000,  
# przy przeksztalceniu indeksu jednopodstawowego, tak ze w 
# styczniu 2000 indeks ma wartosc100  
# Tu jest miejsce na Twoje rozwiazanie !
#
#
#
#
#
#
#
#
#
#
#Tu jest jedno z proponowanych rozwiazan:    

CPI_PL_WE3 <-CPI_PL_WE2 %>% filter(time >="2000-01-01")

Values2000 <- CPI_PL_WE %>% filter(time=="2000-01-01") %>% select(value, geo) %>% rename(value2000= value)

CPI_PL_WE4 <- full_join(CPI_PL_WE3, Values2000)%>% select(-c(YB, MB, Begin)) %>% mutate(NewIndex = (value/value2000)*100)
  
ggplot(CPI_PL_WE4, aes(x=time,y=NewIndex, color=geo))+geom_line() +xlab("lata")+ylab("01.2000=100") +ggtitle("Sharmonizowany indeks cen konsumenta (HICP) \n dla Polski, Niemiec i Francji") 












#########Wyciaganie indeksu CPI dla Polski - tak aby  

CPI_PL <- CPI_PL_WE4 %>% filter(geo == "PL")

MM_PL <- MinMaxDate2 %>% filter(geo =="PL")%>% select(-c(YB,MB))

#### Tworzenie wektor�w szereg�w czasowych, z wykorzystaniem funkcji TS 

CPI_P = ts(CPI_PL[,"NewIndex"],start=c(2000,1), end=c(as.numeric(MM_PL["YE"]), as.numeric(MM_PL["ME"])),frequency=12)

#plot(CPI_P)

##Logarytm - czesto stosowane w badaniach empirycznych (stabilizacja wariancji w czasie - nie powinno sie stosowac do zmiennych kt�rych wartosci moga byc ujemne)
lnCPI_P = log(CPI_P)
## Pierwsze r�znice logarytm�w
dlnCPI_P = diff(lnCPI_P,lag=1)

plot(dlnCPI_P)

#Kr�tkie zadanie - stw�rz szereg przyrost�w logarytm�w r/r 
#
#
#
#
#
#
#
#
#
#Proponowane rozwiazanie: 
sdlnCPI_P = diff(lnCPI_P, lag=12)

###WProste wykres
plot(CPI_P)
plot(lnCPI_P)
plot(dlnCPI_P)
plot(sdlnCPI_P)

### Analiza wlasciwosci szereg�w    
#Analiza stacjonarnosci test ADF, wariant ze stala i metoda selekcji op�znienie - kryterium akaike
#domyslnie liczba lags ustalona przez uzytkownika
#Tutaj wybieramy liczbe op�znien za pmoca kryterium AIC
# h0 - bladzenie losowe -> niestacjonarny
ADF_CPI <- ur.df(CPI_P, type="drift",lags=18,selectlags="AIC")
ADF_lCPI <- ur.df(lnCPI_P, type="drift", lags =18,selectlags="AIC")
ADF_dlCPI <- ur.df(dlnCPI_P, type="drift",lags= 18,selectlags="AIC")
ADF_sdlCPI <- ur.df(sdlnCPI_P, type="drift",lags= 18,selectlags="AIC")



summary(ADF_CPI)
summary(ADF_lCPI)
summary(ADF_dlCPI)
summary(ADF_sdlCPI)


#Przy interpretacj wynik�w nalezy pamietac,  o tym, ze test ADF 
#faworyzuje hipoteze zerowa o wystepowaniu pierwiastka jednostkowego 

#Test KPSS - wariant ze stala (jest jeszcze mozliwy ze srednia)
# h0 - stacjonarnosc
KPSS_CPI <- ur.kpss(CPI_P, type="mu",lags="short")
KPSS_lCPI <- ur.kpss(lnCPI_P, type="mu",lags="short")
KPSS_dlCPI <- ur.kpss(dlnCPI_P, type="mu",lags="short")
KPSS_sdlCPI <- ur.kpss(sdlnCPI_P, type="mu",lags="short")


summary(KPSS_CPI)
summary(KPSS_lCPI)
summary(KPSS_dlCPI)
summary(KPSS_sdlCPI)



###Wyniki moga byc zaburzone przez potencjalnie wystapujaca sezonowosc
#Dlatego tez warto przeprowadzic testy pierwiastka jednostkowego uwzgledniajace to
# np. Canova and Hansen (CH) test

#ch.test(x, type = c("dummy", "trigonometric"), lag1 = FALSE, NW.order = NULL,
#sid = NULL, xreg = NULL, pvalue = c("RS", "raw"), rs.nobsreg = 13)

#"dummy" for seasonal
#dummies or "trigonometric" for seasonal cycles

#an optional numeric vector, the target seasonal dummies or cycles to be tested.
#By default all the individual and joint test statistics are returned.

#Tu testyjemy sezonowosc miesieczna 
# h0 - proces stacjonarny
ch.test(dlnCPI_P, type = "dummy", sid = 12)
ch.test(lnCPI_P, type = "dummy", sid = 12)



#funkcja autokrelacji i czastkowej autokorelacji

acf(CPI_P)
pacf(CPI_P)

acf(lnCPI_P)
acf(dlnCPI_P)
acf(sdlnCPI_P)
pacf(dlnCPI_P)

pacf(sdlnCPI_P)

### Co mozemy powiedziec o szeregu ? 
#Jak podejsc do jego modelowania - pytanie otwarte do grypy 




#Model AR(1) / ARIMA(1,0,0)
AR1 <- arima(lnCPI_P, order=c(1,0,0))
AR1 %>% forecast %>% plot

#Model ARIMA(1,1,0)
ARIMA110 <- arima(lnCPI_P, order=c(1,1,0),include.mean = TRUE)
ARIMA110 %>% forecast %>% plot

#Model ARIMA(1,0,0) dla przyrost�w i sezonowych przyrost�w
AR1_d <- arima(dlnCPI_P, order=c(1,0,0))
AR1_d %>% forecast %>% plot
AR1_s <- arima(sdlnCPI_P, order=c(1,0,0))
AR1_s %>% forecast %>% plot

#Model z sezonowymi komponentatmi (SARIMA(1,0,0), (1,0,0))
AR1_sesonal <- arima(dlnCPI_P, order=c(1,0,0),seasonal = list(order = c(1, 0, 0)))
AR1_sesonal %>% forecast %>% plot

Arima1_dCPI <- arima(dlnCPI_P, order=c(1,0,0))
 
plot(Arima1_dCPI$residuals)
pacf(Arima1_dCPI$residuals)


#
#
###Automayczna selekcja modeli - wykorzystanie funkcji auto.arima z pakietu forecast 
#auto.arima(y, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2,
#max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
#start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
#seasonal = TRUE, ic = c("aicc", "aic", "bic"), stepwise = TRUE,
#trace = FALSE, approximation = (length(x) > 150 | frequency(x) > 12),
#truncate = NULL, xreg = NULL, test = c("kpss", "adf", "pp"),
#seasonal.test = c("seas", "ocsb", "hegy", "ch"), allowdrift = TRUE,
#allowmean = TRUE, lambda = NULL, biasadj = FALSE, parallel = FALSE,
#num.cores = 2, x = y, ...)
#Arguments
#
#

AutoArima <- auto.arima(dlnCPI_P)
AutoArima %>% forecast %>% plot


#Zadanie
#A) Stworz wykres poziomow CPI dla modeli AR1_sesonal i AutoArima
#


#
#Wielowymiarowa analiza szereg�w czasowych
#
#
#Zadanie przypominajace - stworzenie szereg�w ts dla CPI, lnCPI,  dlnCPI i sdlnCPI dla Niemiec i Francji 
#Tu jest miejsce na Twoje rozwiazanie: 
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

###Proponowane rozwiazane
###Dane_dlaNiemiec

CPI_DE <- CPI_PL_WE4 %>% filter(geo == "DE")
MM_DE <- MinMaxDate2 %>% filter(geo =="DE") %>% select(-c(YB,MB))

CPI_D = ts(CPI_DE[,"NewIndex"],start=c(2000,1), end=c(as.numeric(MM_DE["YE"]), as.numeric(MM_DE["ME"])),frequency=12)
lnCPI_D = log(CPI_D)
dlnCPI_D = diff(lnCPI_D,lag=1)
sdlnCPI_D = diff(lnCPI_D, lag=12)

###Dane

CPI_FR <- CPI_PL_WE4 %>% filter(geo == "FR")
MM_FR <- MinMaxDate2 %>% filter(geo =="FR") %>% select(-c(YB,MB))

CPI_F = ts(CPI_FR[,"NewIndex"],start=c(2000,1), end=c(as.numeric(MM_FR["YE"]), as.numeric(MM_FR["ME"])),frequency=12)
lnCPI_F = log(CPI_F)
dlnCPI_F = diff(lnCPI_F,lag=1)

sdlnCPI_F = diff(lnCPI_F, lag=12)


######Przygotowanie Zbioru do modelu 'VAR

SdCPI <- cbind(sdlnCPI_D,sdlnCPI_F, sdlnCPI_P)

dCPI <- cbind(dlnCPI_D,dlnCPI_F, dlnCPI_P)



#Model VAR (vector autoregressive) - z wykorzystaniem pakietu vars  
#
#VAR(y, p = 1, type = c("const", "trend", "both", "none"),
  #  season = NULL, exogen = NULL, lag.max = NULL,
   # ic = c("AIC", "HQ", "SC", "FPE"))
#
#

VARdCPI <- VAR(dCPI, p=12, type = "const", season=12, lag.max=18, ic="AIC")
summary(VARdCPI)
plot(VARdCPI)

VARSdCPI <- VAR(SdCPI,  type = "const",lag.max=12, ic="AIC")
summary(VARSdCPI)
plot(VARSdCPI)

######Funkcje reakcji na impulse (impulseresponsefunctions)
####
irfdCPI <- irf(VARdCPI, n.ahead=10, runs=1000)
plot(irfdCPI)


irfsdCPI <- irf(VARSdCPI, n.ahead=40, runs=1000)
plot(irfsdCPI)


##########Prognozowanie z  modelu VAR.     
#predict(object, ..., n.ahead = 10, ci = 0.95, dumvar = NULL)

PreddCPI  <- predict(VARdCPI , n.ahead = 24)
PredsdCPI  <- predict(VARSdCPI , n.ahead = 24)
###Wizualizacja: Time Series plots of VAR forecasts with differently shaded confidence regions (fanchart) for each endogenous variable.


#fanchart(x, colors = NULL, cis = NULL, names = NULL, main = NULL, ylab =
 #          NULL, xlab = NULL, col.y = NULL, nc, plot.type = c("multiple",
  #                                                            "single"), mar = par("mar"), oma = par("oma"), ... )

plot(PreddCPI)

plot(PredsdCPI)

fanchart(PreddCPI)

fanchart(PredsdCPI)



#Pakiet do bezposredniego sciagania danych z Eurostatu  - Dane zostaly sciagniete za posrednictwem tego pakiety 
#library(eurostat)

###Kod do bezposredniego sciagniecia danych z Eurostatu:  
#Nie bedzie wykonywany na zajeciach (chyba, ze na sam koniec by pokazac jak to fukcjonuje)
###Sharmonizowany indeks cen konsumenta (HICP)
#Wykaz wszystkich baz danych Eurostatu w kt�re maja w swojej nazwie HCIP 
#
#HICP = search_eurostat(pattern="HICP")
#
#Sciagniecie calej bazy danych dla HICP
#
#HICP_all <- get_eurostat(as.character("prc_hicp_midx"))
#
#
#####Selekcja jedynie indeks�w dla wszystkich d�br (coicop CP00) dla 2005=100 (unit "I05")
#Generalnie po kodach COCIOP mozna wybrac indeksy dla bardziej szczeg�lowych kategorii d�br. 
#
#HICP_total <- HICP_all %>% dplyr::filter(coicop =="CP00"& unit == "I05")
#
### Wyb�r indeksu HICP dla wszystkich d�rb jedynie dla Polski
#
#HICP_PL <- HICP_total%>% filter(geo =="PL")
#
#Wyb�r indeksu HICP dla Polski, Niemiec i Francji 
#HICP_PL_WE <- HICP_total%>% dplyr::filter(geo =="PL" | geo == "DE"|geo == "FR")
#
#
#write.csv(HICP_PL_WE, "HICP_PL_WE.csv")

