# MSc J. Povtarev
# Berechung der Profilübereinstimmungen
# dazu werden alle geeigneten Ratings aus dem Fragebogen pro Elterpaar (Elter-Kind)
# verglichen. Die Frage ist, wie gut stimmen Elternteil x und das zugehörige Kind x
# in ihren Ratings ingesamt überein? Wie ähnlich ist sich ihr Rating-Profil?

# 1. Auswahl der Variablen für das Profil====================================================
# nicht alle Variablen sind sinnvoll vergleichbar. ZB. können Eltern und Kind nicht
# im Alter übereinstimmen. Die Frage ist nur, ob sie in ihren Meinungen und
# Überzeugungen übereinstimmen, insbesondere solchen 'kultureller' Art.
# alle Fragen können aber nicht verglichen werden, weil sie für Eltern und Kind anders
# formuliert waren.
# --> nur gleichlautende Fragen wurden ausgewählt.

# Kulturtransmissionsmotiv: Wünsche und Emotionen
#   KTW01 ...KTW04, KTH01, KE01...KTE06;KTEBew: Spalten (ESSR) 18:21, 23-29
#head(ESSR[c(18:21, 23:29)])
# (ausgelassen) WKTE und WJTK, weil sich die Fragen für E und K unterscheiden und deshalv
#   nicht direkt vergleichbar sind
# Emotionale und Verhaltensreaktionen auf wahrgenommene Bedrohung der KTransmission
#   BK01...BK06; 44-49 # head(ESSR[44:49])
# Bikulturalität
#   BIK01...BIK05; 50-54 #  head(ESSR[44:49])
# Kulturtransmissionsmotiv: Handlungen
#   KTH01, KTH02, KTH03, KTH05, KTH04; head(ESSR[c(22,55,56,81,82)])
# Bikulturalität 2
#   BIK06...BIK10; 58-62 head(ESSR[58:62])
# Folk psychology and morality concerning cultural transmission
#   FPM01...FPM04;63-66 head(ESSR[63:66])
# Funktionen von Kultur:
#   FU01...FU07; 67-73; head(ESSR[67:73])
# Reaktionen auf möglichn Sprachverlust
#   BS01...BS04;74-77 head(ESSR[74:77])
# Ideale Schule
#  IS01...IS03; 78-80 head(ESSR[78:80])
# (ausgelassen: Schulitems)
# Familiäre Werte
#   W01...W17; 137-153 head(ESSR[137:153])
# ESSR[c(191,5)]
# pairsK und Ecode
ESSRP <- ESSR[c(191,5,18:21, 23:29, 44:49, 50:54,22, 55,56,81,82, 58:62, 63:66,67:73, 74:77, 78:80, 137:153)]
names(KSSR)
# auswahl der selben items für die Kinder. Diese stehen in anderen Spalten des dataframe
KSSRP <- KSSR[c(190,5,19:22, 24:30, 46:51, 53:57,23, 58,59,84,52, 61:65, 66:69,70:76, 77:80, 81:83, 136:152)]
names(KSSRP)                
dim(ESSRP)
dim(KSSRP)
head(ESSRP)
# Test ob die Variablennamen übereinstimmen
cbind(names(ESSR[c(191,5,18:21, 23:29, 44:49, 50:54,22, 55,56,81,82, 58:62, 63:66,67:73, 74:77, 78:80, 137:153)]),
      names(KSSR[c(190,5,19:22, 24:30, 46:51, 53:57,23, 58,59,84,52, 61:65, 66:69,70:76, 77:80, 81:83, 136:152)]))   
# dieselben namen (ausser 2)
names(ESSR[c(191,5,18:21, 23:29, 44:49, 50:54,22, 55,56,81,82, 58:62, 63:66,67:73, 74:77, 78:80, 137:153)]) ==  
  names(KSSR[c(190,5,19:22, 24:30, 46:51, 53:57,23, 58,59,84,52, 61:65, 66:69,70:76, 77:80, 81:83, 136:152)])   
cbind(ESSR[191], KSSR[190])

# Profilmatrix nur die verglichenen Variablen in ESSRPM und KSSRPM (dort Spalte 3-69) werden ausgewählt und in einer
# rein numerischen Matrix gespreichert. Das vereinfacht den Vergleich zeilenweise, wie für Profilvergleich erforderlich.
ESSRPM <-as.matrix(ESSRP[3:69])
KSSRPM <-as.matrix(KSSRP[3:69])
head(ESSRPM)
head(KSSRPM)
cbind(colnames(ESSRPM), colnames(KSSRPM))



# 2. Berechnung der Profilübereinstimmung---------------------------------------------------------------------------------
# Eltern-Kind--------------------------------------------------
# Berechnung mit package irr: Intra-klassen Korrelation
library(irr) # berechnet die 6 Versionen des ICC von Shrout und Fleiss (1979)
help(package=irr)
# Agreement vs. consistency: Consistency berücksichtigt nur Shape (Profil) aber
#             keine Mittelwertsunterschiede, consistency berücksichtigt beides --> Agreement
# "single vs. average": Bei "average": Durchschnitt von 2 oder mehr Ratern wird weiterverwendet als Codierung
#               "single" nur 1 Rater wird weiter verwendet
#              --> hier single, da an eine Weiterverwendung des Durchschnitts E und K nicht gedacht ist
#   Model "oneway" vs. "twoway": twoway --> rater sind random effects, oneway --> rater sind fixed effects,
#             hier ist oneway sinnvoll, a der jeweilige Elternteil und das Kind unvertauschbar sind
# Beispiel für Paar Nr. 1 (Zeile 1 in den 2 Dateien)
icc(cbind(ESSRPM[1,],KSSRPM[1,]), model="oneway", type="agreement", unit = "single")$value    # ICC (1,1) # .4786
icc(cbind(ESSRPM[1,],KSSRPM[1,]), model="oneway", type="consistency", unit = "single")$value  # ICC (3,1) # .4786
# icc(cbind(ESSRPM[1,],KSSRPM[1,]), model="oneway", type="consistency", unit = "average")$value  # ICC (3,2) # .6474
# Zur Kontrolle: mit package multicon
# install.packages("multicon")
library(multicon)
help(package=multicon)
get.ICC(cbind(ESSRPM[1,],KSSRPM[1,]))   # 6 ICCs according to Shrout and Fleiss 1979 #
# teilweise gibt kleine numerische unterschiede zu irr
# Achtung: ICC3 ist immer höher oder gleich ICC1, da
# ICC3 nur die Profilähnlichkeit, nicht aber Höhenunterschiede berücksichtigt == 
#Pearson Korrelation zwischen den
# profilen
#  cor(cbind(ESSRPM[1,],KSSRPM[1,])) # .4767075

# Berechnung de ICCs für alle Personen auf einen Schla, geht mit multicon--------------------------------
ICC_Prof <- Profile.ICC(ESSRPM, KSSRPM)
ICC_Prof <- cbind(Case=1:48, ICC_Prof) # Fallnummer 1..48 (Zeilenno) wurde dazu geben
ESSR$Case <- 1:48 # Fallnummer auch hier dazugeben
KSSR$Case <- 1:48 #
head(ICC_Prof) # relevant sind wieder nur ICC1 und ICC3 == ICC1.1 und ICC3.1
# Mittelwert, SD, Median usw. der ICC über die 48 Paare
psych::describe(ICC_Prof[,c(2, 6)]) # descriptive statistics für ICC1.1 und 3.1
# mittlerer ICC1 = .48 und mittlerer ICC3 = .50
# aber sd ist relativ hoch (.25; .24) ebenso wie der range (-.19 bis .90 und -.06 bis 0.9)
# Verteilung des ICC1 übver die Versuchspersonen
plot(sort(ICC_Prof[,"ICC1"]), ylim=c(-0.3,1))
abline(h=c(0,.20))
# 2 sind negativ, weitere 5 sind unter 0.2
plot(sort(ICC_Prof[,"ICC3"]), ylim=c(-0.3,1))
abline(h=c(0,.20))
# 2 sind negativ, weitere 4 unter .20
# Obwohl Fälle ~ 0 Übereinstimmung nicht unmöglich sind, sind sie eher
# unwahrscheinlich, denn 0 Übereinstimmung würde man erhalten, wenn
# einer oder beide Fragebögen zufällig ausgefüllt werden würden.
# Es ist möglich, dass hier eine Person den Frabo nicht ersthaft
# ausgefüllt hat oder dass ein Fehler bei der Zuordnung Kind-Elternteil
# erfolgte
# Fälle mit niedrigsten Übereinstimmungem
ICC_Prof[ICC_Prof[,"ICC1"]< .20,] # Cases 25, 29, 32, 33, 34, 41, 45

# Funktion zum visuellen Vergleich der Profile pro Par (einmal einlesen, danach kann sie verwendet werden)
tcompEK <- function(Elter, Kind) {
  E <- ESSRPM[Elter,];   K <- KSSRPM[Kind,]
  par(mfrow=c(1,3))
  plot(jitter(E), jitter(K), pch=16, xlab = "Elter", ylab="Kind", xlim=c(1,7), ylim=c(1,7),
       main = paste(
         #round(ICC_Prof_SF[Elter,1],2)," ", round(ICC_Prof_SF[Elter,5],2)
         "ICC3 = ", round(get.ICC(cbind(E,K))[,"ICC3"],2)))
  abline(a=0, b=1)
  EK <- cbind(E,K)
  # EK<- EK[order(EK[,1]),]
  #plot(EK[,1],1:67, type = "l", yaxt="n", xlab="Rating")
  #axis(2,1:67, labels = colnames(ESSRPM), las=2, cex.axis=1); grid()
  #plot(EK[,2],1:67, type = "l", yaxt="n", xlab="Rating")
  #axis(2,1:67, labels = colnames(ESSRPM), las=2, cex.axis=1); grid()
  # lines(EK[,2], 1:67, type = "l",col="darkred")
  
  #  Backtoback Barplot
  barplot(-E, col="grey", horiz=TRUE, space=0, add=FALSE, xlim = c(-7.5,7.5), las=2,
          main=paste("E", Elter, "--K",Kind," ",sep=""), cex.names=.9)
  barplot(K, col="darkred", horiz=TRUE, space=0,add=TRUE, axes=FALSE, xlim=c(0,7.5), yaxt='n')
  axis(1, 0, line=1.5, labels="Eltern - Kinder")
  barplot(E-K, las=2, horiz=TRUE, xlim=c(-9,9),cex.names=.9,
          main = paste("ICC1 = ", round(get.ICC(cbind(E,K))[,"ICC1"],2)))
}

#Beispiel:
  tcompEK(1,1) # Eltern 1, Kind 1
tcompEK(2,2) # Eltern 2, Kind 2
tcompEK(3,3)
tcompEK(4,4)
for (i in 1:10) {tcompEK(i,i); readline()} # im Konsolenfenster weiterklicken, damit alle plots nacheinander gezeigt werden
for (i in 11:20) {tcompEK(i,i); readline()}
for (i in 21:30) {tcompEK(i,i); readline()}
for (i in 31:40) {tcompEK(i,i); readline()}
for (i in 41:48) {tcompEK(i,i); readline()}


# Alternativer Profilvergleich mit Liniengrafik
pcompEK <- function(Elter, Kind)
{
  E <- ESSRPM[Elter,];   K <- KSSRPM[Kind,]
  par(mfrow=c(1,2))
  plot(E, 1:67, type="l", ylab="", yaxt="n",
       main =paste("E",Elter, "K", Kind, " r = ",round(cor(E,K),2),sep=""))
  lines(K, 1:67, col="red")   
  axis(2, 1:67, labels=names(E),las=2, cex=.7)
}     
pcompEK(1,1)
# Fälle mit niedrigen Übereinstimmungen:
# Cases 25, 29, 32, 33, 34, 41, 45
tcompEK(25,25)
pcompEK(25,25)
tcompEK(29,29) # <- sehr geringe Ratingvarianz bei Kinder
tcompEK(32,32)
tcompEK(33,33)
tcompEK(34,34) # <- sehr geringe Ratingvarianz bei Kindern
tcompEK(41,41) #
tcompEK(45,45)
# Berechnung der Ratingvarianz
ESSR$SD <- apply(ESSRPM,1,sd)
KSSR$SD <- apply(KSSRPM,1,sd)
cor(ESSR$SD, ICC_Prof[,"ICC1"]) # korrelation EK-Übereinstimmung und ratingvarianz --> schwach positive korrelation
cor(KSSR$SD, ICC_Prof[,"ICC1"]) # korrelation EK-Übereinstimmung und ratingvarianz --> schwach positive korrelation



# deskriptive Auswertung
library("ggplot2")
library("xlsx")
source("PLOT_EKcompare_Kopie.r") 
library("DescTools")

AuswertenZahl<-function(Data){
  # Daten mit Zahlen auswerten
  Data<-as.numeric(Data)
  write.xlsx(sort(table(Data)), file="Data.xlsx")
  sort(table(Data))
}

AuswertenDaten<-function(Data){
  # Daten mit Eingabe auswerten
  write.xlsx(sort(table(Data)), file="Data.xlsx")
  sort(table(Data))
}

DF_Alter<-c(ESSR$EGeburtsjahr)
DF_Alter<-as.numeric(DF_Alter)
DF_Alter<-replace(DF_Alter, DF_Alter==1073, 1973)
DF_Alter<-2021-DF_Alter
table(DF_Alter)
qqnorm(DF_Alter)
qqline(DF_Alter)
shapiro.test(DF_Alter)
mean(2021-DF_Alter)
sd(2021-DF_Alter)

E_Geschlecht<-c(ESSR$EGeschlecht)
E_Geschlecht<-as.numeric(E_Geschlecht)
table(E_Geschlecht)
PercTable(E_Geschlecht)

E_Geburtsland<-c(ESSR$EGeburtsland)
#E_Geburtsland<-as.numeric(E_Geburtsland)
table(E_Geburtsland)


# Aufenthalt
AuswertenZahl(ESSR$EAufenthalt)
mean(as.numeric(ESSR$EAufenthalt))

#geburtsland partner
AuswertenDaten(ESSR$EGeburtslandPartner)

#aufenthalt partner
AuswertenZahl(ESSR$EAufenthaltPartner)
aufenthalt_partner<-c(as.numeric(ESSR$EAufenthaltPartner))
mean(as.numeric(ESSR$EAufenthaltPartner))
                  
# religion
AuswertenZahl(ESSR$EReligion)
PercTable(ESSR$EReligion)


#Bildung
AuswertenZahl(ESSR$EBildung)
PercTable(ESSR$EBildung)
#Beruf
AuswertenDaten(ESSR$EBeruf)


#Kinderzahl
AuswertenZahl(ESSR$EKinderzahl)
PercTable(ESSR$EKinderzahl)

'Deskriptive Statistik Kinder'

AuswertenZahl(2021-KSSR$KGeburtsjahr)
qqnorm(2021-KSSR$KGeburtsjahr)
qqline(2021-KSSR$KGeburtsjahr)
shapiro.test(2021-KSSR$KGeburtsjahr)
mean(2021-KSSR$KGeburtsjahr)

AuswertenZahl(KSSR$KGeschlecht)

AuswertenDaten(KSSR$KGeburtsland)

AuswertenZahl(KSSR$KAufenthalt)
mean(as.numeric(KSSR$KAufenthalt),na.rm=TRUE)

AuswertenZahl(KSSR$KReligion)

AuswertenZahl(KSSR$KBildung)

' Frage 1'
table(ESSR$KTW01,useNA="always") # Hдufigkeitstabelle fьr Eltern KTW01
table(KSSR$KTW01,useNA="always") # HT fьr Kinder KTW01
table(ESSR$KTW01,KSSR$KTW01) # bivariate Tabelle
compareHist("KTW01", "KTW01")
compareEK("KTW01", "KTW01") # <- das ist die wichtigste plotfunktion
