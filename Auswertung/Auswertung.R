library("readxl")
library("ggplot2")
library(dplyr)
library(tidyr)
library(scales)
library("DescTools")


my_data <- readxl::read_excel("Umfrage_Kopie.xlsx",col_names = TRUE)

my_dataframe <- data.frame(my_data, check.names = TRUE)

# Nicht leere Angaben
my_dataframe <- subset(my_dataframe, Kind.1...7!='')


my_data_bayern <- subset(my_dataframe, X26.Bundesland=='Bayern')

Alter<-table(my_data_bayern$X28.Alter)


# deskriptive Auswertung

DF_Homeoffice<-data.frame(my_data_bayern$X1.Homeoffice)
table(DF_Homeoffice)
PercTable(DF_Homeoffice)


DF_Geschlecht <-data.frame(my_data_bayern$X27.Geschlecht)
table(DF_Geschlecht)
PercTable(DF_Geschlecht)

DF_Alter <-data.frame(my_data_bayern$X28.Alter)
table(DF_Alter)
PercTable(DF_Alter)

DF_Bildung <-data.frame(my_data_bayern$X29.Bildungsabschluss)
table(DF_Bildung)
PercTable(DF_Bildung)

DF_Familie<-data.frame(my_data_bayern$X30.Familiensituation)
table(DF_Familie)
PercTable(DF_Familie)

DF_Verhaeltnis<-data.frame(my_data_bayern$X31.VerhaeltnisZumKind)
table(DF_Verhaeltnis)
PercTable(DF_Verhaeltnis)

DF_AnzahlKinder<-data.frame(my_data_bayern$X3.AnzahlKinder)
table(DF_AnzahlKinder)
PercTable(DF_AnzahlKinder)

DF_AlterKind1<-data.frame(my_data_bayern$Kind.1...7)
table(DF_AlterKind1)
PercTable(DF_AlterKind1)

DF_AlterKind2<-data.frame(my_data_bayern$Kind.2)
table(DF_AlterKind2)
PercTable(DF_AlterKind2)

# Auwertung der Fragen

DF_StundenTag<-data.frame(my_data_bayern$X5.StundenUnterrichtstag)
DF_StundenTag[DF_StundenTag == 44228] <- "1-2"
DF_StundenTag[DF_StundenTag == 44257] <- "2-3"
DF_StundenTag[DF_StundenTag == 44289] <- "3-4"
DF_StundenTag[DF_StundenTag == 44320] <- "4-5"
table(DF_StundenTag)
PercTable(DF_StundenTag)


DF_ZeitKinder<-data.frame(my_data_bayern$X6.ZeitKinder)
table(DF_ZeitKinder)
PercTable(DF_ZeitKinder)

DF_KindMail<-data.frame(my_data_bayern$X8.PerMail)
table(DF_KindMail)
PercTable(DF_KindMail)

DF_KindVideo<-data.frame(my_data_bayern$X8.PerVideochat)
table(DF_KindVideo)
PercTable(DF_KindVideo)

DF_Telefon<-data.frame(my_data_bayern$X8.PerTelefon)
table(DF_Telefon)
PercTable(DF_Telefon)


DF_ElternMail<-data.frame(my_data_bayern$X9.ElternPerMail)
table(DF_ElternMail)
PercTable(DF_ElternMail)

DF_ElternVideo<-data.frame(my_data_bayern$X9.ElternPerVideochat)
table(DF_ElternVideo)
PercTable(DF_ElternVideo)

DF_ElternTelefon<-data.frame(my_data_bayern$X9.ElternPerTelefon)
table(DF_ElternTelefon)
PercTable(DF_ElternTelefon)


DF_KorrekturLehrer<-data.frame(my_data_bayern$X10.KorrekturLehrer)
table(DF_KorrekturLehrer)
PercTable(DF_KorrekturLehrer)

DF_WochenplanLehrkraft<-data.frame(my_data_bayern$X11.WochenplanLehrkraft)
table(DF_WochenplanLehrkraft)
PercTable(DF_WochenplanLehrkraft)

DF_AustauschKind<-data.frame(my_data_bayern$X12.AustauschKind)
table(DF_AustauschKind)
PercTable(DF_AustauschKind)

DF_ErreichbarkeitLehrkraft<-data.frame(my_data_bayern$X13.ErreichbarkeitLehrkraft)
table(DF_ErreichbarkeitLehrkraft)
PercTable(DF_ErreichbarkeitLehrkraft)

DF_HilfeFuerKinder<-data.frame(my_data_bayern$X14.HilfeFuerKinder)
table(DF_HilfeFuerKinder)
PercTable(DF_HilfeFuerKinder)

DF_MotivationKind<-data.frame(my_data_bayern$X15.MotivationKind)
table(DF_MotivationKind)
PercTable(DF_MotivationKind)

DF_MotivationKindPlanung<-data.frame(my_data_bayern$X15.Planung)
table(DF_MotivationKindPlanung)
PercTable(DF_MotivationKindPlanung)

DF_MotivationKindTechnik<-data.frame(my_data_bayern$X15.Technik)
table(DF_MotivationKindTechnik)
PercTable(DF_MotivationKindTechnik)

DF_MotivationKindVorlesen<-data.frame(my_data_bayern$X15.Vorlesen)
table(DF_MotivationKindVorlesen)
PercTable(DF_MotivationKindVorlesen)

DF_MotivationKindAndere<-data.frame(my_data_bayern$X16.MotivationKind)
table(DF_MotivationKindAndere)
PercTable(DF_MotivationKindAndere)

DF_PlanenAndere<-data.frame(my_data_bayern$X16.Planen)
table(DF_PlanenAndere)
PercTable(DF_PlanenAndere)

DF_TechnikAndere<-data.frame(my_data_bayern$X16.Technik)
table(DF_TechnikAndere)
PercTable(DF_TechnikAndere)

DF_VorlesenAndere<-data.frame(my_data_bayern$X16.Vorlesen)
table(DF_VorlesenAndere)
PercTable(DF_VorlesenAndere)

DF_Streit<-data.frame(my_data_bayern$X17.Streit)
table(DF_Streit)
PercTable(DF_Streit)

DF_GerneSchule<-data.frame(my_data_bayern$X18.GerneSchule)
table(DF_GerneSchule)
PercTable(DF_GerneSchule)

DF_Alleine<-data.frame(my_data_bayern$X18.Alleine)
table(DF_Alleine)
PercTable(DF_Alleine)

DF_StreitLernen<-data.frame(my_data_bayern$X18.StreitLernen)
table(DF_StreitLernen)
PercTable(DF_StreitLernen)

DF_Selbstorganisation<-data.frame(my_data_bayern$X18.Selbstorganisation)
table(DF_Selbstorganisation)
PercTable(DF_Selbstorganisation)

DF_MotivationKindVorLockdown<-data.frame(my_data_bayern$X19.MotivationKind)
table(DF_MotivationKindVorLockdown)
PercTable(DF_MotivationKindVorLockdown)

DF_PlanungVorLockdown<-data.frame(my_data_bayern$X19.Planung)
table(DF_PlanungVorLockdown)
PercTable(DF_PlanungVorLockdown)

DF_VorlesenVorLockdown<-data.frame(my_data_bayern$X19.Vorlesen)
table(DF_VorlesenVorLockdown)
PercTable(DF_VorlesenVorLockdown)

DF_MotivationKindStress<-data.frame(my_data_bayern$X20.Motiavtion)
table(DF_MotivationKindStress)
PercTable(DF_MotivationKindStress)

DF_PlanungStress<-data.frame(my_data_bayern$X20.Planung)
table(DF_PlanungStress)
PercTable(DF_PlanungStress)

DF_TechnikStress<-data.frame(my_data_bayern$X20.Planung)
table(DF_TechnikStress)
PercTable(DF_TechnikStress)

DF_VerstaendnisStress<-data.frame(my_data_bayern$X20.Planung)
table(DF_VerstaendnisStress)
PercTable(DF_VerstaendnisStress)
          
          
DF_MotivationVerlust<-data.frame(my_data_bayern$X21.Motivation)
table(DF_MotivationVerlust)
PercTable(DF_MotivationVerlust)

DF_Luecke<-data.frame(my_data_bayern$X21.Wissenslücken)
table(DF_Luecke)
PercTable(DF_Luecke)

DF_Soziale<-data.frame(my_data_bayern$X21.SozialeKompetenzen)
table(DF_Soziale)
PercTable(DF_Soziale)

DF_AlleineLernen<-data.frame(my_data_bayern$X22.AlleineLernen)
table(DF_AlleineLernen)
PercTable(DF_AlleineLernen)

DF_SchwierigkeitEltern<-data.frame(my_data_bayern$X22.SchwierigkeitEltern)
table(DF_SchwierigkeitEltern)
PercTable(DF_SchwierigkeitEltern)

DF_Einengung<-data.frame(my_data_bayern$X22.Einengung)
table(DF_Einengung)
PercTable(DF_Einengung)

DF_RueckmeldungLehrkraft<-data.frame(my_data_bayern$X23.RueckmeldungLehrkraft)
table(DF_RueckmeldungLehrkraft)
PercTable(DF_RueckmeldungLehrkraft)

DF_Strukturhilfe<-data.frame(my_data_bayern$X23.Strukturhilfe)
table(DF_Strukturhilfe)
PercTable(DF_Strukturhilfe)

DF_Abgabetermine<-data.frame(my_data_bayern$X23.Abgabetermine)
table(DF_Abgabetermine)
PercTable(DF_Abgabetermine)


DF_Feedback<-data.frame(my_data_bayern$X24.Feedback)
table(DF_Feedback)
PercTable(DF_Feedback)

DF_IndividuellesFeedback<-data.frame(my_data_bayern$X24.IndividuellesFeedback)
table(DF_IndividuellesFeedback)
PercTable(DF_IndividuellesFeedback)

DF_GutLernen<-data.frame(my_data_bayern$X25.GutLernen)
table(DF_GutLernen)
PercTable(DF_GutLernen)
# homogenität

library(psych)
#22
alpha(my_data_bayern[c("X22.AlleineLernen","X22.SchwierigkeitEltern","X22.Einengung")], check.keys=TRUE)
#21
alpha(my_data_bayern[c("X21.Wissenslücken","X21.SozialeKompetenzen","X21.Motivation")], check.keys=TRUE)
#20
alpha(my_data_bayern[c("X20.Motiavtion", "X20.Verstaendnis","X20.TechnischeProbleme","X20.Planung")], check.keys=TRUE)

## New Variables
#20
DF_20<-data.frame((my_data_bayern$X20.Motiavtion+my_data_bayern$X20.Planung+my_data_bayern$X20.TechnischeProbleme+
                    my_data_bayern$X20.Verstaendnis)/4)
#21
DF_21<-data.frame((my_data_bayern$X21.Motivation+my_data_bayern$X21.SozialeKompetenzen+my_data_bayern$X21.Wissenslücken)/3)

#22
DF_22<-data.frame((my_data_bayern$X22.AlleineLernen+my_data_bayern$X22.Einengung+my_data_bayern$X22.SchwierigkeitEltern)/3)


wilcox.test(DF_20$X.my_data_bayern.X20.Motiavtion...my_data_bayern.X20.Planung.., y = NULL,
            alternative = "greater",
            mu = 2.5, paired = FALSE)

wilcox.test(DF_22$X.my_data_bayern.X22.AlleineLernen...my_data_bayern.X22.Einengung..., y = NULL,
            alternative = "greater",
            mu = 2.5, paired = FALSE)

wilcox.test(DF_21$X.my_data_bayern.X21.Motivation...my_data_bayern.X21.SozialeKompetenzen..., y = NULL,
            alternative = "greater",
            mu = 2.5, paired = FALSE)

## Korrelationen 
library("writexl")
library("correlation")
library("tidyverse")
library("corrr")
# fpr die zusammengefassten Variablen 20,21,22
Bayern_Cor20 <- readxl::read_excel("Bayern_Correlation.xlsx",sheet="DF20",col_names = TRUE)
Bayern_Cor21 <- readxl::read_excel("Bayern_Correlation.xlsx",sheet="DF21",col_names = TRUE)
Bayern_Cor22 <- readxl::read_excel("Bayern_Correlation.xlsx",sheet="DF22",col_names = TRUE)
# spearmancorrelation
Bay_Cor20<-correlate(Bayern_Cor20, method = "spearman", diagonal = 1)
Bay_Cor21<-correlate(Bayern_Cor21, method = "spearman", diagonal = 1)
Bay_Cor22<-correlate(Bayern_Cor22, method = "spearman", diagonal = 1)

Bay_Cor20<-Bay_Cor20 %>% shave()
Bay_Cor21<-Bay_Cor21 %>% shave()
Bay_Cor22<-Bay_Cor22 %>% shave()

## Zusammen
Item_18<-c(Bayern_Cor20$X18.Alleine+Bayern_Cor20$X18.Selbstorganisation)/2
Bayern_Cor20$Item_18 <- Item_18
Bayern_Cor21$Item_18 <- Item_18
Bayern_Cor22$Item_18 <- Item_18

#Korrelationsmatrizen in Excel vorerst
write_xlsx(Bay_Cor20,"Bay_Cor20.xlsx")
write_xlsx(Bay_Cor21,"Bay_Cor21.xlsx")
write_xlsx(Bay_Cor22,"Bay_Cor22.xlsx")

# Einteilen in mehr Stress und weniger bzw. keinen
Frage_20_Zufrieden=subset(Bayern_Cor20, DF20<2.5)
Frage_20_Unzufrieden=subset(Bayern_Cor20, DF20>=2.5)

Frage_20_Zufrieden[is.na(Frage_20_Zufrieden)] <- 0
Frage_20_Unzufrieden[is.na(Frage_20_Unzufrieden)] <- 0

# independent 2-group Mann-Whitney U Test
#keine annahmen an normalität und rangskaliert

# Vergleiche für Hypothesentests

# hypothese anzahl der kinder fuehrt zu mehr unzufriedenheit
wilcox.test(Frage_20_Unzufrieden$X3.AnzahlKinder,Frage_20_Zufrieden$X3.AnzahlKinder)
mean(Frage_20_Zufrieden$X3.AnzahlKinder)
mean(Frage_20_Unzufrieden$X3.AnzahlKinder)

Frage_21_Zufrieden=subset(Bayern_Cor21, DF21<2.5)
Frage_21_Unzufrieden=subset(Bayern_Cor21, DF21>=2.5)

Frage_21_Zufrieden[is.na(Frage_21_Zufrieden)] <- 0
Frage_21_Unzufrieden[is.na(Frage_21_Unzufrieden)] <- 0

Wilcox_Test <- function(x,y) {
  
  print(wilcox.test(x,y))
  print(mean(x))
  print(mean(y))
}

wilcox.test(Frage_21_Unzufrieden$X3.AnzahlKinder,Frage_21_Zufrieden$X3.AnzahlKinder)
mean(Frage_21_Zufrieden$X3.AnzahlKinder)
mean(Frage_21_Unzufrieden$X3.AnzahlKinder)

Wilcox_Test(Frage_21_Unzufrieden$X3.AnzahlKinder,Frage_21_Zufrieden$X3.AnzahlKinder)

Frage_22_Zufrieden=subset(Bayern_Cor22, DF22<2.5)
Frage_22_Unzufrieden=subset(Bayern_Cor22, DF22>=2.5)

Frage_22_Zufrieden[is.na(Frage_22_Zufrieden)] <- 0
Frage_22_Unzufrieden[is.na(Frage_22_Unzufrieden)] <- 0

wilcox.test(Frage_22_Unzufrieden$X3.AnzahlKinder,Frage_22_Zufrieden$X3.AnzahlKinder)
mean(Frage_22_Zufrieden$X3.AnzahlKinder)
mean(Frage_22_Unzufrieden$X3.AnzahlKinder)


# zeit ist korreliert mit stress
wilcox.test(Frage_20_Unzufrieden$`X5.StundenUnterrichtstag(UntereinerStunde0,Ueber6=6)`,
            Frage_20_Zufrieden$`X5.StundenUnterrichtstag(UntereinerStunde0,Ueber6=6)`)


mean(Frage_20_Zufrieden$`X5.StundenUnterrichtstag(UntereinerStunde0,Ueber6=6)`)
mean(Frage_20_Unzufrieden$`X5.StundenUnterrichtstag(UntereinerStunde0,Ueber6=6)`)



wilcox.test(Frage_21_Unzufrieden$`X5.StundenUnterrichtstag(UntereinerStunde0,Ueber6=6)`,
            Frage_21_Zufrieden$`X5.StundenUnterrichtstag(UntereinerStunde0,Ueber6=6)`)

mean(Frage_21_Zufrieden$`X5.StundenUnterrichtstag(UntereinerStunde0,Ueber6=6)`)
mean(Frage_21_Unzufrieden$`X5.StundenUnterrichtstag(UntereinerStunde0,Ueber6=6)`)


wilcox.test(Frage_22_Unzufrieden$`X5.StundenUnterrichtstag(UntereinerStunde0,Ueber6=6)`,
            Frage_22_Zufrieden$`X5.StundenUnterrichtstag(UntereinerStunde0,Ueber6=6)`)

mean(Frage_22_Zufrieden$`X5.StundenUnterrichtstag(UntereinerStunde0,Ueber6=6)`)
mean(Frage_22_Unzufrieden$`X5.StundenUnterrichtstag(UntereinerStunde0,Ueber6=6)`)

# probleme beim Zeitfinden

wilcox.test(Frage_20_Unzufrieden$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`,
            Frage_20_Zufrieden$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`)


mean(Frage_20_Zufrieden$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`)
mean(Frage_20_Unzufrieden$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`)



wilcox.test(Frage_21_Unzufrieden$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`,
            Frage_21_Zufrieden$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`)

mean(Frage_21_Zufrieden$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`)
mean(Frage_21_Unzufrieden$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`)


wilcox.test(Frage_22_Unzufrieden$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`,
            Frage_22_Zufrieden$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`)

mean(Frage_22_Zufrieden$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`)
mean(Frage_22_Unzufrieden$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`)

# kommunikation mit der lehrkraft
# per mail
wilcox.test(Frage_20_Unzufrieden$X8.PerMail,
            Frage_20_Zufrieden$X8.PerMail)


mean(Frage_21_Zufrieden$X8.PerMail)
mean(Frage_21_Unzufrieden$X8.PerMail)

wilcox.test(Frage_21_Unzufrieden$X8.PerMail,
            Frage_21_Zufrieden$X8.PerMail)


mean(Frage_21_Zufrieden$X8.PerMail)
mean(Frage_21_Unzufrieden$X8.PerMail)

wilcox.test(Frage_22_Unzufrieden$X8.PerMail,
            Frage_22_Zufrieden$X8.PerMail)


mean(Frage_22_Zufrieden$X8.PerMail)
mean(Frage_22_Unzufrieden$X8.PerMail)

# per videochat
wilcox.test(Frage_20_Unzufrieden$X8.PerVideochat,
            Frage_20_Zufrieden$X8.PerVideochat)


mean(Frage_20_Zufrieden$X8.PerVideochat)
mean(Frage_20_Unzufrieden$X8.PerVideochat)

wilcox.test(Frage_21_Unzufrieden$X8.PerVideochat,
            Frage_21_Zufrieden$X8.PerVideochat)


mean(Frage_21_Zufrieden$X8.PerVideochat)
mean(Frage_21_Unzufrieden$X8.PerVideochat)

wilcox.test(Frage_22_Unzufrieden$X8.PerVideochat,
            Frage_22_Zufrieden$X8.PerVideochat)


mean(Frage_22_Zufrieden$X8.PerVideochat)
mean(Frage_22_Unzufrieden$X8.PerVideochat)

# per telefon
wilcox.test(Frage_20_Unzufrieden$X8.PerTelefon,
            Frage_20_Zufrieden$X8.PerTelefon)


mean(Frage_20_Zufrieden$X8.PerTelefon)
mean(Frage_20_Unzufrieden$X8.PerTelefon)

wilcox.test(Frage_21_Unzufrieden$X8.PerTelefon,
            Frage_21_Zufrieden$X8.PerTelefon)


mean(Frage_21_Zufrieden$X8.PerTelefon)
mean(Frage_21_Unzufrieden$X8.PerTelefon)

wilcox.test(Frage_22_Unzufrieden$X8.PerTelefon,
            Frage_22_Zufrieden$X8.PerTelefon)


mean(Frage_22_Zufrieden$X8.PerTelefon)
mean(Frage_22_Unzufrieden$X8.PerTelefon)

# kommunikation lehrer und eltern
# per mail

wilcox.test(Frage_20_Unzufrieden$X9.ElternPerMail,
            Frage_20_Zufrieden$X9.ElternPerMail)


mean(Frage_21_Zufrieden$X9.ElternPerMail)
mean(Frage_21_Unzufrieden$X9.ElternPerMail)

wilcox.test(Frage_21_Unzufrieden$X9.ElternPerMail,
            Frage_21_Zufrieden$X9.ElternPerMail)


mean(Frage_21_Zufrieden$X9.ElternPerMail)
mean(Frage_21_Unzufrieden$X9.ElternPerMail)

wilcox.test(Frage_22_Unzufrieden$X9.ElternPerMail,
            Frage_22_Zufrieden$X9.ElternPerMail)


mean(Frage_22_Zufrieden$X9.ElternPerMail)
mean(Frage_22_Unzufrieden$X9.ElternPerMail)

# per videochat
wilcox.test(Frage_20_Unzufrieden$X9.ElternPerVideochat,
            Frage_20_Zufrieden$X9.ElternPerVideochat)


mean(Frage_20_Zufrieden$X9.ElternPerVideochat)
mean(Frage_20_Unzufrieden$X9.ElternPerVideochat)

wilcox.test(Frage_21_Unzufrieden$X9.ElternPerVideochat,
            Frage_21_Zufrieden$X9.ElternPerVideochat)


mean(Frage_21_Zufrieden$X9.ElternPerVideochat)
mean(Frage_21_Unzufrieden$X9.ElternPerVideochat)

wilcox.test(Frage_22_Unzufrieden$X9.ElternPerVideochat,
            Frage_22_Zufrieden$X9.ElternPerVideochat)


mean(Frage_22_Zufrieden$X9.ElternPerVideochat)
mean(Frage_22_Unzufrieden$X9.ElternPerVideochat)

# per telefon
wilcox.test(Frage_20_Unzufrieden$X9.ElternPerTelefon,
            Frage_20_Zufrieden$X9.ElternPerTelefon)


mean(Frage_20_Zufrieden$X9.ElternPerTelefon)
mean(Frage_20_Unzufrieden$X9.ElternPerTelefon)

wilcox.test(Frage_21_Unzufrieden$X9.ElternPerTelefon,
            Frage_21_Zufrieden$X9.ElternPerTelefon)


mean(Frage_21_Zufrieden$X9.ElternPerTelefon)
mean(Frage_21_Unzufrieden$X9.ElternPerTelefon)

wilcox.test(Frage_22_Unzufrieden$X9.ElternPerTelefon,
            Frage_22_Zufrieden$X9.ElternPerTelefon)


mean(Frage_22_Zufrieden$X9.ElternPerTelefon)
mean(Frage_22_Unzufrieden$X9.ElternPerTelefon)

# korrektur der lehrer
wilcox.test(Frage_20_Unzufrieden$`X10.KorrekturLehrer(nie=1)`,
            Frage_20_Zufrieden$`X10.KorrekturLehrer(nie=1)`)


mean(Frage_20_Zufrieden$`X10.KorrekturLehrer(nie=1)`)
mean(Frage_20_Unzufrieden$`X10.KorrekturLehrer(nie=1)`)

wilcox.test(Frage_21_Unzufrieden$`X10.KorrekturLehrer(nie=1)`,
            Frage_21_Zufrieden$`X10.KorrekturLehrer(nie=1)`)


mean(Frage_21_Zufrieden$`X10.KorrekturLehrer(nie=1)`)
mean(Frage_21_Unzufrieden$`X10.KorrekturLehrer(nie=1)`)

wilcox.test(Frage_22_Unzufrieden$`X10.KorrekturLehrer(nie=1)`,
            Frage_22_Zufrieden$`X10.KorrekturLehrer(nie=1)`)


mean(Frage_22_Zufrieden$`X10.KorrekturLehrer(nie=1)`)
mean(Frage_22_Unzufrieden$`X10.KorrekturLehrer(nie=1)`)

# tagesplan von der lehrkraft
wilcox.test(Frage_20_Unzufrieden$X11.WochenplanLehrkraft,
            Frage_20_Zufrieden$X11.WochenplanLehrkraft)


mean(Frage_20_Zufrieden$X11.WochenplanLehrkraft)
mean(Frage_20_Unzufrieden$X11.WochenplanLehrkraft)

wilcox.test(Frage_21_Unzufrieden$X11.WochenplanLehrkraft,
            Frage_21_Zufrieden$X11.WochenplanLehrkraft)


mean(Frage_21_Zufrieden$X11.WochenplanLehrkraft)
mean(Frage_21_Unzufrieden$X11.WochenplanLehrkraft)

wilcox.test(Frage_22_Unzufrieden$X11.WochenplanLehrkraft,
            Frage_22_Zufrieden$X11.WochenplanLehrkraft)


mean(Frage_22_Zufrieden$X11.WochenplanLehrkraft)
mean(Frage_22_Unzufrieden$X11.WochenplanLehrkraft)

# fehlen des austauschs Hypothese 8
wilcox.test(Frage_20_Unzufrieden$X12.AustauschKind,
            Frage_20_Zufrieden$X12.AustauschKind)


mean(Frage_20_Zufrieden$X12.AustauschKind)
mean(Frage_20_Unzufrieden$X12.AustauschKind)

wilcox.test(Frage_21_Unzufrieden$X12.AustauschKind,
            Frage_21_Zufrieden$X12.AustauschKind)


mean(Frage_21_Zufrieden$X12.AustauschKind)
mean(Frage_21_Unzufrieden$X12.AustauschKind)

wilcox.test(Frage_22_Unzufrieden$X12.AustauschKind,
            Frage_22_Zufrieden$X12.AustauschKind)


mean(Frage_22_Zufrieden$X12.AustauschKind)
mean(Frage_22_Unzufrieden$X12.AustauschKind)

wilcox.test(Frage_20_Unzufrieden$X12.AustauschKind,
            Frage_20_Zufrieden$X12.AustauschKind)


# erreichbarkeit Hypothese 6
wilcox.test(Frage_20_Unzufrieden$X13.ErreichbarkeitLehrkraft,
            Frage_20_Zufrieden$X13.ErreichbarkeitLehrkraft)
mean(Frage_20_Zufrieden$X13.ErreichbarkeitLehrkraft)
mean(Frage_20_Unzufrieden$X13.ErreichbarkeitLehrkraft)

wilcox.test(Frage_21_Unzufrieden$X13.ErreichbarkeitLehrkraft,
            Frage_21_Zufrieden$X13.ErreichbarkeitLehrkraft)


mean(Frage_21_Zufrieden$X13.ErreichbarkeitLehrkraft)
mean(Frage_21_Unzufrieden$X13.ErreichbarkeitLehrkraft)

wilcox.test(Frage_22_Unzufrieden$X13.ErreichbarkeitLehrkraft,
            Frage_22_Zufrieden$X13.ErreichbarkeitLehrkraft)


mean(Frage_22_Zufrieden$X13.ErreichbarkeitLehrkraft)
mean(Frage_22_Unzufrieden$X13.ErreichbarkeitLehrkraft)

# hilfe bei anderen Hypothese 9
wilcox.test(Frage_20_Unzufrieden$`X14.HilfeFuerKinder(ja=1)`,
            Frage_20_Zufrieden$`X14.HilfeFuerKinder(ja=1)`)
mean(Frage_20_Zufrieden$`X14.HilfeFuerKinder(ja=1)`)
mean(Frage_20_Unzufrieden$`X14.HilfeFuerKinder(ja=1)`)

wilcox.test(Frage_21_Unzufrieden$`X14.HilfeFuerKinder(ja=1)`,
            Frage_21_Zufrieden$`X14.HilfeFuerKinder(ja=1)`)


mean(Frage_21_Zufrieden$`X14.HilfeFuerKinder(ja=1)`)
mean(Frage_21_Unzufrieden$`X14.HilfeFuerKinder(ja=1)`)

wilcox.test(Frage_22_Unzufrieden$`X14.HilfeFuerKinder(ja=1)`,
            Frage_22_Zufrieden$`X14.HilfeFuerKinder(ja=1)`)


mean(Frage_22_Zufrieden$`X14.HilfeFuerKinder(ja=1)`)
mean(Frage_22_Unzufrieden$`X14.HilfeFuerKinder(ja=1)`)

# motivation der eltern
wilcox.test(Frage_20_Unzufrieden$X15.MotivationKind,
            Frage_20_Zufrieden$X15.MotivationKind)
mean(Frage_20_Zufrieden$X15.MotivationKind)
mean(Frage_20_Unzufrieden$X15.MotivationKind)

wilcox.test(Frage_21_Unzufrieden$X15.MotivationKind,
            Frage_21_Zufrieden$X15.MotivationKind)
mean(Frage_21_Zufrieden$X15.MotivationKind)
mean(Frage_21_Unzufrieden$X15.MotivationKind)

wilcox.test(Frage_22_Unzufrieden$X15.MotivationKind,
            Frage_22_Zufrieden$X15.MotivationKind)


mean(Frage_22_Zufrieden$X15.MotivationKind)
mean(Frage_22_Unzufrieden$X15.MotivationKind)

# planen mit eltern
wilcox.test(Frage_20_Unzufrieden$X15.Planung,
            Frage_20_Zufrieden$X15.Planung)
mean(Frage_20_Zufrieden$X15.Planung)
mean(Frage_20_Unzufrieden$X15.Planung)

wilcox.test(Frage_21_Unzufrieden$X15.Planung,
            Frage_21_Zufrieden$X15.Planung)
mean(Frage_21_Zufrieden$X15.Planung)
mean(Frage_21_Unzufrieden$X15.Planung)

wilcox.test(Frage_22_Unzufrieden$X15.Planung,
            Frage_22_Zufrieden$X15.Planung)


mean(Frage_22_Zufrieden$X15.Planung)
mean(Frage_22_Unzufrieden$X15.Planung)


#motivation anderer personen
wilcox.test(Frage_20_Unzufrieden$X16.MotivationKind,
            Frage_20_Zufrieden$X16.MotivationKind)
mean(Frage_20_Zufrieden$X16.MotivationKind)
mean(Frage_20_Unzufrieden$X16.MotivationKind)

wilcox.test(Frage_21_Unzufrieden$X16.MotivationKind,
            Frage_21_Zufrieden$X16.MotivationKind)
mean(Frage_21_Zufrieden$X16.MotivationKind)
mean(Frage_21_Unzufrieden$X16.MotivationKind)

wilcox.test(Frage_22_Unzufrieden$X16.MotivationKind,
            Frage_22_Zufrieden$X16.MotivationKind)


mean(Frage_22_Zufrieden$X16.MotivationKind)
mean(Frage_22_Unzufrieden$X16.MotivationKind)

# planen mit anderen personen
wilcox.test(Frage_20_Unzufrieden$X16.Planen,
            Frage_20_Zufrieden$X16.Planen)
mean(Frage_20_Zufrieden$X16.Planen)
mean(Frage_20_Unzufrieden$X16.Planen)

wilcox.test(Frage_21_Unzufrieden$X16.Planen,
            Frage_21_Zufrieden$X16.Planen)
mean(Frage_21_Zufrieden$X16.Planen)
mean(Frage_21_Unzufrieden$X16.Planen)

wilcox.test(Frage_22_Unzufrieden$X16.Planen,
            Frage_22_Zufrieden$X16.Planen)

# streit
wilcox.test(Frage_20_Unzufrieden$X17.Streit,
            Frage_20_Zufrieden$X17.Streit)
mean(Frage_20_Zufrieden$X17.Streit)
mean(Frage_20_Unzufrieden$X17.Streit)

wilcox.test(Frage_21_Unzufrieden$X17.Streit,
            Frage_21_Zufrieden$X17.Streit)
mean(Frage_21_Zufrieden$X17.Streit)
mean(Frage_21_Unzufrieden$X17.Streit)

wilcox.test(Frage_22_Unzufrieden$X17.Streit,
            Frage_22_Zufrieden$X17.Streit)

# teilzeitarbeit

# filtern der datensaetze nach homeoffice und vollzeit
# frage nach allgemeinen stress
# Hypothese 2
Frage_20_Teilzeit=subset(Bayern_Cor20, `X1.Homeoffice(nichterwerb=0,teilzeitweniger50=1,teilzeitmehrals50=2,3=3)`==1 |
                             `X1.Homeoffice(nichterwerb=0,teilzeitweniger50=1,teilzeitmehrals50=2,3=3)`==2)
Frage_20_Vollzeit=subset(Bayern_Cor20, `X1.Homeoffice(nichterwerb=0,teilzeitweniger50=1,teilzeitmehrals50=2,3=3)`==3)

wilcox.test(Frage_20_Teilzeit$DF20,
            Frage_20_Vollzeit$DF20)
mean(Frage_20_Teilzeit$DF20)
mean(Frage_20_Vollzeit$DF20)

Frage_21_Teilzeit=subset(Bayern_Cor21, `X1.Homeoffice(nichterwerb=0,teilzeitweniger50=1,teilzeitmehrals50=2,3=3)`==1 |
                             `X1.Homeoffice(nichterwerb=0,teilzeitweniger50=1,teilzeitmehrals50=2,3=3)`==2)
Frage_21_Vollzeit=subset(Bayern_Cor21, `X1.Homeoffice(nichterwerb=0,teilzeitweniger50=1,teilzeitmehrals50=2,3=3)`==3)

wilcox.test(Frage_21_Teilzeit$DF21,
            Frage_21_Vollzeit$DF21)
mean(Frage_21_Teilzeit$DF21)
mean(Frage_21_Vollzeit$DF21)

Frage_22_Teilzeit=subset(Bayern_Cor22, `X1.Homeoffice(nichterwerb=0,teilzeitweniger50=1,teilzeitmehrals50=2,3=3)`==1 |
                             `X1.Homeoffice(nichterwerb=0,teilzeitweniger50=1,teilzeitmehrals50=2,3=3)`==2)
Frage_22_Vollzeit=subset(Bayern_Cor22, `X1.Homeoffice(nichterwerb=0,teilzeitweniger50=1,teilzeitmehrals50=2,3=3)`==3)

wilcox.test(Frage_22_Teilzeit$DF22,
            Frage_22_Vollzeit$DF22)
mean(Frage_22_Homeoffice$DF22)
mean(Frage_22_Vollzeit$DF22)


# frage nach 100 Prozent Ho und Präsenz
# Hypothese 1
Frage_20_Praesenez=subset(Bayern_Cor20,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==2)
Frage_20_HO=subset(Bayern_Cor20,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==1 |`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==0 )


wilcox.test(Frage_20_Praesenez$DF20,
            Frage_20_HO$DF20)
mean(Frage_20_Praesenez$DF20)
mean(Frage_20_HO$DF20)

Frage_21_Praesenez=subset(Bayern_Cor21,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==2)
Frage_21_HO=subset(Bayern_Cor21,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==1 |`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==0 )


wilcox.test(Frage_21_Praesenez$DF21,
            Frage_21_HO$DF21)
mean(Frage_21_Praesenez$DF21)
mean(Frage_21_HO$DF21)

Frage_22_Praesenez=subset(Bayern_Cor22,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==2)
Frage_22_HO=subset(Bayern_Cor22,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==1 |`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==0 )


wilcox.test(Frage_22_Praesenez$DF22,
            Frage_22_HO$DF22)
mean(Frage_22_Praesenez$DF22)
mean(Frage_22_HO$DF22)

# frage unterschied vor und nach lockdown
Frage_18_20_GerneInSchule=subset(Bayern_Cor20,X18.GerneSchule >2.5)
Frage_18_20_NichtGerneInSchule=subset(Bayern_Cor20,X18.GerneSchule <=2.5)

Frage_18_21_GerneInSchule=subset(Bayern_Cor21,X18.GerneSchule >2.5)
Frage_18_21_NichtGerneInSchule=subset(Bayern_Cor21,X18.GerneSchule <=2.5)

Frage_18_22_GerneInSchule=subset(Bayern_Cor22,X18.GerneSchule >2.5)
Frage_18_22_NichtGerneInSchule=subset(Bayern_Cor22,X18.GerneSchule <=2.5)

wilcox.test(Frage_18_GerneInSchule$DF20,
            Frage_18_NichtGerneInSchule$DF20)
mean(Frage_18_GerneInSchule$DF20)
mean(Frage_18_NichtGerneInSchule$DF20)

wilcox.test(Frage_18_21_GerneInSchule$DF21,
            Frage_18_21_NichtGerneInSchule$DF21)
mean(Frage_18_21_GerneInSchule$DF21)
mean(Frage_18_21_NichtGerneInSchule$DF21)

wilcox.test(Frage_18_22_GerneInSchule$DF22,
            Frage_18_22_NichtGerneInSchule$DF22)
mean(Frage_18_22_GerneInSchule$DF22)
mean(Frage_18_22_NichtGerneInSchule$DF22)

# motivation kind vor und nach lockdown
wilcox.test(Frage_18_20_GerneInSchule$X15.MotivationKind,
            Frage_18_20_NichtGerneInSchule$X15.MotivationKind)
mean(Frage_18_20_GerneInSchule$X15.MotivationKind)
mean(Frage_18_20_NichtGerneInSchule$X15.MotivationKind)

wilcox.test(Frage_18_21_GerneInSchule$X15.MotivationKind,
           Frage_18_21_NichtGerneInSchule$X15.MotivationKind)
mean(Frage_18_21_GerneInSchule$X15.MotivationKind)
mean(Frage_18_21_NichtGerneInSchule$X15.MotivationKind)


wilcox.test(Frage_18_21_GerneInSchule$X15.MotivationKind,
            Frage_18_21_NichtGerneInSchule$X15.MotivationKind)
mean(Frage_18_21_GerneInSchule$X15.MotivationKind)
mean(Frage_18_21_NichtGerneInSchule$X15.MotivationKind)
# streit
wilcox.test(Frage_18_20_GerneInSchule$X17.Streit,
            Frage_18_20_NichtGerneInSchule$X17.Streit)
mean(Frage_18_20_GerneInSchule$X17.Streit)
mean(Frage_18_20_NichtGerneInSchule$X17.Streit)

wilcox.test(Frage_18_21_GerneInSchule$X17.Streit,
            Frage_18_21_NichtGerneInSchule$X17.Streit)
mean(Frage_18_21_GerneInSchule$X17.Streit)
mean(Frage_18_21_NichtGerneInSchule$X17.Streit)


wilcox.test(Frage_18_21_GerneInSchule$X17.Streit,
            Frage_18_21_NichtGerneInSchule$X17.Streit)
mean(Frage_18_21_GerneInSchule$X17.Streit)
mean(Frage_18_21_NichtGerneInSchule$X17.Streit)

# unterschied vor und nach lockdown Hypothese 10

wilcox.test(Bayern$X19.MotivationKind,
            Bayern$X15.MotivationKind)
mean(Bayern$X19.MotivationKind)
mean(Bayern$X15.MotivationKind)

wilcox.test(Bayern$X19.Planung,
            Bayern$X15.Planung)
mean(Bayern$X19.Planung)
mean(Bayern$X15.Planung)

wilcox.test(Bayern$X19.Vorlesen,
            Bayern$X15.Vorlesen)
mean(Bayern$X19.Vorlesen)
mean(Bayern$X15.Vorlesen)

# Unterschied  eigenständiges Lernen und Stress 
# Hypothese 11
wilcox.test(Frage_20_Unzufrieden$X18.Selbstorganisation,
            Frage_20_Zufrieden$X18.Selbstorganisation)
mean(Frage_20_Zufrieden$X18.Selbstorganisation)
mean(Frage_20_Unzufrieden$X18.Selbstorganisation)

wilcox.test(Frage_21_Unzufrieden$X18.Selbstorganisation,
            Frage_21_Zufrieden$X18.Selbstorganisation)
mean(Frage_21_Zufrieden$X18.Selbstorganisation)
mean(Frage_21_Unzufrieden$X18.Selbstorganisation)

wilcox.test(Frage_22_Unzufrieden$X18.Selbstorganisation,
            Frage_22_Zufrieden$X18.Selbstorganisation)

mean(Frage_22_Zufrieden$X18.Selbstorganisation)
mean(Frage_22_Unzufrieden$X18.Selbstorganisation)

# eltern die 100 prozent zu hause sind, sind gestresster
# hypothese 12


Frage_20_100ProHomeoffice=subset(Bayern_Cor20,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==0)
Frage_20_Nicht100ProHomeoffice=subset(Bayern_Cor20,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==1 |
                                           `X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==2)

Frage_21_100ProHomeoffice=subset(Bayern_Cor21,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==0)
Frage_21_Nicht100ProHomeoffice=subset(Bayern_Cor21,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==1 |
                                           `X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==2)

Frage_22_100ProHomeoffice=subset(Bayern_Cor22,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==0)
Frage_22_Nicht100ProHomeoffice=subset(Bayern_Cor22,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==1 |
                                           `X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==2)

wilcox.test(Frage_20_100ProHomeoffice$DF20,
            Frage_20_Nicht100ProHomeoffice$DF20)
mean(Frage_20_100ProHomeoffice$DF20)
mean(Frage_20_Nicht100ProHomeoffice$DF20)

wilcox.test(Frage_21_100ProHomeoffice$DF21,
            Frage_21_Nicht100ProHomeoffice$DF21)
mean(Frage_21_100ProHomeoffice$DF21)
mean(Frage_21_Nicht100ProHomeoffice$DF21)

wilcox.test(Frage_22_100ProHomeoffice$DF22,
            Frage_22_Nicht100ProHomeoffice$DF22)
mean(Frage_22_100ProHomeoffice$DF22)
mean(Frage_22_Nicht100ProHomeoffice$DF22)

# Hypothese 14: hilfe anderer personen reduziert stress
# motivation
wilcox.test(Frage_20_Unzufrieden$X16.MotivationKind,
            Frage_20_Zufrieden$X16.MotivationKind)
mean(Frage_20_Zufrieden$X16.MotivationKind)
mean(Frage_20_Unzufrieden$X16.MotivationKind)

wilcox.test(Frage_21_Unzufrieden$X16.MotivationKind,
            Frage_21_Zufrieden$X16.MotivationKind)

mean(Frage_21_Unzufrieden$X16.MotivationKind)
mean(Frage_21_Zufrieden$X16.MotivationKind)

wilcox.test(Frage_22_Unzufrieden$X16.MotivationKind,
            Frage_22_Zufrieden$X16.MotivationKind)


mean(Frage_22_Unzufrieden$X16.MotivationKind)
mean(Frage_22_Zufrieden$X16.MotivationKind)

# planung
wilcox.test(Frage_20_Unzufrieden$X16.Planen,
            Frage_20_Zufrieden$X16.Planen)
mean(Frage_20_Zufrieden$X16.Planen)
mean(Frage_20_Unzufrieden$X16.Planen)


wilcox.test(Frage_21_Unzufrieden$X16.Planen,
            Frage_21_Zufrieden$X16.Planen)

mean(Frage_21_Unzufrieden$X16.Planen)
mean(Frage_21_Zufrieden$X16.Planen)

wilcox.test(Frage_22_Unzufrieden$X16.Planen,
            Frage_22_Zufrieden$X16.Planen)


mean(Frage_22_Unzufrieden$X16.Planen)
mean(Frage_22_Zufrieden$X16.Planen)

# Technik
wilcox.test(Frage_20_Unzufrieden$X16.Technik,
            Frage_20_Zufrieden$X16.Technik)
mean(Frage_20_Zufrieden$X16.Technik)
mean(Frage_20_Unzufrieden$X16.Technik)


wilcox.test(Frage_21_Unzufrieden$X16.Technik,
            Frage_21_Zufrieden$X16.Technik)

mean(Frage_21_Unzufrieden$X16.Technik)
mean(Frage_21_Zufrieden$X16.Technik)

wilcox.test(Frage_22_Unzufrieden$X16.Technik,
            Frage_22_Zufrieden$X16.Technik)


mean(Frage_22_Unzufrieden$X16.Technik)
mean(Frage_22_Zufrieden$X16.Technik)

# vorlesen etc.
wilcox.test(Frage_20_Unzufrieden$X16.Vorlesen,
            Frage_20_Zufrieden$X16.Vorlesen)
mean(Frage_20_Zufrieden$X16.Vorlesen)
mean(Frage_20_Unzufrieden$X16.Vorlesen)


wilcox.test(Frage_21_Unzufrieden$X16.Vorlesen,
            Frage_21_Zufrieden$X16.Vorlesen)

mean(Frage_21_Unzufrieden$X16.Vorlesen)
mean(Frage_21_Zufrieden$X16.Vorlesen)

wilcox.test(Frage_22_Unzufrieden$X16.Vorlesen,
            Frage_22_Zufrieden$X16.Vorlesen)


mean(Frage_22_Unzufrieden$X16.Vorlesen)
mean(Frage_22_Zufrieden$X16.Vorlesen)

# vehalten vor dem Lockdown
wilcox.test(Frage_20_Unzufrieden$X18.GerneSchule,
            Frage_20_Zufrieden$X18.GerneSchule)
mean(Frage_20_Zufrieden$X18.GerneSchule)
mean(Frage_20_Unzufrieden$X18.GerneSchule)


wilcox.test(Frage_21_Unzufrieden$X18.GerneSchule,
            Frage_21_Zufrieden$X18.GerneSchule)

mean(Frage_21_Unzufrieden$X18.GerneSchule)
mean(Frage_21_Zufrieden$X18.GerneSchule)

wilcox.test(Frage_22_Unzufrieden$X18.GerneSchule,
            Frage_22_Zufrieden$X18.GerneSchule)


mean(Frage_22_Unzufrieden$X18.GerneSchule)
mean(Frage_22_Zufrieden$X18.GerneSchule)

# alleine bewältigen
wilcox.test(Frage_20_Unzufrieden$X18.Alleine,
            Frage_20_Zufrieden$X18.Alleine)
mean(Frage_20_Zufrieden$X18.Alleine)
mean(Frage_20_Unzufrieden$X18.Alleine)


wilcox.test(Frage_21_Unzufrieden$X18.Alleine,
            Frage_21_Zufrieden$X18.Alleine)

mean(Frage_21_Unzufrieden$X18.Alleine)
mean(Frage_21_Zufrieden$X18.Alleine)

wilcox.test(Frage_22_Unzufrieden$X18.Alleine,
            Frage_22_Zufrieden$X18.Alleine)


mean(Frage_22_Unzufrieden$X18.Alleine)
mean(Frage_22_Zufrieden$X18.Alleine)

# streit beim lernen
wilcox.test(Frage_20_Unzufrieden$X18.StreitLernen,
            Frage_20_Zufrieden$X18.StreitLernen)
mean(Frage_20_Zufrieden$X18.StreitLernen)
mean(Frage_20_Unzufrieden$X18.StreitLernen)


wilcox.test(Frage_21_Unzufrieden$X18.StreitLernen,
            Frage_21_Zufrieden$X18.StreitLernen)

mean(Frage_21_Unzufrieden$X18.StreitLernen)
mean(Frage_21_Zufrieden$X18.StreitLernen)

wilcox.test(Frage_22_Unzufrieden$X18.StreitLernen,
            Frage_22_Zufrieden$X18.StreitLernen)


mean(Frage_22_Unzufrieden$X18.StreitLernen)
mean(Frage_22_Zufrieden$X18.StreitLernen)

# selbstorganisation
wilcox.test(Frage_20_Unzufrieden$X18.Selbstorganisation,
            Frage_20_Zufrieden$X18.Selbstorganisation)
mean(Frage_20_Zufrieden$X18.Selbstorganisation)
mean(Frage_20_Unzufrieden$X18.Selbstorganisation)


wilcox.test(Frage_21_Unzufrieden$X18.Selbstorganisation,
            Frage_21_Zufrieden$X18.Selbstorganisation)

mean(Frage_21_Unzufrieden$X18.Selbstorganisation)
mean(Frage_21_Zufrieden$X18.Selbstorganisation)

wilcox.test(Frage_22_Unzufrieden$X18.Selbstorganisation,
            Frage_22_Zufrieden$X18.Selbstorganisation)


mean(Frage_22_Unzufrieden$X18.Selbstorganisation)
mean(Frage_22_Zufrieden$X18.Selbstorganisation)

# selbstlernen und Stress
# Hypothese 16

Frage_18_20_GutSelbstLernen=subset(Bayern_Cor20,X18.Selbstorganisation > 2.5)
Frage_18_20_SchlechtSelbstLernen=subset(Bayern_Cor20,X18.Selbstorganisation <=2.5)

Frage_18_21_GutSelbstLernen=subset(Bayern_Cor21,X18.Selbstorganisation >2.5)
Frage_18_21_SchlechtSelbstLernen=subset(Bayern_Cor21,X18.Selbstorganisation <=2.5)

Frage_18_22_GutSelbstLernen=subset(Bayern_Cor22,X18.Selbstorganisation >2.5)
Frage_18_22_SchlechtSelbstLernen=subset(Bayern_Cor22,X18.Selbstorganisation <=2.5)

wilcox.test(Frage_18_20_GutSelbstLernen$DF20,
            Frage_18_20_SchlechtSelbstLernen$DF20)
mean(Frage_18_20_GutSelbstLernen$DF20)
mean(Frage_18_20_SchlechtSelbstLernen$DF20)


wilcox.test(Frage_18_21_GutSelbstLernen$DF21,
            Frage_18_21_SchlechtSelbstLernen$DF21)
mean(Frage_18_21_GutSelbstLernen$DF21)
mean(Frage_18_21_SchlechtSelbstLernen$DF21)


wilcox.test(Frage_18_22_GutSelbstLernen$DF22,
            Frage_18_22_SchlechtSelbstLernen$DF22)
mean(Frage_18_22_GutSelbstLernen$DF22)
mean(Frage_18_22_SchlechtSelbstLernen$DF22)

# Zusammengefasst


wilcox.test(Frage_20_Unzufrieden$Item_18,
            Frage_20_Zufrieden$Item_18)
mean(Frage_20_Zufrieden$Item_18)
mean(Frage_20_Unzufrieden$Item_18)

wilcox.test(Frage_21_Unzufrieden$Item_18,
            Frage_21_Zufrieden$Item_18)
mean(Frage_21_Zufrieden$Item_18)
mean(Frage_21_Unzufrieden$Item_18)

wilcox.test(Frage_22_Unzufrieden$Item_18,
            Frage_22_Zufrieden$Item_18)
mean(Frage_22_Zufrieden$Item_18)
mean(Frage_22_Unzufrieden$Item_18)

Frage_18_20_GutSelbstLernen=subset(Bayern_Cor20,Item_18 >= 2.5)
Frage_18_20_SchlechtSelbstLernen=subset(Bayern_Cor20,Item_18 <2.5)

Frage_18_21_GutSelbstLernen=subset(Bayern_Cor21,Item_18>=2.5)
Frage_18_21_SchlechtSelbstLernen=subset(Bayern_Cor21,Item_18 <2.5)

Frage_18_22_GutSelbstLernen=subset(Bayern_Cor22,Item_18>=2.5)
Frage_18_22_SchlechtSelbstLernen=subset(Bayern_Cor22,Item_18 <2.5)

# Hypothese 16
wilcox.test(Frage_18_20_GutSelbstLernen$DF20,
            Frage_18_20_SchlechtSelbstLernen$DF20)
mean(Frage_18_20_GutSelbstLernen$DF20)
mean(Frage_18_20_SchlechtSelbstLernen$DF20)


wilcox.test(Frage_18_21_GutSelbstLernen$DF21,
            Frage_18_21_SchlechtSelbstLernen$DF21)
mean(Frage_18_21_GutSelbstLernen$DF21)
mean(Frage_18_21_SchlechtSelbstLernen$DF21)


wilcox.test(Frage_18_22_GutSelbstLernen$DF22,
            Frage_18_22_SchlechtSelbstLernen$DF22)
mean(Frage_18_22_GutSelbstLernen$DF22)
mean(Frage_18_22_SchlechtSelbstLernen$DF22)

# selbstlernen und Zeit
# Hypothese 17

wilcox.test(Frage_18_20_GutSelbstLernen$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`,
            Frage_18_20_SchlechtSelbstLernen$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`)
mean(Frage_18_20_GutSelbstLernen$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`)
mean(Frage_18_20_SchlechtSelbstLernen$`X6.ZeitKinder(trifftueberhauptnichtzu=1)`)

# eltern die 100 prozent präsent arbeiten, sind gestresster
# variable =0
# Hypothese 13

Frage_20_100Praesenz=subset(Bayern_Cor20,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==2)
Frage_20_Nicht100Praesenz=subset(Bayern_Cor20,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==1 |
                                        `X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==0)

Frage_21_100Praesenz=subset(Bayern_Cor21,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==2)
Frage_21_Nicht100Praesenz=subset(Bayern_Cor21,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==1 |
                                        `X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==0)

Frage_22_100Praesenz=subset(Bayern_Cor22,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==2)
Frage_22_Nicht100Praesenz=subset(Bayern_Cor22,`X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==1 |
                                        `X2.Arbeitssituation(100HO=0,Wechsel=1,100Praesenz=2)`==0)

wilcox.test(Frage_20_100Praesenz$DF20,
            Frage_20_Nicht100Praesenz$DF20)
mean(Frage_20_100Praesenz$DF20)
mean(Frage_20_Nicht100Praesenz$DF20)

wilcox.test(Frage_21_100Praesenz$DF21,
            Frage_21_Nicht100Praesenz$DF21)
mean(Frage_21_100Praesenz$DF21)
mean(Frage_21_Nicht100Praesenz$DF21)

wilcox.test(Frage_22_100Praesenz$DF22,
            Frage_22_Nicht100Praesenz$DF22)
mean(Frage_22_100Praesenz$DF22)
mean(Frage_22_Nicht100Praesenz$DF22)


mean(Frage_22_Zufrieden$X12.AustauschKind)
mean(Frage_21_Unzufrieden$X12.AustauschKind)
