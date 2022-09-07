# ANovA
# tests für ANOVA und Kruskal


library("readxl")
library("ggplot2")
library(dplyr)
library(tidyr)
library(scales)
library("DescTools")
library("car")
library("ggpubr")
library("knitr")
library("writexl")
library(tidyverse)
library(ggpubr)
library(rstatix)
library(psych)
library("xlsx")
library("sjstats")
## anova and interaction effects

my_data_model <- readxl::read_excel("ANOVA_Model_Datensatz.xlsx",col_names = TRUE)

my_data_model <- data.frame(my_data_model, check.names = TRUE)
my_data_model [is.na(my_data_model )] <- 0

my_dataframe_anova$DF20[my_dataframe_anova$DF20 == "Gestresst"] <- 1
my_dataframe_anova$DF20[my_dataframe_anova$DF20 == "Ungestresst"] <-0 


# Kronbach Frage 18
alpha(my_dataframe_anova[c("X18.Alleine","X18.Selbstorganisation")],check.keys=TRUE)

Item_18<-c(my_data_anova$X18.Alleine+my_dataframe_anova$X18.Selbstorganisation)/2


model = lm(my_data_model$DF20~
             my_data_model$X6.ZeitKinder + 
             my_data_model$X12.AustauschKind +
             my_data_model$X15.MotivationKind +
             my_data_model$X15.Planung +
             my_data_model$X15.Technik+
             my_data_model$X6.ZeitKinder:my_data_model$X3.AnzahlKinder+
             my_data_model$X6.ZeitKinder:my_data_model$X5.StundenUnterrichtstag+
             my_data_model$X6.ZeitKinder:my_data_model$X12.AustauschKind+
             my_data_model$X15.MotivationKind:my_data_model$X19.MotivationKind+
             my_data_model$X15.MotivationKind:my_data_model$X16.MotivationKind+
             my_data_model$X15.Technik:my_data_model$X15.Planung+
             my_data_model$X6.ZeitKinder:my_data_model$X15.MotivationKind,
           data = my_data_model)



sstable <- Anova(model)
sstable
options(knitr.kable.NA = '')
write.xlsx(sstable, file="anova.xlsx",col.names=TRUE, row.names=TRUE,
           append = FALSE)

#kruskal test


kruskal.test(my_data_model$X6.ZeitKinder + 
               my_data_model$X12.AustauschKind +
               my_data_model$X15.MotivationKind +
               my_data_model$X15.Planung +
               my_data_model$X15.Technik
             ~my_data_model$DF20)

my_data_model %>% kruskal_effsize(my_data_model$X12.AustauschKind
                ~my_data_model$DF20)




options(knitr.kable.NA = '')
sstable <- Anova(model)
sstable
options(knitr.kable.NA = '')
write.xlsx(sstable, file="anova.xlsx",col.names=TRUE, row.names=TRUE,
           append = FALSE)

model = lm(my_data_model$DF21~
             my_data_model$X6.ZeitKinder + 
             my_data_model$X12.AustauschKind +
             my_data_model$X8.PerTelefon +
             my_data_model$X14.HilfeFuerKinder+
             my_data_model$X15.MotivationKind +
             my_data_model$X16.MotivationKind +
             my_data_model$X16.Planen +
             my_data_model$X6.ZeitKinder:my_data_model$X3.AnzahlKinder+
             my_data_model$X8.PerMail:my_data_model$X8.PerVideochat+
             my_data_model$X16.Planen :my_data_model$X16.Technik+
             my_data_model$X6.ZeitKinder:my_data_model$X5.StundenUnterrichtstag+
             my_data_model$X19.MotivationKind:my_data_model$X15.MotivationKind+
             my_data_model$X15.MotivationKind:my_data_model$X16.MotivationKind+
             my_data_model$X6.ZeitKinder:my_data_model$X15.MotivationKind,
           data = my_data_model)

sstable <- Anova(model)
sstable
options(knitr.kable.NA = '')
write.xlsx(sstable, file="anova.xlsx",col.names=TRUE, row.names=TRUE,
           append = FALSE)
eta_squared(model)

kruskal.test(my_data_model$X6.ZeitKinder + 
               my_data_model$X12.AustauschKind +
               my_data_model$X13.ErreichbarkeitLehrkraft +
               my_data_model$X8.PerTelefon +
               my_data_model$X14.HilfeFuerKinder+
               my_data_model$X16.MotivationKind +
               my_data_model$X16.Planen 
               ~my_data_model$DF21)

my_data_model %>% kruskal_effsize( my_data_model$X16.Planen 
                                  ~my_data_model$DF21)



model = lm(my_data_model$DF22~
             my_data_model$X6.ZeitKinder + 
             my_data_model$X12.AustauschKind+
             my_data_model$X6.ZeitKinder:my_data_model$X12.AustauschKind+
             my_data_model$X19.MotivationKind:my_data_model$X15.MotivationKind+
             my_data_model$X15.MotivationKind:my_data_model$X16.MotivationKind+
             my_data_model$X6.ZeitKinder:my_data_model$X15.MotivationKind,
           data = my_data_model)

sstable <- Anova(model)
sstable
options(knitr.kable.NA = '')
write.xlsx(sstable, file="anova.xlsx",col.names=TRUE, row.names=TRUE,
            append = FALSE)


kruskal.test(my_data_model$X6.ZeitKinder + 
               my_data_model$X12.AustauschKind
             ~my_data_model$DF22)

my_data_model %>% kruskal_effsize( my_data_model$X12.AustauschKind
                                   ~my_data_model$DF22)

# post hoc
library("FSA")

Input = ("
         Wert               p_wert
         ZeitKinder         4.64e-11
         PlanungKind        0.0065
         AustauschKind      0.04586
         KindTechnik        0.0461
         KindMotivation     0.06004
         ")

Data = read.table(textConnection(Input),header=TRUE)
Data = Data[order(Data$p_wert),]
Bonferroni =
  p.adjust(Data$p_wert,
           method = "bonferroni")

Bonferroni

Input = ("
 Wert               p_wert
 ZeitKinder         1.71924121692005e-10
 AustauschKind      2.34936249208349e-07
 Telefon            0.00227973845143858
 Lehrkraft          0.0111003419569046
 Motivation16       0.0355365115500135
 PlanungKind        0.0317142975758053
 Hilfe              0.0532409493496485
 Motivation15       0.0659259919301424
")

Data = read.table(textConnection(Input),header=TRUE)
Data = Data[order(Data$p_wert),]
Bonferroni =
  p.adjust(Data$p_wert,
           method = "bonferroni")
Bonferroni

Input = ("
 Wert               p_wert
 ZeitKinder         1.36e-23
 AustauschKind      3.06e-05
")

Data = read.table(textConnection(Input),header=TRUE)
Data = Data[order(Data$p_wert),]
Bonferroni =
  p.adjust(Data$p_wert,
           method = "bonferroni")
Bonferroni

model = lm(my_data_model$DF22~
             my_data_model$X6.ZeitKinder + 
             my_data_model$X12.AustauschKind)

model = lm(
             my_data_model$X6.ZeitKinder + 
             my_data_model$X18.Selbstorganisation +
             my_data_model$X12.AustauschKind +
             my_data_model$X15.MotivationKind +
             my_data_model$X15.Planung +
             my_data_model$X15.Technik
             ~as.factor(my_data_model$DF20),
           data = my_data_model)

# ???
ANOVA=aov(model)
TUKEY <- TukeyHSD(ANOVA, 'my_data_anova$DF20', conf.level=0.95)
TUKEY
