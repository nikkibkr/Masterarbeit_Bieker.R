# Auswertung Masterarbeit

# Laden der benötigten Pakete
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape2)
library(tidyr)
library(tidyverse)
library(car)
library(psych)


### Fahrsimulatordaten ###

# working directory setzen 
setwd("C:/Users/nikla/Documents/Data/MA")

# Daten hochladen DS_STc
DS_STc <- read_csv("reaction_times_stc.csv")
colnames(DS_STc) [1] <- "subjectID"
DS_STc$subjectID <- sub("^sub", "", DS_STc$subjectID)

# Reaktionszeiten in Sekunden umrechnen und entfernen der SubjectIDs >61
DS_STc <- DS_STc[DS_STc$subjectID >= "sub61", ]
DS_STc[, -1] <- lapply(DS_STc[, -1], function(x) ifelse(!is.na(x), x / 1000, x))

# Präfix "sub" entfernen und nur Zahlen behalten
DS_STc$subjectID <- substr(DS_STc$subjectID, 4, nchar(DS_STc$subjectID))

# Reaktionszeiten >0,1 und <4 herausfiltern
num_columns <- ncol(DS_STc)
for (i in 2:num_columns) {
  if (grepl("^RT_STc", colnames(DS_STc)[i])) {
    DS_STc[[i]] <- ifelse(!is.na(DS_STc[[i]]), 
                          ifelse(DS_STc[[i]] < 0.1 | DS_STc[[i]] > 4, NA, DS_STc[[i]]),
                          NA)
  }
}

#Anzahl der RT vor dem Filtern 
anzahl_RT_DS_STc_vor<- sum(!is.na(DS_STc[, grepl("^RT_STc", colnames(DS_STc))]))
anzahl_RT_DS_STc_vor 

# Spaltennamen ändern
colnames(DS_STc) <- gsub("\\s\\(s\\)$", "", colnames(DS_STc))

# Daten in das 'melted' Format umwandeln
DS_STc <- gather(DS_STc, key = "Spalte", value = "Reaktionszeit", -subjectID) %>%
  select(-Spalte)

# Daten nach der Nummer in der Spalte "subjectID" sortieren und NAs ignorieren
DS_STc <- DS_STc %>%
  mutate(subjectNum = str_extract(subjectID, "\\d+")) %>%
  arrange(as.numeric(subjectNum))

# Entfernen der Zwischenspalte "subjectNum"
DS_STc <- DS_STc %>%
  select(-subjectNum)

# Umbenennen der Spalte "Reaktionszeit" in "RT_STc"
colnames(DS_STc)[colnames(DS_STc) == "Reaktionszeit"] <- "RT_STc"

# Jitter-Boxplot der Reaktionszeiten vor dem Filtern der Ausreißer
ggplot(DS_STc, aes(x = "", y = RT_STc)) +
  geom_boxplot(fill = "lightgreen", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("Jitter-Boxplot DS_STc vor dem Filtern der Ausreißer") +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5))

# Ausreißer identifizieren
Q1 <- quantile(DS_STc$RT_STc, 0.25, na.rm = TRUE)
Q3 <- quantile(DS_STc$RT_STc, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
filtered_DS_STc <- DS_STc %>%
  mutate(RT_STc_filtered = ifelse(!(RT_STc < (Q1 - 1.5 * IQR) | RT_STc > (Q3 + 1.5 * IQR)), RT_STc, NA)) %>%
  select(subjectID, RT_STc)

# Anzahl der Ausreißer nach dem Filtern
anzahl_RT_DS_STc_nach <- sum(!is.na(filtered_DS_STc$RT_STc))
anzahl_RT_DS_STc_nach

# Jitter-Boxplot nach dem Filtern der Ausreißer
ggplot(filtered_DS_STc, aes(x = "", y = RT_STc)) +
  geom_boxplot(fill = "green", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("Jitter-Boxplot DS_STc nach dem Filtern der Ausreißer") +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5))

# Eindeutige subjectIDs extrahieren
unique_subjects <- unique(filtered_DS_STc$subjectID)

# Durchschnitt für jede subjectID berechnen und in all_means speichern
all_means <- data.frame(subjectID = unique_subjects, RT_DS_STc = NA)

for (i in 1:length(unique_subjects)) {
  sub <- unique_subjects[i]
  subset_data_DS_STc <- filtered_DS_STc[filtered_DS_STc$subjectID == sub, ]
  mean_value <- mean(subset_data_DS_STc$RT_STc, na.rm = TRUE)
  all_means$RT_DS_STc[i] <- mean_value
}

# Daten hochladen DS_STi
DS_STi <- read_csv("reaction_times_sti.csv")
colnames(DS_STi)[1] <- "subjectID"

# Reaktionszeiten in Sekunden umrechnen und entfernen der SubjectIDs >61
DS_STi <- DS_STi[DS_STi$subjectID >= "sub61", ]
DS_STi[, -1] <- lapply(DS_STi[, -1], function(x) ifelse(!is.na(x), x / 1000, x))

# Präfix "sub" entfernen und nur Zahlen behalten
DS_STi$subjectID <- substr(DS_STi$subjectID, 4, nchar(DS_STi$subjectID))

# Reaktionszeiten >0,1 und <4 herausfiltern
num_columns <- ncol(DS_STi)
for (i in 2:num_columns) {
  if (grepl("^RT_STi", colnames(DS_STi)[i])) {
    DS_STi[[i]] <- ifelse(!is.na(DS_STi[[i]]),
                          ifelse(DS_STi[[i]] < 0.1 | DS_STi[[i]] > 4, NA, DS_STi[[i]]),
                          NA)
  }
}

#Anzahl der RT vor dem Filtern 
anzahl_RT_DS_STi_vor <- sum(!is.na(DS_STi[, grepl("^RT_STi", colnames(DS_STi))]))
anzahl_RT_DS_STi_vor

# Spaltennamen ändern
colnames(DS_STi) <- gsub("\\s\\(s\\)$", "", colnames(DS_STi))

# Daten in das 'melted' Format umwandeln
DS_STi <- gather(DS_STi, key = "Spalte", value = "Reaktionszeit", -subjectID) %>%
  select(-Spalte)

# Daten nach der Nummer in der Spalte "subjectID" sortieren und NAs ignorieren
DS_STi <- DS_STi %>%
  mutate(subjectNum = str_extract(subjectID, "\\d+")) %>%
  arrange(as.numeric(subjectNum))

# Entfernen der Zwischenspalte "subjectNum"
DS_STi <- DS_STi %>%
  select(-subjectNum)

# Umbenennen der Spalte "Reaktionszeit" in "RT_STi"
colnames(DS_STi)[colnames(DS_STi) == "Reaktionszeit"] <- "RT_STi"

# Jitter-Boxplot der Reaktionszeiten vor dem Filtern der Ausreißer
ggplot(DS_STi, aes(x = "", y = RT_STi)) +
  geom_boxplot(fill = "lightgreen", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("Jitter-Boxplot DS_STi vor dem Filtern der Ausreißer") +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5))

# Ausreißer identifizieren
Q1 <- quantile(DS_STi$RT_STi, 0.25, na.rm = TRUE)
Q3 <- quantile(DS_STi$RT_STi, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
filtered_DS_STi <- DS_STi %>%
  mutate(RT_STi_filtered = ifelse(!(RT_STi < (Q1 - 1.5 * IQR) | RT_STi > (Q3 + 1.5 * IQR)), RT_STi, NA)) %>%
  select(subjectID, RT_STi)

# Anzahl der Ausreißer nach dem Filtern
anzahl_RT_DS_STi_nach <- sum(!is.na(filtered_DS_STi$RT_STi))
anzahl_RT_DS_STi_nach

# Jitter-Boxplot nach dem Filtern der Ausreißer
ggplot(filtered_DS_STi, aes(x = "", y = RT_STi)) +
  geom_boxplot(fill = "green", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("Jitter-Boxplot DS_STi nach dem Filtern der Ausreißer") +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5))

# Eindeutige subjectIDs extrahieren
unique_subjects <- unique(filtered_DS_STi$subjectID,)

# Durchschnitt für jede subjectID von STi berechnen und in all_means einfügen
for (sub in unique_subjects) {
  subset_data_DS_STi <- filtered_DS_STi[filtered_DS_STi$subjectID == sub, ]
  subset_data_DS_STi$RT_STi <- as.numeric(subset_data_DS_STi$RT_STi)  # Spalte "STi" in numerischen Datentyp umwandeln
  mean_value <- mean(subset_data_DS_STi$RT_STi, na.rm = TRUE)
  all_means$mean_STi[all_means$subjectID == sub] <- mean_value
}
# Spaltennamen in all_means umbenennen
colnames(all_means)[colnames(all_means) == "mean_STi"] <- "RT_DS_STi"

# Daten hochladen DS_DTc
DS_DTc <- read_csv("reaction_times_dtc.csv")
colnames(DS_DTc)[1] <- "subjectID"

# Reaktionszeiten in Sekunden umrechnen und entfernen der SubjectIDs >61
DS_DTc <- DS_DTc[DS_DTc$subjectID >= "sub61", ]
DS_DTc[, -1] <- lapply(DS_DTc[, -1], function(x) ifelse(!is.na(x), x / 1000, x))

# Präfix "sub" entfernen und nur Zahlen behalten
DS_DTc$subjectID <- substr(DS_DTc$subjectID, 4, nchar(DS_DTc$subjectID))

# Reaktionszeiten >0,1 und <4 herausfiltern
num_columns <- ncol(DS_DTc)
for (i in 2:num_columns) {
  if (grepl("^RT_DTc", colnames(DS_DTc)[i])) {
    DS_DTc[[i]] <- ifelse(!is.na(DS_DTc[[i]]),
                          ifelse(DS_DTc[[i]] < 0.1 | DS_DTc[[i]] > 4, NA, DS_DTc[[i]]),
                          NA)
  }
}

#Anzahl der RT vor dem Filtern 
anzahl_RT_DS_DTc_vor <- sum(!is.na(DS_DTc[, grepl("^RT_DTc", colnames(DS_DTc))]))
anzahl_RT_DS_DTc_vor

# Spaltennamen ändern
colnames(DS_DTc) <- gsub("\\s\\(s\\)$", "", colnames(DS_DTc))

# Daten in das 'melted' Format umwandeln
DS_DTc <- gather(DS_DTc, key = "Spalte", value = "Reaktionszeit", -subjectID) %>%
  select(-Spalte)

# Daten nach der Nummer in der Spalte "subjectID" sortieren und NAs ignorieren
DS_DTc <- DS_DTc %>%
  mutate(subjectNum = str_extract(subjectID, "\\d+")) %>%
  arrange(as.numeric(subjectNum))

# Entfernen der Zwischenspalte "subjectNum"
DS_DTc <- DS_DTc %>%
  select(-subjectNum)

# Umbenennen der Spalte "Reaktionszeit" in "RT_DTc"
colnames(DS_DTc)[colnames(DS_DTc) == "Reaktionszeit"] <- "RT_DTc"

# Jitter-Boxplot der Reaktionszeiten vor dem Filtern der Ausreißer
ggplot(DS_DTc, aes(x = "", y = RT_DTc)) +
  geom_boxplot(fill = "lightgreen", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("Jitter-Boxplot DS_DTc vor dem Filtern der Ausreißer") +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5))

# Ausreißer identifizieren
Q1 <- quantile(DS_DTc$RT_DTc, 0.25, na.rm = TRUE)
Q3 <- quantile(DS_DTc$RT_DTc, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
filtered_DS_DTc <- DS_DTc %>%
  mutate(RT_DTc_filtered = ifelse(!(RT_DTc < (Q1 - 1.5 * IQR) | RT_DTc > (Q3 + 1.5 * IQR)), RT_DTc, NA)) %>%
  select(subjectID, RT_DTc)

# Anzahl der Ausreißer nach dem Filtern
anzahl_RT_DS_DTc_nach <- sum(!is.na(filtered_DS_DTc$RT_DTc))
anzahl_RT_DS_DTc_nach

# Jitter-Boxplot nach dem Filtern der Ausreißer
ggplot(filtered_DS_DTc, aes(x = "", y = RT_DTc)) +
  geom_boxplot(fill = "green", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("Jitter-Boxplot DS_DTc nach dem Filtern der Ausreißer") +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5))

# Eindeutige subjectIDs extrahieren
unique_subjects <- unique(filtered_DS_DTc$subjectID)

# Durchschnitt für jede subjectID von DTc berechnen und in all_means einfügen
for (sub in unique_subjects) {
  subset_data_DS_DTc <- filtered_DS_DTc[filtered_DS_DTc$subjectID == sub, ]
  subset_data_DS_DTc$RT_DTc <- as.numeric(subset_data_DS_DTc$RT_DTc)
  mean_value <- mean(subset_data_DS_DTc$RT_DTc, na.rm = TRUE)
  all_means$mean_DTc[all_means$subjectID == sub] <- mean_value
}

# Spaltennamen in all_means umbenennen
colnames(all_means)[colnames(all_means) == "mean_DTc"] <- "RT_DS_DTc"

# Daten hochladen DS_DTi
DS_DTi <- read_csv("reaction_times_dti.csv")
colnames(DS_DTi)[1] <- "subjectID"

# Reaktionszeiten in Sekunden umrechnen und entfernen der SubjectIDs >61
DS_DTi <- DS_DTi[DS_DTi$subjectID >= "sub61", ]
DS_DTi[, -1] <- lapply(DS_DTi[, -1], function(x) ifelse(!is.na(x), x / 1000, x))

# Präfix "sub" entfernen und nur Zahlen behalten
DS_DTi$subjectID <- substr(DS_DTi$subjectID, 4, nchar(DS_DTi$subjectID))

# Reaktionszeiten >0,1 und <4 herausfiltern
num_columns <- ncol(DS_DTi)
for (i in 2:num_columns) {
  if (grepl("^RT_DTi", colnames(DS_DTi)[i])) {
    DS_DTi[[i]] <- ifelse(!is.na(DS_DTi[[i]]),
                          ifelse(DS_DTi[[i]] < 0.1 | DS_DTi[[i]] > 4, NA, DS_DTi[[i]]),
                          NA)
  }
}

#Anzahl der RT vor dem Filtern 
anzahl_RT_DS_DTi_vor <- sum(!is.na(DS_DTi[, grepl("^RT_DTi", colnames(DS_DTi))]))
anzahl_RT_DS_DTi_vor

# Spaltennamen ändern
colnames(DS_DTi) <- gsub("\\s\\(s\\)$", "", colnames(DS_DTi))

# Daten in das 'melted' Format umwandeln
DS_DTi <- gather(DS_DTi, key = "Spalte", value = "Reaktionszeit", -subjectID) %>%
  select(-Spalte)

# Daten nach der Nummer in der Spalte "subjectID" sortieren und NAs ignorieren
DS_DTi <- DS_DTi %>%
  mutate(subjectNum = str_extract(subjectID, "\\d+")) %>%
  arrange(as.numeric(subjectNum))

# Entfernen der Zwischenspalte "subjectNum"
DS_DTi <- DS_DTi %>%
  select(-subjectNum)

# Umbenennen der Spalte "Reaktionszeit" in "RT_DTi"
colnames(DS_DTi)[colnames(DS_DTi) == "Reaktionszeit"] <- "RT_DTi"

# Jitter-Boxplot der Reaktionszeiten vor dem Filtern der Ausreißer
ggplot(DS_DTi, aes(x = "", y = RT_DTi)) +
  geom_boxplot(fill = "lightgreen", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("Jitter-Boxplot DS_DTi vor dem Filtern der Ausreißer") +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5))

# Ausreißer identifizieren
Q1 <- quantile(DS_DTi$RT_DTi, 0.25, na.rm = TRUE)
Q3 <- quantile(DS_DTi$RT_DTi, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
filtered_DS_DTi <- DS_DTi %>%
  mutate(RT_DTi_filtered = ifelse(!(RT_DTi < (Q1 - 1.5 * IQR) | RT_DTi > (Q3 + 1.5 * IQR)), RT_DTi, NA)) %>%
  select(subjectID, RT_DTi)

# Anzahl der Ausreißer nach dem Filtern
anzahl_RT_DS_DTi_nach <- sum(!is.na(filtered_DS_DTi$RT_DTi))
anzahl_RT_DS_DTi_nach

# Jitter-Boxplot nach dem Filtern der Ausreißer
ggplot(filtered_DS_DTi, aes(x = "", y = RT_DTi)) +
  geom_boxplot(fill = "green", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("Jitter-Boxplot DS_DTi nach dem Filtern der Ausreißer") +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5))

# Eindeutige subjectIDs extrahieren
unique_subjects <- unique(filtered_DS_DTc$subjectID)

# Durchschnitt für jede subjectID von DTi berechnen und in all_means einfügen
for (sub in unique_subjects) {
  subset_data_DS_DTi <- filtered_DS_DTi[filtered_DS_DTi$subjectID == sub, ]
  subset_data_DS_DTi$RT_DTi <- as.numeric(subset_data_DS_DTi$RT_DTi)
  mean_value <- mean(subset_data_DS_DTi$RT_DTi, na.rm = TRUE)
  all_means$mean_DTi[all_means$subjectID == sub] <- mean_value
}

# Spaltennamen in all_means umbenennen
colnames(all_means)[colnames(all_means) == "mean_DTi"] <- "RT_DS_DTi"


### Labordaten ------------------------------------------------------###

# Dateipfad zur TSV-Datei
dateipfad <- "C:/Users/nikla/Documents/Data/MA/Labordaten.tsv"

# Einlesen der TSV-Datei
Labordaten <- read.table(dateipfad, sep = "\t", header = TRUE)

# Spalte in numerische Variable ändern
Labordaten$rt_man <- as.numeric(as.character(Labordaten$rt_man))

# gefiltertes Dataframe mit relevanten Daten 
gefiltertes_df <- Labordaten %>%
  select(subjectID, mapping, STDT, task, rt_voc, rt_man, timepoint.x, error) %>%
  filter(subjectID >= 60, STDT != "TS", timepoint.x != "Practice")

# Millisekunden in Sekunden umwandeln für die Spalten rt_voc und rt_man (unter Berücksichtigung von NA-Werten)
gefiltertes_df$rt_voc <- ifelse(!is.na(gefiltertes_df$rt_voc), gefiltertes_df$rt_voc / 1000, NA)
gefiltertes_df$rt_man <- ifelse(!is.na(gefiltertes_df$rt_man), gefiltertes_df$rt_man / 1000, NA)

# Werte unter 0,1s und über 4s als NA ändern
gefiltertes_df$rt_voc[gefiltertes_df$rt_voc < 0.1 | gefiltertes_df$rt_voc > 4] <- NA
gefiltertes_df$rt_man[gefiltertes_df$rt_man < 0.1 | gefiltertes_df$rt_man > 4] <- NA

# Filtern der Labordaten für L_STc
filtered_L_STc <- gefiltertes_df[gefiltertes_df$mapping == "compatible" &
                                   gefiltertes_df$STDT == "ST" &
                                   gefiltertes_df$timepoint.x == "Procedure" &
                                   (gefiltertes_df$task == "vm" | gefiltertes_df$task == "av") & 
                                   gefiltertes_df$subjectID >= 61 , ]
# Kombinieren der Reaktionszeit-Spalten
filtered_L_STc$RT_L_STc <- ifelse(!is.na(filtered_L_STc$rt_man), filtered_L_STc$rt_man, filtered_L_STc$rt_voc)
filtered_L_STc <- filtered_L_STc %>% select(-rt_voc, -rt_man)

# Anzahl der Ausreißer nach dem Filtern 
anzahl_RT_L_STc_vor <- sum(!is.na(filtered_L_STc$RT_L_STc))
anzahl_RT_L_STc_vor

# Jitter-Boxplot vor dem Filtern der Ausreißer
ggplot(filtered_L_STc, aes(x = "", y = RT_L_STc)) +
  geom_boxplot(fill = "lightgray", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("RT_L_STc - vor dem Filtern der Ausreißer") + 
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, by = 0.3))

# Ausreißer identifizieren 
Q1 <- quantile(filtered_L_STc$RT_L_STc, 0.25, na.rm = TRUE)
Q3 <- quantile(filtered_L_STc$RT_L_STc, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
filtered_L_STc <- filtered_L_STc[!(filtered_L_STc$RT_L_STc < (Q1 - 1.5 * IQR) | filtered_L_STc$RT_L_STc > (Q3 + 1.5 * IQR)), ]

# Anzahl der Ausreißer nach dem Filtern 
anzahl_RT_L_STc_nach <- sum(!is.na(filtered_L_STc$RT_L_STc))
anzahl_RT_L_STc_nach

# Jitter-Boxplot nach dem Filtern der Ausreißer
ggplot(filtered_L_STc, aes(x = "", y = RT_L_STc)) +
  geom_boxplot(fill = "lightgray", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("RT_L_STc - nach dem Filtern der Ausreißer") + 
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, by = 0.3))

# Spalte für die Reaktionszeiten im all_means-Dataframe erstellen
all_means$RT_L_STc <- NA

# Schleife durch alle Zeilen im all_means-Dataframe
for (i in 1:nrow(all_means)) {
  # Extrahieren der subjectID
  subjectID <- all_means$subjectID[i]
  
  # Mittelwert der Reaktionszeit berechnen
  mean_RT <- mean(filtered_L_STc[filtered_L_STc$subjectID == subjectID, "RT_L_STc"], na.rm = TRUE)
  
  # Spaltenname für die Labordaten
  column_name <- "RT_L_STc"
  
  # Zuweisen des Mittelwerts in das all_means-Dataframe
  all_means[i, column_name] <- mean_RT
}

# Filtern der Labordaten für L_STi
filtered_L_STi <- gefiltertes_df[gefiltertes_df$mapping == "incompatible" &
                                   gefiltertes_df$STDT == "ST" &
                                   gefiltertes_df$timepoint.x == "Procedure" &
                                   (gefiltertes_df$task == "am" | gefiltertes_df$task == "vv") & 
                                   gefiltertes_df$subjectID >= 61 , ]

# Kombinieren der Reaktionszeit-Spalten
filtered_L_STi$RT_L_STi <- ifelse(!is.na(filtered_L_STi$rt_man), filtered_L_STi$rt_man, filtered_L_STi$rt_voc)
filtered_L_STi <- filtered_L_STi %>% select(-rt_voc, -rt_man)

# Anzahl der Ausreißer vor dem Filtern 
anzahl_RT_L_STi_vor <- sum(!is.na(filtered_L_STi$RT_L_STi))
anzahl_RT_L_STi_vor

# Jitter-Boxplot vor dem Filtern der Ausreißer
ggplot(filtered_L_STi, aes(x = "", y = RT_L_STi)) +
  geom_boxplot(fill = "lightgray", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("RT_L_STi - vor dem Filtern der Ausreißer") + 
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, by = 0.3))

# Ausreißer identifizieren 
Q1 <- quantile(filtered_L_STi$RT_L_STi, 0.25, na.rm = TRUE)
Q3 <- quantile(filtered_L_STi$RT_L_STi, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
filtered_L_STi <- filtered_L_STi[!(filtered_L_STi$RT_L_STi < (Q1 - 1.5 * IQR) | filtered_L_STi$RT_L_STi > (Q3 + 1.5 * IQR)), ]

# Anzahl der Ausreißer nach dem Filtern 
anzahl_RT_L_STi_nach <- sum(!is.na(filtered_L_STi$RT_L_STi))
anzahl_RT_L_STi_nach

# Jitter-Boxplot nach dem Filtern der Ausreißer
ggplot(filtered_L_STi, aes(x = "", y = RT_L_STi)) +
  geom_boxplot(fill = "lightgray", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("RT_L_STi - nach dem Filtern der Ausreißer") + 
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, by = 0.3))

# Spalte für die Reaktionszeiten im all_means-Dataframe erstellen
all_means$RT_L_STi <- NA

# Schleife durch alle Zeilen im all_means-Dataframe
for (i in 1:nrow(all_means)) {
  # Extrahieren der subjectID
  subjectID <- all_means$subjectID[i]
  
  # Mittelwert der Reaktionszeit berechnen
  mean_RT <- mean(filtered_L_STi[filtered_L_STi$subjectID == subjectID, "RT_L_STi"], na.rm = TRUE)
  
  # Spaltenname für die Labordaten
  column_name <- "RT_L_STi"
  
  # Zuweisen des Mittelwerts in das all_means-Dataframe
  all_means[i, column_name] <- mean_RT
}

# Filtern der Labordaten für DTc
filtered_L_DTc <- gefiltertes_df[gefiltertes_df$mapping == "compatible" &
                                   gefiltertes_df$STDT == "DT" &
                                   gefiltertes_df$timepoint.x == "Procedure" &
                                   gefiltertes_df$task == "dtc" &
                                   gefiltertes_df$subjectID >= 61 , ]

# Kombinieren der Reaktionszeit-Spalten
filtered_L_DTc$RT_L_DTc <- ifelse(!is.na(filtered_L_DTc$rt_man) & !is.na(filtered_L_DTc$rt_voc),
                                  (filtered_L_DTc$rt_man + filtered_L_DTc$rt_voc) / 2,
                                  ifelse(!is.na(filtered_L_DTc$rt_man), filtered_L_DTc$rt_man, filtered_L_DTc$rt_voc))
filtered_L_DTc <- filtered_L_DTc %>% select(-rt_voc, -rt_man)

# Anzahl der Ausreißer vor dem Filtern 
anzahl_RT_L_DTc_vor <- sum(!is.na(filtered_L_DTc$RT_L_DTc))
anzahl_RT_L_DTc_vor

# Jitter-Boxplot vor dem Filtern der Ausreißer
ggplot(filtered_L_DTc, aes(x = "", y = RT_L_DTc)) +
  geom_boxplot(fill = "lightgray", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("RT_L_DTc - vor dem Filtern der Ausreißer") + 
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5))

# Ausreißer identifizieren 
Q1 <- quantile(filtered_L_DTc$RT_L_DTc, 0.25, na.rm = TRUE)
Q3 <- quantile(filtered_L_DTc$RT_L_DTc, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
filtered_L_DTc <- filtered_L_DTc[!(filtered_L_DTc$RT_L_DTc < (Q1 - 1.5 * IQR) | filtered_L_DTc$RT_L_DTc > (Q3 + 1.5 * IQR)), ]

# Anzahl der Ausreißer nach dem Filtern 
anzahl_RT_L_DTc_nach <- sum(!is.na(filtered_L_DTc$RT_L_DTc))
anzahl_RT_L_DTc_nach

# Jitter-Boxplot nach dem Filtern der Ausreißer
ggplot(filtered_L_DTc, aes(x = "", y = RT_L_DTc)) +
  geom_boxplot(fill = "lightgray", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("RT_L_DTc - nach dem Filtern der Ausreißer") + 
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5))

# Spalte für die Reaktionszeiten im all_means-Dataframe erstellen
all_means$RT_L_DTc <- NA

# Schleife durch alle Zeilen im all_means-Dataframe
for (i in 1:nrow(all_means)) {
  # Extrahieren der subjectID
  subjectID <- all_means$subjectID[i]
  
  # Mittelwert der Reaktionszeit berechnen
  mean_RT <- mean(filtered_L_DTc[filtered_L_DTc$subjectID == subjectID, "RT_L_DTc"], na.rm = TRUE)
  
  # Spaltenname für die Labordaten
  column_name <- "RT_L_DTc"
  
  # Hinzufügen des Mittelwerts zur entsprechenden Zeile und Spalte
  all_means[i, column_name] <- mean_RT
}

# Filtern der Labordaten für DTi
filtered_L_DTi <- gefiltertes_df[gefiltertes_df$mapping == "incompatible" &
                                   gefiltertes_df$STDT == "DT" &
                                   gefiltertes_df$timepoint.x == "Procedure" &
                                   gefiltertes_df$task == "dti" &
                                   gefiltertes_df$subjectID >= 61 , ]

# Kombinieren der Reaktionszeit-Spalten
filtered_L_DTi$RT_L_DTi <- ifelse(!is.na(filtered_L_DTi$rt_man) & !is.na(filtered_L_DTi$rt_voc),
                                  (filtered_L_DTi$rt_man + filtered_L_DTi$rt_voc) / 2,
                                  ifelse(!is.na(filtered_L_DTi$rt_man), filtered_L_DTi$rt_man, filtered_L_DTi$rt_voc))
filtered_L_DTi <- filtered_L_DTi %>% select(-rt_voc, -rt_man)

# Anzahl der Ausreißer vor dem Filtern 
anzahl_RT_L_DTi_vor <- sum(!is.na(filtered_L_DTi$RT_L_DTi))
anzahl_RT_L_DTi_vor

# Jitter-Boxplot vor dem Filtern der Ausreißer
ggplot(filtered_L_DTi, aes(x = "", y = RT_L_DTi)) +
  geom_boxplot(fill = "lightgray", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("RT_L_DTi - vor dem Filtern der Ausreißer") + 
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5))

# Ausreißer identifizieren 
Q1 <- quantile(filtered_L_DTi$RT_L_DTi, 0.25, na.rm = TRUE)
Q3 <- quantile(filtered_L_DTi$RT_L_DTi, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
filtered_L_DTi <- filtered_L_DTi[!(filtered_L_DTi$RT_L_DTi < (Q1 - 1.5 * IQR) | filtered_L_DTi$RT_L_DTi > (Q3 + 1.5 * IQR)), ]

# Anzahl der Ausreißer nach dem Filtern 
anzahl_RT_L_DTi_nach <- sum(!is.na(filtered_L_DTi$RT_L_DTi))
anzahl_RT_L_DTi_nach

# Jitter-Boxplot nach dem Filtern der Ausreißer
ggplot(filtered_L_DTi, aes(x = "", y = RT_L_DTi)) +
  geom_boxplot(fill = "lightgray", color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "", y = "Reaktionszeit") +
  ggtitle("RT_L_DTi - nach dem Filtern der Ausreißer") + 
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 0.5))

# Spalte für die Reaktionszeiten im all_means-Dataframe erstellen
all_means$RT_L_DTi <- NA

# Schleife durch alle Zeilen im all_means-Dataframe
for (i in 1:nrow(all_means)) {
  # Extrahieren der subjectID
  subjectID <- all_means$subjectID[i]
  
  # Mittelwert der Reaktionszeit berechnen
  mean_RT <- mean(filtered_L_DTi[filtered_L_DTi$subjectID == subjectID, "RT_L_DTi"], na.rm = TRUE)
  
  # Spaltenname für die Labordaten
  column_name <- "RT_L_DTi"
  
  # Hinzufügen des Mittelwerts zur entsprechenden Zeile und Spalte
  all_means[i, column_name] <- mean_RT
}

##------------------------------------------------------------------------##

# Ausgewählte Spalten
selected_columns <- c("RT_DS_STi", "RT_DS_STc", "RT_DS_DTi", "RT_DS_DTc", "RT_L_STi", "RT_L_STc", "RT_L_DTi", "RT_L_DTc")

# Berechnung der Spaltenmittelwerte
column_means <- colMeans(all_means[, selected_columns], na.rm = TRUE)

# Erstellen des finalen DataFrames mit ausgewählten Spaltennamen als Zeile
final_df <- data.frame(t(column_means))

# Umbenennen der Spalten
colnames(final_df) <- selected_columns

# Daten in ein geeignetes Format bringen
plot_data_ST <- final_df[, c("RT_L_STc", "RT_L_STi", "RT_DS_STc", "RT_DS_STi")]
plot_data_ST <- stack(plot_data_ST)

# Barplot erstellen ST
ggplot(plot_data_ST, aes(x = ind, y = values, fill = ind)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("gray", "lightgray", "green", "lightgreen")) +
  labs(x = "Task", y = "Reaction Time", title = "Barplot of ST") +
  theme_minimal() +
  theme(legend.position = "none")

# Daten in ein geeignetes Format bringen
plot_data_DT <- final_df[, c("RT_L_DTc", "RT_L_DTi", "RT_DS_DTc", "RT_DS_DTi")]
plot_data_DT <- stack(plot_data_DT)

# Barplot erstellen DT
ggplot(plot_data_DT, aes(x = ind, y = values, fill = ind)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("gray", "lightgray", "green", "lightgreen")) +
  labs(x = "Task", y = "Reaction Time", title = "Barplot of DT") +
  theme_minimal() +
  theme(legend.position = "none")

# Daten in ein geeignetes Format bringen
plot_data_L <- final_df[, c("RT_L_STc", "RT_L_STi", "RT_L_DTc", "RT_L_DTi")]
plot_data_L <- stack(plot_data_L)

# Barplot erstellen L
ggplot(plot_data_L, aes(x = ind, y = values, fill = ind)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("gray", "lightgray", "green", "lightgreen")) +
  labs(x = "Task", y = "Reaction Time", title = "Barplot of L") +
  theme_minimal() +
  theme(legend.position = "none")

# Daten in ein geeignetes Format bringen
plot_data_DS <- final_df[, c("RT_DS_STc", "RT_DS_STi", "RT_DS_DTc", "RT_DS_DTi")]
plot_data_DS <- stack(plot_data_DS)

# Barplot erstellen DS
ggplot(plot_data_DS, aes(x = ind, y = values, fill = ind)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("gray", "lightgray", "green", "lightgreen")) +
  labs(x = "Task", y = "Reaction Time", title = "Barplot of DS") +
  theme_minimal() +
  theme(legend.position = "none")

# MCE berechnen

# 1a Berechnung MCEoverall, Version Mittelwerte voneinander abziehen
Mcomp <- c(final_df$RT_DS_STc, final_df$RT_DS_DTc, final_df$RT_L_STc, final_df$RT_L_DTc)
mean_comp <- mean(Mcomp)
Mincomp <- c(final_df$RT_DS_STi, final_df$RT_DS_DTi, final_df$RT_L_STi, final_df$RT_L_DTi)
mean_incomp <- mean(Mincomp)
MCEoverall <- mean_comp - mean_incomp
MCE_df <- data.frame(MCEoverall)

# 1b Berechnung MCEoverall, Version Mittelwerte addieren und danach abziehen
mean_comp <- mean(final_df$RT_DS_STc + final_df$RT_DS_DTc + final_df$RT_L_STc + final_df$RT_L_DTc)
mean_incomp <- mean(final_df$RT_DS_STi + final_df$RT_DS_DTi + final_df$RT_L_STi + final_df$RT_L_DTi)
MCEoverall <- mean_comp - mean_incomp

# 2 MCE vom Labor berechnen 

MCE_L_comp <- c(final_df$RT_L_STc,final_df$RT_L_DTc) 
mean_MCE_L_comp <- mean(MCE_L_comp)
MCE_L_incomp <- c(final_df$RT_L_STi, final_df$RT_L_DTi)
mean_MCE_L_incomp <- mean(MCE_L_incomp)
MCE_L <- mean_MCE_L_comp - mean_MCE_L_incomp
MCE_df$MCE_L <- MCE_L

# 3 MCE vom DS berechnen 

MCE_DS_comp <- c(final_df$RT_DS_STc,final_df$RT_DS_DTc) 
mean_MCE_DS_comp <- mean(MCE_DS_comp)
MCE_DS_incomp <- c(final_df$RT_DS_STi, final_df$RT_DS_DTi)
mean_MCE_DS_incomp <- mean(MCE_DS_incomp)
MCE_DS <- mean_MCE_DS_comp - mean_MCE_DS_incomp
MCE_df$MCE_DS <- MCE_DS

# 4 MCE von Labor ST 

MCE_L_ST <- mean(final_df$RT_L_STc - final_df$RT_L_STi)
MCE_df$MCE_L_ST <- MCE_L_ST

# 5 MCE von DS ST 

MCE_DS_ST <- mean(final_df$RT_DS_STc - final_df$RT_DS_STi)
MCE_df$MCE_DS_ST <- MCE_DS_ST

# 6 MCE von Labor DT

MCE_L_DT <- mean(final_df$RT_L_DTc - final_df$RT_L_DTi)
MCE_df$MCE_L_DT <- MCE_L_DT

# 7 MCE von DS DT 

MCE_DS_DT <- mean(final_df$RT_DS_DTc - final_df$RT_DS_DTi)
MCE_df$MCE_DS_DT <- MCE_DS_DT


# Erstelle ein leeres DataFrame für die Ergebnisse
MCE <- data.frame(subjectID = numeric(),
                  MCE = numeric(),
                  Umgebung = character(),
                  Aufgabe = character(),
                  stringsAsFactors = FALSE)

# Schleife durch jede subjectID im all_means-DataFrame
for (i in 1:nrow(all_means)) {
  subjectID <- all_means$subjectID[i]
  
  # Berechne die MCE-Werte für jede Kombination von Umgebung und Aufgabe
  MCE_DS_ST <- all_means$RT_DS_STc[i] - all_means$RT_DS_STi[i]
  MCE_DS_DT <- all_means$RT_DS_DTc[i] - all_means$RT_DS_DTi[i]
  MCE_L_ST <- all_means$RT_L_STc[i] - all_means$RT_L_STi[i]
  MCE_L_DT <- all_means$RT_L_DTc[i] - all_means$RT_L_DTi[i]
  
  # Füge die Ergebnisse der MCE-Tabelle hinzu
  MCE <- rbind(MCE, data.frame(subjectID = subjectID,
                               MCE = c(MCE_DS_ST, MCE_DS_DT, MCE_L_ST, MCE_L_DT),
                               Umgebung = c("DS", "DS", "L", "L"),
                               Aufgabe = c("ST", "DT", "ST", "DT"),
                               stringsAsFactors = FALSE))
}

#Deskriptive Statistik 
describeBy(MCE ~ Aufgabe + Umgebung, data = MCE)
boxplot(MCE ~ Umgebung + Aufgabe, data = MCE)

# Homogenität der Varianzen 
leveneTest(MCE ~ Umgebung * Aufgabe, data = MCE)

## zweifaktorielle ANOVA ##
zweianova <- aov(MCE ~ Umgebung + Aufgabe + Umgebung * Aufgabe, data = MCE)
summary(zweianova)

# Normalverteilung Residuen 
plot(zweianova,2)

#TukeyTest, post-hoc Test
TukeyHSD(zweianova)

# Boxplot der Gruppen
ggplot(MCE, aes(x = Umgebung, y = MCE, fill = Aufgabe)) +
  geom_boxplot() +
  labs(x = "Umgebung", y = "MCE (Sek)", fill = "Aufgabe") +
  scale_fill_manual(values = c("white", "gray"), labels = c("DT", "ST")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

# Erstellung des Interaktionsplots mit beschrifteten Faktorstufen und Legende außerhalb der Abbildung
interaction.plot(MCE_cleaned$Umgebung, MCE_cleaned$Aufgabe, MCE_cleaned$MCE,
                 type = "l", col = c("blue", "orange"),
                 xlab = "Aufgabe", ylab = "MCE (Sek)", trace.label = "Umgebung",
                 main = "Interaktionsplot Umgebung x Aufgabe",
                 lwd = 1, pch = 16, cex = 1.2,
                 col.lab = "black", col.main = "black",
                 col.axis = "black", col.sub = "black",
                 font.main = 1, font.lab = 1, font.axis = 1,
                 ylim = c(min(MCE_cleaned$MCE), max(MCE_cleaned$MCE)))



# Plot der Daten aus MCE
ggplot(MCE, aes(x = Umgebung, y = MCE, color = Aufgabe)) +
  geom_point() +
  labs(x = "Umgebung", y = "MCE (Sek)", color = "Aufgabe") +
  scale_color_manual(values = c("blue", "orange"), labels = c("DT", "ST")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))





