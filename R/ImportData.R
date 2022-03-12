## Wczytywanie danych ----

### dane Taiwan ----
data <- read_csv("dane/data.csv")

## dane Słowacja ----
bankrupt_agriculture_13_year_10_11_12 <- read_delim("dane/j89csb932y-2/bankrupt_agriculture_13_year_10_11_12.csv", 
                                                    ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                                grouping_mark = ""), 
                                                    col_types = cols(X1 = col_skip()), trim_ws = TRUE)
bankrupt_agriculture_14_year_11_12_13 <- read_delim("dane/j89csb932y-2/bankrupt_agriculture_14_year_11_12_13.csv", 
                                                    ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                                    trim_ws = TRUE)
bankrupt_agriculture_15_year_12_13_14 <- read_delim("dane/j89csb932y-2/bankrupt_agriculture_15_year_12_13_14.csv", 
                                                    ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                                    trim_ws = TRUE)
bankrupt_agriculture_16_year_13_14_15 <- read_delim("dane/j89csb932y-2/bankrupt_agriculture_16_year_13_14_15.csv", 
                                                    ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                                    trim_ws = TRUE)
bankrupt_construction_13_year_10_11_12 <- read_delim("dane/j89csb932y-2/bankrupt_construction_13_year_10_11_12.csv", 
                                                     ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                                     trim_ws = TRUE)
bankrupt_construction_14_year_11_12_13 <- read_delim("dane/j89csb932y-2/bankrupt_construction_14_year_11_12_13.csv", 
                                                     ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                                     trim_ws = TRUE)
bankrupt_construction_15_year_12_13_14 <- read_delim("dane/j89csb932y-2/bankrupt_construction_15_year_12_13_14.csv", 
                                                     ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                                     trim_ws = TRUE)
bankrupt_construction_16_year_13_14_15 <- read_delim("dane/j89csb932y-2/bankrupt_construction_16_year_13_14_15.csv", 
                                                     ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                                     trim_ws = TRUE)
bankrupt_manufacture_13_year_10_11_12 <- read_delim("dane/j89csb932y-2/bankrupt_manufacture_13_year_10_11_12.csv", 
                                                    ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                                    trim_ws = TRUE)
bankrupt_manufacture_14_year_11_12_13 <- read_delim("dane/j89csb932y-2/bankrupt_manufacture_14_year_11_12_13.csv", 
                                                    ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                                    trim_ws = TRUE)
bankrupt_manufacture_15_year_12_13_14 <- read_delim("dane/j89csb932y-2/bankrupt_manufacture_15_year_12_13_14.csv", 
                                                    ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                                    trim_ws = TRUE)
bankrupt_manufacture_16_year_13_14_15 <- read_delim("dane/j89csb932y-2/bankrupt_manufacture_16_year_13_14_15.csv", 
                                                    ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                                    trim_ws = TRUE)
bankrupt_retail_13_year_10_11_12 <- read_delim("dane/j89csb932y-2/bankrupt_retail_13_year_10_11_12.csv", 
                                               ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                               trim_ws = TRUE)
bankrupt_retail_14_year_11_12_13 <- read_delim("dane/j89csb932y-2/bankrupt_retail_14_year_11_12_13.csv", 
                                               ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                               trim_ws = TRUE)
bankrupt_retail_15_year_12_13_14 <- read_delim("dane/j89csb932y-2/bankrupt_retail_15_year_12_13_14.csv", 
                                               ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                               trim_ws = TRUE)
bankrupt_retail_16_year_13_14_15 <- read_delim("dane/j89csb932y-2/bankrupt_retail_16_year_13_14_15.csv", 
                                               ";", escape_double = FALSE, locale = locale(decimal_mark = ",",          grouping_mark = ""), col_types = cols(X1 = col_skip()), 
                                               trim_ws = TRUE)
nonbankrupt_agriculture_13_year_10_11_12 <- read_csv("dane/j89csb932y-2/nonbankrupt_agriculture_13_year_10_11_12.csv")
nonbankrupt_agriculture_14_year_11_12_13 <- read_csv("dane/j89csb932y-2/nonbankrupt_agriculture_14_year_11_12_13.csv")
nonbankrupt_agriculture_15_year_12_13_14 <- read_csv("dane/j89csb932y-2/nonbankrupt_agriculture_15_year_12_13_14.csv")
nonbankrupt_agriculture_16_year_13_14_15 <- read_csv("dane/j89csb932y-2/nonbankrupt_agriculture_16_year_13_14_15.csv")
nonbankrupt_construction_13_year_10_11_12 <- read_csv("dane/j89csb932y-2/nonbankrupt_construction_13_year_10_11_12.csv")
nonbankrupt_construction_14_year_11_12_13 <- read_csv("dane/j89csb932y-2/nonbankrupt_construction_14_year_11_12_13.csv")
nonbankrupt_construction_15_year_12_13_14 <- read_csv("dane/j89csb932y-2/nonbankrupt_construction_15_year_12_13_14.csv")
nonbankrupt_construction_16_year_13_14_15 <- read_csv("dane/j89csb932y-2/nonbankrupt_construction_16_year_13_14_15.csv")
nonbankrupt_manufacture_13_year_10_11_12 <- read_csv("dane/j89csb932y-2/nonbankrupt_manufacture_13_year_10_11_12.csv")
nonbankrupt_manufacture_14_year_11_12_13 <- read_csv("dane/j89csb932y-2/nonbankrupt_manufacture_14_year_11_12_13.csv")
nonbankrupt_manufacture_15_year_12_13_14 <- read_csv("dane/j89csb932y-2/nonbankrupt_manufacture_15_year_12_13_14.csv")
nonbankrupt_manufacture_16_year_13_14_15 <- read_csv("dane/j89csb932y-2/nonbankrupt_manufacture_16_year_13_14_15.csv")
nonbankrupt_retail_13_year_10_11_12 <- read_csv("dane/j89csb932y-2/nonbankrupt_retail_13_year_10_11_12.csv")
nonbankrupt_retail_14_year_11_12_13 <- read_csv("dane/j89csb932y-2/nonbankrupt_retail_14_year_11_12_13.csv")
nonbankrupt_retail_15_year_12_13_14 <- read_csv("dane/j89csb932y-2/nonbankrupt_retail_15_year_12_13_14.csv")
nonbankrupt_retail_16_year_13_14_15 <- read_csv("dane/j89csb932y-2/nonbankrupt_retail_16_year_13_14_15.csv")
dataSkBank1<-rbind(bankrupt_agriculture_13_year_10_11_12, bankrupt_construction_13_year_10_11_12, bankrupt_manufacture_13_year_10_11_12,bankrupt_retail_13_year_10_11_12)
dataSkBank1$class<-1

dataSkBank2<-rbind(bankrupt_agriculture_14_year_11_12_13, bankrupt_construction_14_year_11_12_13, bankrupt_manufacture_14_year_11_12_13,bankrupt_retail_14_year_11_12_13)
dataSkBank2$class<-1

dataSkBank3<-rbind(bankrupt_agriculture_15_year_12_13_14, bankrupt_construction_15_year_12_13_14, bankrupt_manufacture_15_year_12_13_14,bankrupt_retail_15_year_12_13_14)
dataSkBank3$class<-1

dataSkBank4<-rbind(bankrupt_agriculture_16_year_13_14_15, bankrupt_construction_16_year_13_14_15, bankrupt_manufacture_16_year_13_14_15, bankrupt_retail_16_year_13_14_15)
dataSkBank4$class<-1

dataSknon1<-rbind(nonbankrupt_agriculture_13_year_10_11_12, nonbankrupt_construction_13_year_10_11_12, nonbankrupt_manufacture_13_year_10_11_12, nonbankrupt_retail_13_year_10_11_12)
dataSknon1$class<-0

dataSknon2<-rbind(nonbankrupt_agriculture_14_year_11_12_13, nonbankrupt_construction_14_year_11_12_13, nonbankrupt_manufacture_14_year_11_12_13,nonbankrupt_retail_14_year_11_12_13)
dataSknon2$class<-0

dataSknon3<-rbind(nonbankrupt_agriculture_15_year_12_13_14, nonbankrupt_construction_15_year_12_13_14, nonbankrupt_manufacture_15_year_12_13_14,nonbankrupt_retail_15_year_12_13_14)
dataSknon3$class<-0

dataSknon4<-rbind(nonbankrupt_agriculture_16_year_13_14_15, nonbankrupt_construction_16_year_13_14_15, nonbankrupt_manufacture_16_year_13_14_15, nonbankrupt_retail_16_year_13_14_15)
dataSknon4$class<-0

dataSk1<-rbind(dataSkBank1, dataSknon1)

wskazniki<-paste("Wsk", seq(1:21), sep = "")
wskazniki1<-paste(wskazniki,"_1", sep="")
wskazniki2<-paste(wskazniki,"_2", sep="")
wskazniki3<-paste(wskazniki,"_3", sep="")
dataSk1<-as.data.frame(dataSk1)


colnames(dataSk1)<-c(wskazniki1, wskazniki2, wskazniki3, "class")

dataSk2<-rbind(dataSkBank2, dataSknon2)
colnames(dataSk2)<-c(wskazniki1, wskazniki2, wskazniki3, "class")
dataSk2<-as.data.frame(dataSk2)

dataSk3<-rbind(dataSkBank3, dataSknon3)
colnames(dataSk3)<-c(wskazniki1, wskazniki2, wskazniki3, "class")
dataSk3<-as.data.frame(dataSk3)

dataSk4<-rbind(dataSkBank4, dataSknon4)
colnames(dataSk4)<-c(wskazniki1, wskazniki2, wskazniki3, "class")
dataSk4<-as.data.frame(dataSk4)


dataSk<-rbind(dataSk1, dataSk2, dataSk3, dataSk4)


## dane POLSKA ----
# bankructwo po 5 latach
X1year <- read_csv("dane/1year.csv", col_types = cols(Attr1 = col_double(), Attr2 = col_double(),
                                                      Attr3 = col_double(), Attr4 = col_double(),
                                                      Attr5 = col_double(), Attr6 = col_double(),
                                                      Attr7 = col_double(), Attr8 = col_double(),
                                                      Attr9 = col_double(), Attr10 = col_double(),
                                                      Attr11 = col_double(), Attr12 = col_double(),
                                                      Attr13 = col_double(), Attr14 = col_double(),
                                                      Attr15 = col_double(), Attr16 = col_double(),
                                                      Attr17 = col_double(), Attr18 = col_double(),
                                                      Attr19 = col_double(), Attr20 = col_double(),
                                                      Attr21 = col_double(), Attr22 = col_double(),
                                                      Attr23 = col_double(), Attr24 = col_double(),
                                                      Attr25 = col_double(), Attr26 = col_double(), 
                                                      Attr27 = col_double(), Attr28 = col_double(),
                                                      Attr29 = col_double(), Attr30 = col_double(),
                                                      Attr31 = col_double(), Attr32 = col_double(), 
                                                      Attr33 = col_double(), Attr34 = col_double(),
                                                      Attr35 = col_double(), Attr36 = col_double(),
                                                      Attr37 = col_double(), Attr38 = col_double(),
                                                      Attr39 = col_double(), Attr40 = col_double(), 
                                                      Attr41 = col_double(), Attr42 = col_double(),
                                                      Attr43 = col_double(), Attr44 = col_double(),
                                                      Attr45 = col_double(), Attr46 = col_double(), 
                                                      Attr47 = col_double(), Attr48 = col_double(),
                                                      Attr49 = col_double(), Attr50 = col_double(),
                                                      Attr51 = col_double(), Attr52 = col_double(),
                                                      Attr53 = col_double(), Attr54 = col_double(),
                                                      Attr55 = col_double(), Attr56 = col_double(),
                                                      Attr57 = col_double(), Attr58 = col_double(),
                                                      Attr59 = col_double(), Attr60 = col_double(),
                                                      Attr61 = col_double(), Attr62 = col_double(),
                                                      Attr63 = col_double(), Attr64 = col_double(), 
                                                      class= col_integer()))

# bankrucywo po 4 latach
X2year <- read_csv("dane/2year.csv", col_types = cols(Attr1 = col_double(), Attr2 = col_double(),
                                                      Attr3 = col_double(), Attr4 = col_double(),
                                                      Attr5 = col_double(), Attr6 = col_double(),
                                                      Attr7 = col_double(), Attr8 = col_double(),
                                                      Attr9 = col_double(), Attr10 = col_double(),
                                                      Attr11 = col_double(), Attr12 = col_double(),
                                                      Attr13 = col_double(), Attr14 = col_double(),
                                                      Attr15 = col_double(), Attr16 = col_double(),
                                                      Attr17 = col_double(), Attr18 = col_double(),
                                                      Attr19 = col_double(), Attr20 = col_double(),
                                                      Attr21 = col_double(), Attr22 = col_double(),
                                                      Attr23 = col_double(), Attr24 = col_double(),
                                                      Attr25 = col_double(), Attr26 = col_double(), 
                                                      Attr27 = col_double(), Attr28 = col_double(),
                                                      Attr29 = col_double(), Attr30 = col_double(),
                                                      Attr31 = col_double(), Attr32 = col_double(), 
                                                      Attr33 = col_double(), Attr34 = col_double(),
                                                      Attr35 = col_double(), Attr36 = col_double(),
                                                      Attr37 = col_double(), Attr38 = col_double(),
                                                      Attr39 = col_double(), Attr40 = col_double(), 
                                                      Attr41 = col_double(), Attr42 = col_double(),
                                                      Attr43 = col_double(), Attr44 = col_double(),
                                                      Attr45 = col_double(), Attr46 = col_double(), 
                                                      Attr47 = col_double(), Attr48 = col_double(),
                                                      Attr49 = col_double(), Attr50 = col_double(),
                                                      Attr51 = col_double(), Attr52 = col_double(),
                                                      Attr53 = col_double(), Attr54 = col_double(),
                                                      Attr55 = col_double(), Attr56 = col_double(),
                                                      Attr57 = col_double(), Attr58 = col_double(),
                                                      Attr59 = col_double(), Attr60 = col_double(),
                                                      Attr61 = col_double(), Attr62 = col_double(),
                                                      Attr63 = col_double(), Attr64 = col_double(), 
                                                      class= col_integer()))

# bankructwo po 3 latach

X3year <- read_csv("dane/3year.csv", col_types = cols(Attr1 = col_double(), Attr2 = col_double(),
                                                      Attr3 = col_double(), Attr4 = col_double(),
                                                      Attr5 = col_double(), Attr6 = col_double(),
                                                      Attr7 = col_double(), Attr8 = col_double(),
                                                      Attr9 = col_double(), Attr10 = col_double(),
                                                      Attr11 = col_double(), Attr12 = col_double(),
                                                      Attr13 = col_double(), Attr14 = col_double(),
                                                      Attr15 = col_double(), Attr16 = col_double(),
                                                      Attr17 = col_double(), Attr18 = col_double(),
                                                      Attr19 = col_double(), Attr20 = col_double(),
                                                      Attr21 = col_double(), Attr22 = col_double(),
                                                      Attr23 = col_double(), Attr24 = col_double(),
                                                      Attr25 = col_double(), Attr26 = col_double(), 
                                                      Attr27 = col_double(), Attr28 = col_double(),
                                                      Attr29 = col_double(), Attr30 = col_double(),
                                                      Attr31 = col_double(), Attr32 = col_double(), 
                                                      Attr33 = col_double(), Attr34 = col_double(),
                                                      Attr35 = col_double(), Attr36 = col_double(),
                                                      Attr37 = col_double(), Attr38 = col_double(),
                                                      Attr39 = col_double(), Attr40 = col_double(), 
                                                      Attr41 = col_double(), Attr42 = col_double(),
                                                      Attr43 = col_double(), Attr44 = col_double(),
                                                      Attr45 = col_double(), Attr46 = col_double(), 
                                                      Attr47 = col_double(), Attr48 = col_double(),
                                                      Attr49 = col_double(), Attr50 = col_double(),
                                                      Attr51 = col_double(), Attr52 = col_double(),
                                                      Attr53 = col_double(), Attr54 = col_double(),
                                                      Attr55 = col_double(), Attr56 = col_double(),
                                                      Attr57 = col_double(), Attr58 = col_double(),
                                                      Attr59 = col_double(), Attr60 = col_double(),
                                                      Attr61 = col_double(), Attr62 = col_double(),
                                                      Attr63 = col_double(), Attr64 = col_double(), 
                                                      class= col_integer()))

# bankructwo po 2 latach
X4year <- read_csv("dane/4year.csv", col_types = cols(Attr1 = col_double(), Attr2 = col_double(),
                                                      Attr3 = col_double(), Attr4 = col_double(),
                                                      Attr5 = col_double(), Attr6 = col_double(),
                                                      Attr7 = col_double(), Attr8 = col_double(),
                                                      Attr9 = col_double(), Attr10 = col_double(),
                                                      Attr11 = col_double(), Attr12 = col_double(),
                                                      Attr13 = col_double(), Attr14 = col_double(),
                                                      Attr15 = col_double(), Attr16 = col_double(),
                                                      Attr17 = col_double(), Attr18 = col_double(),
                                                      Attr19 = col_double(), Attr20 = col_double(),
                                                      Attr21 = col_double(), Attr22 = col_double(),
                                                      Attr23 = col_double(), Attr24 = col_double(),
                                                      Attr25 = col_double(), Attr26 = col_double(), 
                                                      Attr27 = col_double(), Attr28 = col_double(),
                                                      Attr29 = col_double(), Attr30 = col_double(),
                                                      Attr31 = col_double(), Attr32 = col_double(), 
                                                      Attr33 = col_double(), Attr34 = col_double(),
                                                      Attr35 = col_double(), Attr36 = col_double(),
                                                      Attr37 = col_double(), Attr38 = col_double(),
                                                      Attr39 = col_double(), Attr40 = col_double(), 
                                                      Attr41 = col_double(), Attr42 = col_double(),
                                                      Attr43 = col_double(), Attr44 = col_double(),
                                                      Attr45 = col_double(), Attr46 = col_double(), 
                                                      Attr47 = col_double(), Attr48 = col_double(),
                                                      Attr49 = col_double(), Attr50 = col_double(),
                                                      Attr51 = col_double(), Attr52 = col_double(),
                                                      Attr53 = col_double(), Attr54 = col_double(),
                                                      Attr55 = col_double(), Attr56 = col_double(),
                                                      Attr57 = col_double(), Attr58 = col_double(),
                                                      Attr59 = col_double(), Attr60 = col_double(),
                                                      Attr61 = col_double(), Attr62 = col_double(),
                                                      Attr63 = col_double(), Attr64 = col_double(), 
                                                      class= col_integer()))


# bankructwo po roku
X5year <- read_csv("dane/5year.csv", col_types = cols(Attr1 = col_double(), Attr2 = col_double(),
                                                      Attr3 = col_double(), Attr4 = col_double(),
                                                      Attr5 = col_double(), Attr6 = col_double(),
                                                      Attr7 = col_double(), Attr8 = col_double(),
                                                      Attr9 = col_double(), Attr10 = col_double(),
                                                      Attr11 = col_double(), Attr12 = col_double(),
                                                      Attr13 = col_double(), Attr14 = col_double(),
                                                      Attr15 = col_double(), Attr16 = col_double(),
                                                      Attr17 = col_double(), Attr18 = col_double(),
                                                      Attr19 = col_double(), Attr20 = col_double(),
                                                      Attr21 = col_double(), Attr22 = col_double(),
                                                      Attr23 = col_double(), Attr24 = col_double(),
                                                      Attr25 = col_double(), Attr26 = col_double(), 
                                                      Attr27 = col_double(), Attr28 = col_double(),
                                                      Attr29 = col_double(), Attr30 = col_double(),
                                                      Attr31 = col_double(), Attr32 = col_double(), 
                                                      Attr33 = col_double(), Attr34 = col_double(),
                                                      Attr35 = col_double(), Attr36 = col_double(),
                                                      Attr37 = col_double(), Attr38 = col_double(),
                                                      Attr39 = col_double(), Attr40 = col_double(), 
                                                      Attr41 = col_double(), Attr42 = col_double(),
                                                      Attr43 = col_double(), Attr44 = col_double(),
                                                      Attr45 = col_double(), Attr46 = col_double(), 
                                                      Attr47 = col_double(), Attr48 = col_double(),
                                                      Attr49 = col_double(), Attr50 = col_double(),
                                                      Attr51 = col_double(), Attr52 = col_double(),
                                                      Attr53 = col_double(), Attr54 = col_double(),
                                                      Attr55 = col_double(), Attr56 = col_double(),
                                                      Attr57 = col_double(), Attr58 = col_double(),
                                                      Attr59 = col_double(), Attr60 = col_double(),
                                                      Attr61 = col_double(), Attr62 = col_double(),
                                                      Attr63 = col_double(), Attr64 = col_double(), 
                                                      class= col_integer()))


