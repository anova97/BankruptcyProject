## Braki danych ----
## Polska

source("R/ImportData.R")

options(scipen = 100)

brak1 <- visdat::vis_miss(X1year,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("X1year") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))
brak2 <- visdat::vis_miss(X2year,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("X2year") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))
brak3 <- visdat::vis_miss(X3year,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("X3year") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))
brak4 <- visdat::vis_miss(X4year,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("X4year") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))
brak5 <- visdat::vis_miss(X5year,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("X5year") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))

razem <- ggarrange(brak1, brak2, brak3, brak4, brak5) 
annotate_figure(razem, top = text_grob("Braki dla poszczególnych zbiorów danych", 
                                       face = "bold", size = 18))

## słowacja 

brak1 <- visdat::vis_miss(dataSk1,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("dataSk1") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))
brak2 <- visdat::vis_miss(dataSk2,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("dataSk2") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))
brak3 <- visdat::vis_miss(dataSk3,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("dataSk3") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))
brak4 <- visdat::vis_miss(dataSk4,show_perc_col = FALSE, warn_large_data=FALSE) + ylab("Obserwacje")+ggtitle("dataSk4") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))

brak1 + ylab("Obserwacje") +theme(axis.title.x = element_text(size = 20)) + theme(axis.text.x=element_text(color=c("black","transparent")))

razem <- ggarrange(brak1, brak2, brak3, brak4) 
annotate_figure(razem, top = text_grob("Braki dla poszczególnych zbiorów danych", 
                                       face = "bold", size = 18))
## statystyki opisowe ----
#

# saved as pdf (wydruki)
statystykiWydruk(X1year, 1)
statystykiWydruk(X2year, 2)
statystykiWydruk(X3year, 3)
statystykiWydruk(X4year, 4)
statystykiWydruk(X5year, 5)


summary(X1year)
summary(X2year)
summary(X3year)
summary(X4year)
summary(X5year)

# saved as pdf
statystykiWydruk(dataSk1, "sk1")
statystykiWydruk(dataSk2, "sk2")
statystykiWydruk(dataSk3, "sk3")
statystykiWydruk(dataSk4, "sk4")


statystykiWydruk(data[,c(46:84,86:93,95,96)], "taiwan2")

## usunięcie braków ----

### po brakach
options(scipen = 100)

X1year <- usunNAKol(X1year)
X1yearpo <- usunNAWiersz(X1year)

X2year <- usunNAKol(X2year)
X2yearpo <- usunNAWiersz(X2year)

X3year <- usunNAKol(X3year)
X3yearpo <- usunNAWiersz(X3year)

X4year <- usunNAKol(X4year)
X4yearpo <- usunNAWiersz(X4year)

X5year <- usunNAKol(X5year)
X5yearpo <- usunNAWiersz(X5year)

brak1po <- visdat::vis_miss(X1yearpo,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("X1year") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))
brak2po <- visdat::vis_miss(X2yearpo,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("X2year") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))
brak3po <- visdat::vis_miss(X3yearpo,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("X3year") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))
brak4po <- visdat::vis_miss(X4yearpo,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("X4year") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))
brak5po <- visdat::vis_miss(X5yearpo,show_perc_col = FALSE) + ylab("Obserwacje")+ggtitle("X5year") + theme(axis.text.x=element_text(color=c("black","transparent", "transparent")))

brak1po + ylab("Obserwacje") +theme(axis.title.x = element_text(size = 20)) + theme(axis.text.x=element_text(color=c("black","transparent")))


razem <- ggarrange(brak1po, brak2po, brak3po, brak4po, brak5po) 
annotate_figure(razem, top = text_grob("Braki dla poszczególnych zbiorów danych", 
                                       face = "bold", size = 18))

## stosunek  ilościowy klas ----

wykres1 <-  barplot(table(X1year$class), col = c("darkgreen", "red"),
                  xlab="Bankructwo",
                  ylab="Ilość firm")
text(wykres1, 0, table(X1year$class), cex=2,pos=3) 
title("X1year")

wykres2 <-  barplot(table(X2year$class), col = c("darkgreen", "red"),
                  xlab="Bankructwo",
                  ylab="Ilość firm")
text(wykres2, 0, table(X2year$class), cex=2,pos=3) 
title("X2year")

wykres <-  barplot(table(X3year$class), col = c("darkgreen", "red"),
                 xlab="Bankructwo",
                 ylab="Ilość firm")
text(wykres, 0, table(X3year$class), cex=2,pos=3) 
title("X3year")

wykres <-  barplot(table(X4year$class), col = c("darkgreen", "red"),
                 xlab="Bankructwo",
                 ylab="Ilość firm")
text(wykres, 0, table(X4year$class), cex=2,pos=3) 
title("X4year")

wykres <-  barplot(table(X5year$class), col = c("darkgreen", "red"),
                 xlab="Bankructwo",
                 ylab="Ilość firm")
text(wykres, 0, table(X5year$class), cex=2,pos=3) 
title("X5year")

# SK
wykres1SK <-  barplot(table(dataSk1$class), col = c("darkgreen", "red"),
                    xlab="Bankructwo",
                    ylab="Ilość firm")
text(wykres1SK, 0, table(dataSk1$class), cex=2,pos=3) 
title("dataSk1")

wykres2SK <-  barplot(table(dataSk2$class), col = c("darkgreen", "red"),
                    xlab="Bankructwo",
                    ylab="Ilość firm")
text(wykres2SK, 0, table(dataSk2$class), cex=2,pos=3) 
title("dataSk2")

wykres3SK <-  barplot(table(dataSk3$class), col = c("darkgreen", "red"),
                   xlab="Bankructwo",
                   ylab="Ilość firm")
text(wykres3SK, 0, table(dataSk3$class), cex=2,pos=3) 
title("dataSk3")

wykres4SK <-  barplot(table(dataSk4$class), col = c("darkgreen", "red"),
                   xlab="Bankructwo",
                   ylab="Ilość firm")
text(wykres4SK, 0, table(dataSk4$class), cex=2,pos=3) 
title("dataSk4")
## korelacja ----

X1year <- dataYear(X1year)

korelacjaZmienne1p <- cor(X1year[,1:(ncol(X1year)-1)])
ggcorrplot(korelacjaZmienne1p, tl.cex = "8")+ggtitle("X1year")


