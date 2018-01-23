KOLOKWIUM

1. Pobranie pliku ze strony
a) wczytanie danych do środowiska
data <- read.csv("ships.csv")
head(data)

b)usunięcie zmiennej X (pierwsza zmienna)
data <- select(data, -(1))
head(data)

c) zamień wartość zmiennej "ship" na tryb czynnikowy zgodnie z wzorem 1= pierwsz, 2=drugi, 3=trzeci,4=czwarty, 5=piąty
data$ship <- factor(data$ship, labels = c("pierwszy","drugi","trzeci","czwarty","piaty"))
head(data)

d) stwórz nowy wektor o nazwie p, długości równej liczbie wczytanego zbioru danych, o wartościach z sekwencji od 1 do 40
p <- rep(c(1:40),length.out = nrow(data))
p

e) dodaje wektor p jako nową kolumnę we wczytanym zbiorze danych 
data <- mutate(data, p)
head(data)

f) napisz funkcję która wczytując zbiór danych  zwróci tekst  będący połączeniem  wartości z kolumny  "ship" i "p"
f = function(file){
  data <- read.csv(file)
  p <- c(1:nrow(data))
  data <- mutate(data, p)
  return(
    print(select(data, ship, p))
  )
}

f("ships.csv")


ZADANIE 2

Wykonaj serię wykresów na podstawie zbioru danych. wykorzystaj pakiet ggplot2 i tidy verse

a) wczytaj dane txhousing, wybierz losowo 40% obserwacji spośród wierszy bez braków danych . zmień typ zmiennych "year" i "month"  na tym czynnikowy
data <- txhousing
head(data)

data <- na.omit(data)
head(data)

test <- sample_frac(data, 0.4)
head(test)

test$year <- factor(test$year)
test$month <- factor(test$month)
head(test)

b) pogrupuj zbiór względem  roku i miesiąca, podsumuj go wg średniej wartości sprzedaży  i średniej wielkości mieszkania 
test <- arrange(test, year, month)
head(test)

sumtest <- test %>%
  group_by(year, month) %>%
  select(salse, volume) %>%
  summarize(
    avs = mean(sales),
    avv = mean(volume)
  )
sumtest

c) narysuj wykres słupkowy wskazujący wartość średniej spredaży w każdym roku , dodaj tytul

salesyear <- test %>%
  group_by(year) %>%
  summarize(
    avs = mean(sales)
  )
  
salesyear

print(
  ggplot(salesyear) + 
    geom_col(
      aes( x = year, y = avs)
    ) +
    ggtitle("Sprzedaz w latach")
)

d) narysuj wykres punktowy  wskazujący średnią wielkosć sprzedanego mieszkania w danym roku. Kolorami zaznacz na jaki miesiąc są to dane.
odwróc tekst na osi X  pod kątem 90 stropni

volumeyear <- test %>%
  group_by(year, month) %>%
  summarize(
    avv = mean(volume)
  )
volumeyear

ggplot(volumeyear) +
  geom_point(
    aes( x = year, y = avv, col = month)
  ) +
  theme(axis.text.x = element_text(angle = 90))
  
  e) wyeksportuj 1 wykresy do pliku o nazwie  wykres.pdf  o wymiarach 10x8
  
  pdf("wykres.pdf", width = 10, height = 8)
  
  
  ZADANIE 4
  
  Zbuduj 2 drzewa klasyfikacyjne na podstawie modeli ctree i rpart. Porownaj ich jakość klasyfikacyjną i Accuracy
  , obliczonymi na podstawie zbioru testującego.  Wykorzystaj dane z pakietu Curet o nazwie GermanCredit, wyznaczając prognoze bycia dobrym 
  lub zlym klientem . Zoptymalizuj parametry modeli.
  
  set.seed(1)

data("GermanCredit")
head(GermanCredit)
train_frac <- 0.2

training.set.index <- (runif(nrow(GermanCredit)) < train_frac)
train.set <- GermanCredit[training.set.index, ]
test.set <- GermanCredit[!training.set.index, ]

ctree.model <- ctree(factor(Class) ~ ., data = train.set,
                     controls = ctree_control(mincriterion = 0.99,
                                              minsplit = 20))
plot(ctree.model, tnex = 2, type = "extended")

rpart.model <- rpart(Class ~ ., train.set, cp = 0.00001, minsplit = 2)
plotcp(rpart.model)

minimum.error <- which.min(rpart.model$cptable[, "xerror"])
optimal.complexity <- rpart.model$cptable[minimum.error, "CP"]
points(minimum.error, rpart.model$cptable[minimum.error, "xerror"],
       col = "red", pch = 19)
pruned.tree <- prune(rpart.model, cp = optimal.complexity)
plot(pruned.tree, compress = T, uniform = T, margin = 0.1,
     branch = 0.3, nspace = 2)
text(pruned.tree, use.n = TRUE, pretty = 0)

confusion.matrix <- list()
print(confusion.matrix[[1]] <- table(predict(ctree.model, new = test.set),
                                     test.set$Class))
print(confusion.matrix[[2]] <- table(predict(rpart.model, type = "class",
                                             newdata = test.set),
                                     test.set$Class))
print(confusion.matrix[[3]] <- table(predict(pruned.tree, type = "class",
                                             newdata = test.set),
                                     test.set$Class))

CalculateAccuracy <- function(confusion.matrix) {
  return(sum(diag(confusion.matrix)) / sum(confusion.matrix))
}

print(data.frame(model = c("ctree", "rpart","rpart przyciety"),
                 dokladnosc = sapply(confusion.matrix, CalculateAccuracy)),
      row.names = FALSE)
      
      
ZADANIE 5 

Stworz las losowy  dokonujący klasyfikacji klientów  na podstawie danych z (źródło). Oceń za pomocą wykresu istotność zmiennych.
Wyrysuj wkresy ROC, sensitivity/specificity i Lift

data <- read_csv2("bank.csv")
head(data)

data <- mutate(data,
  job = factor(job),
  marital = factor(marital),
  education = factor(education),
  default = factor(default),
  housing = factor(housing),
  loan = factor(loan),
  contact = factor(contact),
  month = factor(month),
  poutcome = factor(poutcome),
  y = factor(y)
)
head(data)

test <- runif(nrow(data), 0,1)

rf <- randomForest(y ~. , data = data[test <= 0.8,],
                   mtre = 500,
                   mtry = 4)
varImpPlot(rf)

forecast <- predict(rf, newdata = data[test > 0.8, ], type = "prob")[,2]

plottingData <- prediction(forecast,data[test > 0.8, ]$y)

# krzywa ROC - potrzebuje "ciÄ…gĹ‚ej" prognozy
plot(performance(plottingData,"tpr","fpr"),lwd=2, colorize=T) 

#AUC (Area Under Curve) - pole pod krzywÄ… ROC
performance(plottingData,"auc")

# Sensitivity/specificity plots ~ trade-off
plot(performance(plottingData ,"sens","spec"),lwd=2) 

# Lift chart
plot(performance(plottingData ,"lift","rpp"),lwd=2, col = "darkblue") 

