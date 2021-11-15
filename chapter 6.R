library(fpp2)
library(seasonal)
library(forecast)

## 6.1

##6.2 A.

plot(plastics, ylab="Sales", main="Product A")
## We can observe that as the year progress the sales of the product increase and when the year is starting the sales are very low. We can observe a highly seasonal product.

## B.

mull <- decompose(plastics, type = "multiplicative")
trnd <- mull$trend
snsl <- mull$seasonal

plot(trnd)
plot(snsl)

## C. 
## The result do validate the graphical interpretation as they show the trend going upwards and the seasonality in each year.

## D.
plot(seasadj(mull))

## E.
plastics[25] = plastics[25]+500
mull <- decompose(plastics, type = "multiplicative")
plot(seasadj(mull))

## F.

## I believe that it does makes a difference outliers in the middle of the date disrupt the seasonality of the product.

## G.
Randowlk <-rwf(seasadj(mull), drift=TRUE)

## H.

resns <- stl(plastics, t.window = 15, s.window = "periodic")
frcts1 <- forecast(resns, method= "naive")

##6.3 

## I could not donwload the data for some reason my computer does not let me save it as csv or excel file.

##6.4 A.

## The decomposition posses a positive trend as it increases from the 1980s to the 1990s. We can also observe a seasonality in the labor force and a period where there a great number of people outiside of the labor forcee.

## B.

## The recession if visible in the remainder plot in the early 1990s with a big spike.


##6.5 A. 

autoplot(cangas)
ggsubseriesplot(cangas)
ggseasonplot(cangas)

## The plots show that there is an increase of production during the months of september to january (probaly do to demand). there is also an upwards trend that may be casued by the increase of population.

## B.
cangas %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

## C. 
cangas %>% seas() %>%
  autoplot() +
  ggtitle("SEATS decomposition of Canadian Monthly Gas Production")

cangas %>% seas(x11="") -> fit
autoplot(fit) +
  ggtitle("X11 decomposition of Canadian Monthly Gas Production")

##6.6



##6.7
Wrttngfc <- stlf(writing, 
                     robust = TRUE,
                     lambda = BoxCox.lambda(writing),
                     method = "rwdrift")
autoplot(Wrttngfc)

##6.8
Fancyfc <- stlf(fancy, 
                 robust = TRUE,
                 lambda = BoxCox.lambda(fancy),
                 method = "rwdrift")
autoplot(Fancyfc)
