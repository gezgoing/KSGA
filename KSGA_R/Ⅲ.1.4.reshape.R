library(reshape)
data(airquality)
names(airquality) <- tolower(names(airquality))

# melt
aqm <- melt(airquality, id = c("month", "day"), na.rm = TRUE)
head(aqm,2)
tail(aqm,2)

# cast
a <- cast(aqm, month ~ variable, c(mean,max,min,median,var,sd))
head(a)
b <- cast(aqm, day ~ month ~ variable)
head(b)
c <- cast(aqm, month ~ . |variable, mean)
head(c)
d <- cast(aqm, month ~ variable, mean, margins=c("grand_row", "grand_col"))
head(d)
e <- cast(aqm, day ~ month, mean, subset=variable=="ozone")
head(e)
f <- cast(aqm, month ~ variable, range)
head(f)
