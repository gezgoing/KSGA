### Ⅴ.2.2) R을 이용한 시각화

library(ggplot2)
data(diamonds)
head(diamonds)

## Base 

# Histograms
hist(diamonds$carat, main = "Carat Histogram", xlab = "Carat")

# Scatterplot
plot(price ~ carat, data = diamonds)
plot(diamonds$carat, diamonds$price)

# Boxplots
boxplot(diamonds$carat)


## ggplot2

# ggplot2 Histograms and Densities
ggplot(diamonds) + geom_histogram(aes(x = carat))
ggplot(diamonds) + geom_density(aes(carat), fill = "grey50")

# ggplot2 Scatterplots
ggplot(diamonds, aes(x = carat, y = price)) + geom_point()

g <- ggplot(diamonds, aes(x = carat, y = price))
g + geom_point(aes(color = color))

g + geom_point(aes(color = color)) + facet_wrap(~color)
g + geom_point(aes(color = color)) + facet_grid(cut ~ clarity)

ggplot(diamonds, aes(carat)) + geom_histogram() + facet_wrap(~color)


# ggplot2 Boxplots and Violins Plots
ggplot(diamonds, aes(y = carat, x = 1)) + geom_boxplot()

ggplot(diamonds, aes(y = carat)) + geom_boxplot()

ggplot(diamonds, aes(y = carat, x = cut)) + geom_boxplot()

ggplot(diamonds, aes(y = carat, x = cut)) + geom_violin()

ggplot(diamonds, aes(y = carat, x = cut)) + geom_point() + geom_violin()

ggplot(diamonds, aes(y = carat, x = cut)) + geom_violin() + geom_point()

ggplot(economics, aes(x = date, y = pop)) + geom_line()

library(lubridate)

economics$year <- year(economics$date)
economics$month <- month(economics$date)

econ2000 <- economics[economics$year >= 2000,]
econ2000

library(scales)

g <- ggplot(econ2000, aes(x = month, y = pop))

g <- g + geom_line(aes(color = factor(year), group = year))
g <- g + scale_color_discrete(name = "Year")
g <- g + scale_y_continuous(labels = comma)
g <- g + labs(title = "Population Growth", x = "Month", y = "Population")
g

# Themes
install.packages('ggthemes')
library(ggthemes)

g2 <- ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = color))

g2 + theme_economist() + scale_color_economist()
g2 + theme_excel() + scale_color_excel()
g2 + theme_tufte()
g2 + theme_wsj()
