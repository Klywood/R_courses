setwd("C:/Users/User/Desktop/proj/R_lang/Data analysis in R_part1/")

?mtcars


df <- mtcars

str(df)

df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))

library(psych)

?describe

#  Main statistics

descr = describe(x = df[, -c(8,9)])


descr2 <-  describeBy(x = df[, -c(8,9)], group = df$vs, mat = T, digits = 1)


descr3 <-  describeBy(x = df[, -c(8,9)], group = df$vs, mat = T, digits = 1, fast = T)


describeBy(df$qsec, list(df$vs, df$am), digits = 1,
           fast = T)



df$mpg[1:10] <- NA


mean(df$mpg, na.rm = T)

data <- airquality

new <- subset(data, Month %in% c(7,8,9))

result <- aggregate(Ozone ~ Month, new, length)

str(airquality)

describeBy(airquality, list(airquality$Month),
           digits = 1)

describeBy(iris, fast = T)


aggregate(x=iris, by=list(iris$Species), FUN=median, )

describeBy(iris, group = list(iris$Species))

describeBy(iris, group = iris$Species)$'virginica'['median']

my_vector <- rnorm(30)

my_vector[sample(1:30, 10)] <- NA
is.na(my_vector)

replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))

# Simple plots

hist(df$mpg, breaks = 20, xlab = 'MPG')

boxplot(mpg ~ am, df, ylab = 'MPG')

plot(df$mpg, df$hp)

#  ggplot2

library(ggplot2)

ggplot(df, aes(x = mpg, fill = am)) + 
  geom_dotplot()

ggplot(df, aes(x = mpg, fill = am)) + 
  geom_density(alpha=0.4)


ggplot(df, aes(x = am, y = hp, col = vs)) +
  geom_boxplot()

ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec)) +
  geom_point(alpha = 0.4)


ggplot(data, aes(x = as.factor(Month), y = Ozone)) +
  geom_boxplot(na.rm = T)

plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, col = hp))+
  geom_point()


plot2 <- ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species, size = Petal.Length)) +
  geom_point()

#  saving data
write.csv(df, 'mtcars.csv')
#  saving variables
save(descr, file='descr.RData')




