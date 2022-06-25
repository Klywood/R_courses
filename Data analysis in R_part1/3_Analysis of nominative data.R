setwd("C:/Users/User/Desktop/proj/R_lang/Data analysis in R_part1/")


df <-  read.csv('grants.csv')

str(df)

#  creating factor
?factor
df$status <- factor(df$status, labels = c('Not funded', 'Funded'))

# table 1
t1 <- table(df$status)
t1
dim(t1)

# table 2
t2 <- table(status = df$status, field = df$field)
t2
dim(t2)


prop.table(t2) #  summary
prop.table(t2, 1) #  by rows
prop.table(t2, 2) #  by cols

# table 3
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3
dim(t3)


#  task 1
#  Ваша задача в переменную red_men сохранить долю рыжеволосых (Red) 
#    от общего числа голубоглазых мужчин.

red_men <- prop.table(HairEyeColor[ , ,'Male'], 2)
red_men['Red', 'Blue']

#  task 2
#  Напишите число зеленоглазых женщин в наборе данных HairEyeColor
sum(HairEyeColor[ , ,'Female'][, 'Green'])


#  Plots
barplot(t2)
barplot(t2, legend.text = T, args.legend = list(x = 'topright'))
barplot(t2, legend.text = T, args.legend = list(x = 'topright'), beside = T)

mosaicplot(t2)

#  task 3
#  Постройте столбчатую диаграмму распределения цвета глаз по цвету волос только у женщин
library(psych)
library(ggplot2)
mydata <- as.data.frame(HairEyeColor)
?geom_bar
women  <-  subset(x = mydata, subset = mydata$Sex == 'Female')
ggplot(data = women, aes(x = Hair, y = Freq, fill = Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

#  Binomial Test
binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)

#  Chi-Square Test
chisq.test(t1)
chisq.test(t2)

#  Fisher's Exact Test
fisher.test(t2)

#  task 4
# https://stepik.org/lesson/11502/step/12?unit=2525
w_brown <- HairEyeColor['Brown', ,'Female']
chisq.test(w_brown)

#  task 5
#  https://stepik.org/lesson/11502/step/13?unit=2525
diamonds
main_stat <- chisq.test(table(Cut = diamonds$cut, Color = diamonds$color))$statistic

#  task 6
#  https://stepik.org/lesson/11502/step/14?unit=2525
diamonds
my_diam <- diamonds
mean_price <- mean(my_diam$price)
mean_carat <- mean(my_diam$carat)

my_diam$factor_price <- ifelse(test = my_diam$price >= mean_price, 1, 0)
my_diam$factor_carat <- ifelse(test = my_diam$carat >= mean_carat, 1, 0)

main_stat <- chisq.test(table(my_diam$factor_price, my_diam$factor_carat))$statistic

#  task 7
#  https://stepik.org/lesson/11502/step/15?unit=2525
fisher_test <- fisher.test(table(mtcars$am, mtcars$vs))$p.value

