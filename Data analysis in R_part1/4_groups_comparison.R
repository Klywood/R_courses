setwd("C:/Users/User/Desktop/proj/R_lang/Data analysis in R_part1/")
#  https://stepik.org/lesson/11504/step/1?unit=2527

?iris

df <- iris

str(df)
#  Оставляем 2 группы цветков
df1 <- subset(df, Species != 'setosa')
str(df1)

hist(df1$Sepal.Length)

library(ggplot2)
#  graph 1
ggplot(df1, aes(x = Sepal.Length)) +
  geom_histogram(fill = 'white', col='black', binwidth = 0.4)+
  facet_grid(Species ~ .)
#  graph 2
ggplot(df1, aes(x = Sepal.Length, fill = Species))+
  geom_density(alpha = 0.4)
#  graph 3
ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

#  Проверка нормальности распределения
shapiro.test(df1$Sepal.Length)

shapiro.test(df1$Sepal.Length[df1$Species == 'versicolor'])
shapiro.test(df1$Sepal.Length[df1$Species == 'virginica'])

#  Проверка гомогенности дисперсий
bartlett.test(Sepal.Length ~ Species, df1)

#  Значения p-уровня значимости во всех проверках превышают 0,05
#  распределения можно считать нормальными, а дисперсии гомогенными

#  Проводим T-тест
test1 <- t.test(Sepal.Length ~ Species, df1)

str(test1)
test1$p.value
# H0: среднее равно 8 (отклоняем H0)
t.test(df1$Sepal.Length, mu = 8)
# H0: 2 распределения НЕ отличаются (отклоняем H0)
t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

#  Task 1 - https://stepik.org/lesson/11504/step/10?unit=2527
View(ToothGrowth)
gr1 <- subset(ToothGrowth, supp == 'OJ' & dose == 0.5)
gr2 <- subset(ToothGrowth, supp == 'VC' & dose == 2)

t_stat <- t.test(gr1$len, gr2$len)$statistic

#  Task 2 - https://stepik.org/lesson/11504/step/11?unit=2527
data <- read.csv('lekarstva.csv')

t.test(data$Pressure_before, data$Pressure_after, paired = T)$statistic


#  Визуализация результатов теста
install.packages("Hmisc")
#  Доверительные интервалы и среднее значение
ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.1)+
  stat_summary(fun = mean, geom = 'point', size = 4)
# 2 вариант
ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size=1)

#  Непараметрический анализ критерия Стьюдента 
#  (если требования для Т теста не выполняюся)
?wilcox.test

wilcox.test(Petal.Length ~ Species, df1)
#  для зависимых выборок
wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

#  Task 3 - https://stepik.org/lesson/11504/step/15?unit=2527

task3 <- read.table('gr_compr_task3.txt')

if (bartlett.test(V1 ~ V2, task3)$p.value >= 0.05) {
  t.test(V1 ~ V2, task3, var.equal = T)$p.value
} else {
  wilcox.test(V1 ~ V2, task3, var.equal = T)$p.value
}

#  Task 4 - https://stepik.org/lesson/11504/step/16?unit=2527
task4 <- read.table('gr_compr_task4.txt')

t_test_4 <- t.test(task4$V1, task4$V2)
if (t_test_4$p.value < 0.05){
  print(c(mean(task4$V1), mean(task4$V2), t_test_4$p.value))
} else {
  print('The difference is not significant')
}



