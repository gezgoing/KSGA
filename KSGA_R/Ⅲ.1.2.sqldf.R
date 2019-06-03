# install.packages("sqldf")
library(sqldf)

data(iris)
tmp <- iris
sqldf("select * from tmp limit 2")
head(iris,2)
names(tmp)

names(tmp)<-c("sepal_length","sepal_width","Petal_Length","petal_width","species")
names(tmp)<-tolower(names(tmp))

sqldf("select * from tmp limit 5")
no<-1:150
label<-rep(1:10,15)

ex<-cbind(no,tmp,label)
sqldf("select * from ex limit 11")

# 조건 - where
sql_a<-sqldf("select no, sepal_length, sepal_width, species, label from ex where sepal_width < 2.5")
head(sql_a,2)
range(sql_a$sepal_width)

sql_b<-sqldf("select sepal_length, petal_width, species from ex where species = 'setosa'")
head(sql_b,2)
table(sql_b$species)

# 순서 - order by
sqldf("select sepal_length, petal_width from ex order by sepal_length desc limit 2")

# 그룹 - group by
sqldf("select avg(sepal_length), avg(petal_width), species from ex group by species")
sqldf("select max(sepal_length) max_s_l, min(sepal_length) min_s_l, min(petal_width), species from ex group by species")
sqldf("select count(*) from ex group by species")

# max (num vs. chr)
ex$label<-as.numeric(ex$label)
table(ex$label)
sqldf("select no, sepal_width, species, max(label) from ex where sepal_width < 2.8 and no<71 group by species")

ex$label<-as.character(ex$label)
table(ex$label)
sqldf("select no, sepal_width, species, max(label) from ex where sepal_width < 2.8 and no<71 group by species")

# join
table(ex$species)
ex4<-ex[c(1:3,51:53,101:103),c(1,2,6)]
str(ex4)

species<-c(rep("setosa",2),rep("versicolor",2))
idx<-c(1:2,51:52)
ex2<-data.frame(species,idx)
str(ex2)

ex2$species<-factor(ex2$species)
levels(ex2$species)<-levels(ex4$species)

ex2;ex4
sqldf("select a.*, b.* from ex4 a, ex2 b where a.no = b.idx")
(a<-sqldf("select a.*, b.idx from ex4 a, ex2 b where a.no = b.idx"))

(a<-sqldf("select a.*, b.idx from ex4 a left outer join ex2 b on a.no = b.idx"))

# Dynamic Sql
summary(ex$sepal_length)
range(ex$sepal_length)
d<-0
for (i in 4:7){
  sql_sl<-paste("select count(*) from ex where sepal_length >",i,"and sepal_length<",i+1)
  d[i-3]<-sqldf(sql_sl)[1,1]
}
d
