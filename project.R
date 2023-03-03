# Mikolaj Halemba
rm(list=ls())

#Libraries
library(eurostat)
library(ggplot2)
library(dplyr)

#Load data
df<-eurostat::get_eurostat("prc_hicp_manr")

#Filter data
df_new <- filter(df,coicop == "CP00")
df_new <- filter(df_new,time >= "2000-02-01")
df_new <- filter(df_new,time <= "2022-09-01")

#Prepare countries
countries_eu <- c(eu_countries$name)
countries_eu <- countries_eu[countries_eu != "United Kingdom"]
df_new$geo <- label_eurostat(df_new$geo, dic = "geo", countrycode = "country.name")
df_new <- df_new %>% filter(df_new$geo %in% countries_eu)

#Aggreagte data
df_agg <- aggregate(df_new$values, list(df_new$geo,df_new$time), FUN=sum,na.rm=T)
df_agg <- df_agg %>% rename(country=Group.1, time=Group.2, values=x)
df_agg<-df_agg[, c("time","country","values")]

#Visualize data
wyk <- ggplot(data=df_agg, aes(x=time, y=values, group=country, colour=country))+geom_line()
x11();print(wyk)

#Prepare data for dendogram
df<-df_agg %>%
  group_by(country) %>%
  summarise(values = list(values))

matrix <- matrix(unlist(df$values), nrow = length(df$values), byrow = TRUE)
df_hra_dist <- dist(matrix, method="minkowski", p=1.5)

#fit for aglomerative hierarchical clustering
fit <- hclust(df_hra_dist)

#Visualize dendogram
x11();plot(fit,labels = df$country,main = "Clustering countries based on HICP",xlab="",ylab="")
rect.hclust(fit, k=4, border="cadetblue")


