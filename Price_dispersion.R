#Load libraries and Import Dataset
library(ggplot2)
library(data.table)
library(dplyr)
library(plotly)
library(forecast)
library(nlme)
library(fpp2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(zoo)
library(data.table)
library("ggforce")
library(zoo)
library(ggdendro)
library(tidyr)
library(dtw)
library(graphics)
bigd <- fread('data_pricing.csv') # Import dataset
#Check dataset
head(bigd,n=2)
str(bigd)
dim(bigd)
#Filter the catagory speakers
unique(bigd$category) #Check the name of category correctly
filter<-bigd$category == "Mobile Speakers" # Create filter for speakers
speaker <- bigd[filter,] #Save the data in a dataframe
#Speaker dataframe added Month-Year in new column
speaker$Md <- as.yearmon(paste(speaker$year, speaker$month_numerical , sep = "-" ))
View(speaker)#Write it to file for future reference
rm(bigd) ## remove big dataset
unique(speaker$category) # Check the only category in dataframe speaker
dim(speaker)
#Write it in a file for future reference
write.csv(speaker,'~/Documents/Data Visualization/Home_Assignment/speaker.csv') 
(unique(speaker$store_id))  # No of unique store ids in data
(unique(speaker$product_id)) # No of unique product ids in data


####Task1 - Graph 1
#--------------


# preparing e dataframe filtered with a product_id for monthly avg price
speaker %>% group_by(store_id) %>%
  summarise(Mean=mean(cpi_adjusted_price)) -> per_year
View(per_year)
summary(per_year)  ## Mean per year per 


## First graph - boxplot summary statistics

My_Theme1 <- theme(
  axis.title.x = element_text(size = 22),
  axis.text.x = element_text(size = 22),
  axis.title.y = element_text(size = 22,),
  axis.text.y = element_text(size = 22),
  plot.title = element_text(size = 35, face = "bold"),
  legend.position="none")

p<-ggplot(speaker,aes(as.factor(year),cpi_adjusted_price,color=as.factor(year)))

p+geom_jitter(size=0.00001)+
  geom_boxplot(size=0.5,alpha=0.5,color="black",outlier.colour = "black",outlier.size = 0.7,outlier.shape =18,outlier.alpha = 0.4)+
  stat_boxplot(geom ='errorbar',color="black",size=1.2)+
  stat_summary(fun="mean",color= "red",size=0.7,shape=8)+
  ggtitle("Mobile Speaker Category : All stores")+
  ylab("CPI adjusted prices")+
  xlab("Time Period")+
  theme_ipsum()


p+geom_jitter(size=0.00001)+
  geom_boxplot(size=1.2,alpha=0.5,color="black",outlier.colour = "black",outlier.size = 0.9,
               outlier.shape =1,outlier.alpha = 0.8)+
  stat_boxplot(geom ='errorbar',color="black",size=1.2)+
  stat_summary(fun="mean",color= "red",size=0.7,shape=8)+
  coord_cartesian(ylim = c(-100, 7500))+
  ggtitle("Mobile Speakers")+
  ylab("CPI adjusted prices")+
  xlab("Time Period")+
  theme_minimal()+
  My_Theme1

#### Calculate mean prices per product ID

speaker %>% group_by(product_id) %>%
  summarise(Mean=mean(price)) -> mean_price

View(mean_price)
summary(mean_price)

## Selected 4 product IDs
# Calculating number of stores in every product id for exploring data
speaker %>% group_by(product_id) %>%
  add_count(unique(store_id)) -> bb
View(bb)
write.csv(bb,'~/Documents/Data Visualization/Home_Assignment/bb.csv') #wrote the file to disk and put it back with no of stores per id
bc <- fread('bb.csv')

#Summary stats boxplot - selected prod id from mean price

#1121458 min 121.5  = 13 stores
#523968 max 31069 = 5 stores
#2866047 - 433.8 1st Quartile = 20 stores
#3676036 - 2579 3rd Quartile= 20 stores
#1436287 - Median =  21 stores

#Check no of stores
filter <- speaker$product_id == "155663"
t<- speaker[filter,]
unique(t$store_id)

#Mean plot of monthly average of prices for a product

filter <-speaker$product_id == "1121458"
g<- speaker[filter,]
g %>% group_by(Md,store_id) %>%
  summarise(Mean=mean(cpi_adjusted_price)) -> per_year2


ggplot(per_year2, aes(Md,Mean,color=as.factor(store_id))) +
  geom_line(size=0.8)+geom_jitter()+
  theme_minimal()+
  ggtitle("Average prices for Product ID : 1121458 ")+
  theme(legend.position="none")+
  ylab("Average prices per month")+
  xlab("Time Period")+
  My_Theme1

#########_-------------- Timeseries plots

ggplot(speaker %>% filter(product_id == 1436287),aes(date,cpi_adjusted_price,color=as.factor(store_id)))+
  geom_step()+
  theme_minimal()+
  ggtitle("Prices per store : Product 1436287")+
  scale_x_date(date_labels = "%b-%Y",guide = guide_axis(angle = 90))+
  xlab("Dates")+ylab("CPI adjusted prices")+
  My_Theme1

ggplot(speaker %>% filter(product_id == 3676036),aes(date,cpi_adjusted_price,color=as.factor(store_id)))+
  geom_step()+
  theme_minimal()+
  theme(legend.position="none")+
  ggtitle("Prices per store : Product 3676036")+
  scale_x_date(date_labels = "%b-%Y",guide = guide_axis(angle = 90))+
  xlab("Dates")+ylab("CPI adjusted prices")+
  My_Theme1
2866047

ggplot(speaker %>% filter(product_id == 2866047),aes(date,cpi_adjusted_price,color=as.factor(store_id)))+
  geom_step()+
  theme_minimal()+
  theme(legend.position="none")+
  ggtitle("Prices per store : Product 2866047")+
  scale_x_date(date_labels = "%b-%Y",guide = guide_axis(angle = 90))+
  xlab("Dates")+ylab("CPI adjusted prices")+
  My_Theme1

523968


ggplot(speaker %>% filter(product_id == 523968),aes(date,cpi_adjusted_price,color=as.factor(store_id)))+
  geom_step()+
  theme_minimal()+
  theme(legend.position="none")+
  ggtitle("Prices per store : Product 523968")+
  scale_x_date(date_labels = "%b-%Y",guide = guide_axis(angle = 90))+
  xlab("Dates")+ylab("CPI adjusted prices")+
  My_Theme1

ggplot(speaker %>% filter(product_id == 1121458),aes(date,cpi_adjusted_price,color=as.factor(store_id)))+
  geom_step()+
  theme_minimal()+
  theme(legend.position="none")+
  ggtitle("Prices per store : Product 1121458")+
  scale_x_date(date_labels = "%b-%Y",guide = guide_axis(angle = 90))+
  xlab("Dates")+ylab("CPI adjusted prices")+
  My_Theme1


######### Clustering 

# Products be presented by data exploration

# 2560633,1557082 by data exploration
#1121458 min
#523968  max

# Run this command 1 time to prepare function for deleting NA values - Code proivded by Charlie
pr_DB$set_entry(FUN = dtwOmitNA, names = c("dtwOmitNA"))

#Change this below x valyes with different prod id and 
#run further commands altogether to plot the graphs in 1 shot

x= "2560633"


speaker$store_id <- as.factor(speaker$store_id) # Convert store id to factor
spc=speaker # New dataframe spc for redundancy
filter <-spc$product_id == x 
id2<- spc[filter,]  
data_pricing_wide <-
  dcast(id2, store_id ~ date, 
        value.var = "cpi_adjusted_price")
data_pricing_wide = as.data.frame(data_pricing_wide)
row.names(data_pricing_wide) <- data_pricing_wide$store_id
data_pricing_wide$store_id <- NULL
#Function provided by Charlie
dtwOmitNA <-function (x,y)
{
  a<-na.omit(x)
  b<-na.omit(y)
  return(dtw(a,b,distance.only=TRUE)$normalizedDistance)
}

#disctance calculation code provided by Charlie
d <- dist(data_pricing_wide, method = "dtwOmitNA")
d
#=============================
cluster_solution <- hclust(d)
cluster_solution
#Plotting cluster dendogram
plot(cluster_solution,
     ylab = "Height",
     xlab = "Store ID - Distance Calculation",
     cex = 0.9,
     hang = -1,
     col = "blue",
)
rect.hclust(cluster_solution, k =3,border = "red")
#Another view of cluster for report

dend<-fviz_dend(cluster_solution, k = 3, # Cut in four groups cex = 0.5, # label size
                k_colors = c( "#00AFBB", "#E7B800", "#FC4E07"), color_labels_by_k = TRUE,
                ggtheme = theme_minimal(),
                rect = TRUE, # Add rectangle around groups
                rect_border = c("#00AFBB", "#E7B800", "#FC4E07"), rect_fill = TRUE,
                horiz = TRUE,cex = 1.5,hang=-1,lwd=1.7,main = "Cluster Dendrogram : Product ID 1121458",
)


dend+
  theme(axis.text.x = element_text(size = 20, color = "black"),
        title = element_text(size = 25, color = "black"))

koibhi <- as.data.frame(cutree(cluster_solution,k=3)) # New DF to cut cluster
colnames(koibhi) <- c("Cluster") # rename row in dataframe koibhi
koibhi # Show clusters
colnames(koibhi) #Check number of columns in Df

#======================
#Merge the two dataframes to match cluster with store ids and back to pivot long format
m4 <- merge(data_pricing_wide,koibhi,by=0)
colnames(m4)
m6 <-pivot_longer(m4, -c(Cluster, Row.names), values_to = "Prices", names_to = "date",values_drop_na = TRUE)
colnames(m6) <- c("store_id","Cluster","date","Prices")
m6 <-  na.omit(m6)
m6$date<-as.Date(m6$date)
#Plot the clustered timeseries

group.colors <- c("red","blue","black")
ggplot(m6,aes(date,Prices,color=as.factor(Cluster)))+
  geom_step(size=0.8,alpha=0.7)+theme_minimal()+
  theme(legend.position="none")+
  scale_x_date(date_labels = "%b-%Y")+
  xlab("Dates")+ylab("CPI adjusted Prices")+
  scale_colour_manual(values=group.colors)


####### Other view of this plot
My_Theme <- theme(
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 14,),
  axis.text.y = element_text(size = 12),
)
Mp<-ggplot(m6,aes(date,Prices,color=as.factor(store_id)))+
  geom_step(size=0.8)+
  facet_wrap(~Cluster,labeller = label_both)+
  theme_ipsum()+
  theme(legend.position="none")+
  ggtitle("Clustered View : Product ID 155663 ")+
  scale_x_date(date_labels = "%b-%Y",guide = guide_axis(angle = 90))+
  xlab("Dates")+ ylab("CPI adjusted prices")

Mp+ My_Theme1
