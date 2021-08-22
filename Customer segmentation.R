#packages
install.packages("NbClust")
install.packages("NbClust")

library(readr)
library(RColorBrewer) #for graphs
library(tidyverse) #for code processing (dplyr)
library(hrbrthemes)#for ggplot
library(purrr) #for k neighbors
library(cluster) #for the average silhouette
library(gridExtra)#for the average silhouette
library(grid)#for the average silhouette
library(NbClust) #visualize the optimal number of clusters
library(factoextra)#visualize the optimal number of clusters
hrbrthemes::import_roboto_condensed()


#Load the data
customer_data = read.csv("Mall_Customers.csv")

#-------------------Check the data--------------------------------

str(customer_data)
names(customer_data)

head(customer_data)
summary(customer_data)


sd(customer_data$Age)
sd(customer_data$Annual.Income..k..)
sd(customer_data$Spending.Score..1.100.)

#-----------------Visualization of CUSTOMER GENDER---------------------

a = table(customer_data$Gender)

a #so we can easily graph this

coul <- brewer.pal(5, "Set2")

barplot(a, 
        main = "Customer Gender",
        ylab = "Count",
        xlab = "Gender",
        col = coul, #or Rainbow(#) #found this later
        legend = rownames(a))


#Not bad but is, even easier with ggplot

ggplot(customer_data, aes(x = Gender, fill= Gender))+
  geom_bar(stat = "count")#+
  #scale_fill_hue(c = 40) to make the color darker

#We start seeing that female customers are already more than male ones. 
#what is the procentage?

b <- sum(a)
b
a/b*100

#So 56% are females and 44% are males

#------------------Visualization of AGE------------------------------

hist(customer_data$Age,
     col = "blue",
     main = "Age distribution",
     ylab = "frequency",
     xlab = "Age",
     label = TRUE)


#The histogram that we did is nice, but It is better to use ggplot for a more stylish graph
customer_data %>% ggplot(aes(x=Age)) +
  geom_histogram(fill= "blue", color="#e9ecef", alpha=0.9) + 
  ggtitle("Age of Cutomers") #+
  #theme_ipsum()+#
  #theme(   
    #plot.title = element_text(size=15)
     #   )

#run a small boxplot

boxplot(customer_data$Age, 
        col = "#e9ecef",
        main = "Costumer age summary" )

#From this, we can se that the mayority of coustumers are in their 30s

#-----------------------Analysis of the ANNUAL INCOME of the Customers-----------------------

customer_data %>% ggplot(aes(x = Annual.Income..k..))+
  geom_histogram(binwidth = 5, fill = "#770033", color="#e9ecef", alpha = 0.7)+
  ggtitle("Customer's Annual Income")

#geom density
customer_data %>% ggplot(aes(x = Annual.Income..k..))+
  geom_density(fill = "#770033", color="#e9ecef", alpha = 0.7)+
  ggtitle("Customer's Annual Income")


#From both graphs, we see that the mayority of clients have a salary betwen 50k and 80k

#----------------Analyzing SPENDING SCORE of the Customers---------------

customer_data %>% ggplot(aes(x = Spending.Score..1.100.))+
  geom_histogram(binwidth = 5, fill = "#1eb496", color = "black", alpha = 0.7)+
  ggtitle("Spending Score")

customer_data %>% ggplot(aes(x = Spending.Score..1.100.))+
  geom_density(fill = "#1eb496", color = "black", alpha = 0.7)+
  ggtitle("Spending Score")

#The spending score is around 25 and 75 points. The most part at 50 points.

#------------------------K-means Algorithm-------------------------------

set.seed(32)

iss <- function(k){
  
  kmeans(customer_data[ ,3:5], 
         k,
         iter.max=100,
         nstart=100,
         algorithm="Lloyd")$tot.withinss
  #use the elbow method to find the location of a bend or a knee for the optimum number of clusters.
}

k.values <-1:10

iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

#4 seems to be the appropriate number of clusters. Appears that the bend starts at that point


#------------------Average Silhouette Method-------------------------

#Use this method to check if 4 is indeed the best method

#Let's start with k = 2
k2<-kmeans(customer_data[,3:5], 
           2, 
           iter.max=100, 
           nstart=50,
           algorithm="Lloyd")

s2<-plot(silhouette(k2$cluster,
                    dist(customer_data[,3:5],
                         "euclidean")))

#with k = 3

k3 <- kmeans(customer_data[ , 3:5],
             3,
             iter.max = 100,
             nstart =  50,
             algorithm =  "Lloyd")

s3 <- plot(silhouette(k3$cluster,
                      dist(customer_data[, 3:5],
                           "euclidean")))


k4 <- kmeans(customer_data[ , 3:5],
             4,
             iter.max = 100,
             nstart =  50,
             algorithm =  "Lloyd")

s4 <- plot(silhouette(k4$cluster,
                      dist(customer_data[, 3:5],
                           "euclidean")))

k5 <- kmeans(customer_data[ , 3:5],
             5,
             iter.max = 100,
             nstart =  50,
             algorithm =  "Lloyd")

s5 <- plot(silhouette(k5$cluster,
                      dist(customer_data[, 3:5],
                           "euclidean")))

k6 <- kmeans(customer_data[ , 3:5],
             6,
             iter.max = 100,
             nstart =  50,
             algorithm =  "Lloyd")

s6 <- plot(silhouette(k6$cluster,
                      dist(customer_data[, 3:5],
                           "euclidean")))
k7 <- kmeans(customer_data[ , 3:5],
             7,
             iter.max = 100,
             nstart =  50,
             algorithm =  "Lloyd")

s7 <- plot(silhouette(k7$cluster,
                      dist(customer_data[, 3:5],
                           "euclidean")))
k8 <- kmeans(customer_data[ , 3:5],
             8,
             iter.max = 100,
             nstart =  50,
             algorithm =  "Lloyd")

s8 <- plot(silhouette(k8$cluster,
                      dist(customer_data[, 3:5],
                           "euclidean")))
k9 <- kmeans(customer_data[ , 3:5],
             9,
             iter.max = 100,
             nstart =  50,
             algorithm =  "Lloyd")

s9 <- plot(silhouette(k9$cluster,
                      dist(customer_data[, 3:5],
                           "euclidean")))
#Graph it

fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")


k7 <- kmeans(customer_data[ , 3:5],
             7,
             iter.max = 100,
             nstart =  50,
             algorithm =  "Lloyd")
k7

pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[,1:2]

set.seed(1)
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k7$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6", "7"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6", "Cluster 7")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")


#--------------Cluster interpretation-------------------------------

#Cluster 6 and 7 - These clusters represent the customer_data with the medium income salary as well as the medium annual spend of salary.

#Cluster 1 - customers having a high annual income as well as a high annual spend.

#Cluster 3 -customers  with low annual income as well as low yearly spend of income.

#Cluster 2 -customers with high annual income and low yearly spend.

#Cluster 5 - customers with low annual income but its high yearly expenditure.

#Now we have a better understanding of how the customers are divided and we can create plans to get
# to this costumers according to their age, annual income and spending score. Of course, morevariables
#can be useful: still need to transform gender to an integer, reviews of products, satisfaction of clients,
#how do the customers pay (card, effective), what do they buy, and so on...
