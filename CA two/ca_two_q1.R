data <- read.csv("http://users.stat.ufl.edu/~winner/data/ship_emissions2.csv") 

x1 =data$NO_x   # independent variable 
x2 =data$SO_2   # independent variable 
x3 =data$PM_10  # independent variable  
y  =data$Fuel   # dependent variable  

table(data$Fuel)  # this function will help to identify the family  

# data cleaning: removing missing values
dataset <- na.omit( data.frame(x1,x2,x3,y) ) 

# Fitting the Model  
fit = glm (y~x1+x2+x3,data = dataset, family = 'gaussian')  # linear regression as Fuel is a continuous variable  
summary(fit) 



# split the dataset to trainset and testset 

n=nrow(dataset) 
indexes = sample(n, n*(80/100)) 
trainset= dataset[indexes,] 
testset = dataset[-indexes,] 

# Fitting the Model    
fit = glm (n~p+m, data = trainset, family = 'gaussian')  # linear regression as Fuel is a continuous variable   
pred=predict(fit, testset) 