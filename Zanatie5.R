iris
x=iris
x$Sepal.Length=NULL
x$Sepal.Width=NULL
x$Petal.Width=NULL
x
x[1:50, 1]
versicolor=(x[51:100,1])
virginica= mean(x[101:150,1])
spec = c(versicolor, virginica)
spec
mean(x[1:50,1])
setosa= mean(x[1:50,1])
mean(x[51:100,1])
versicolor=mean[51:100,1])
versicolor= mean(x[51:100,1])
spec = c(setosa, versicolor, virginica)
spec
names(spec)= c ("setosa", "versicolor", "virginica")
spec
sort (spec)
