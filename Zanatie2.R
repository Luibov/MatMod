z= iris
z
z$Species=NULL
z
z[1,1:4]
sum(z[1,1:4])
sum(z[1,1:4])/4
y= 0
for(i in 1:150)
{x=sum(z[i,1:4])/4
y=c(x,y)}
y
