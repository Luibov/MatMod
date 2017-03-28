LETTERS
abc = sample(LETTERS, 10000, T)
v=0
for(i in 1:length(abc)) 
{if(abc[i]=="A"|abc[i]=="E"|abc[i]=="I"|abc[i]=="O"|abc[i]=="U"|abc[i]=="Y") 
{v=v+1} else {v=v+0}} 
v
