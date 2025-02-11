data=read.csv("/home/ibab/Downloads/Heart.csv",header = TRUE)

#print(data)

print(dim(data))

print(class(data$ChestPain))

data$ChestPain=factor(data$ChestPain,levels = c('typical','asymptomatic','nontypical'))

print(is.factor(data$ChestPain))
print(class(data$ChestPain))
print(unique(data$ChestPain))
print(levels(data$ChestPain))
print(nlevels(data$ChestPain))


Temp_col=c("Hot","Cold",'lukewarm')
temperature=gl(3,2,nrow(data),labels = Temp_col)
new_data=data.frame(data,temperature)
print(new_data)

print(pmin(data$Age,data$RestBP,data$Chol))
print(pmax(data$Age,data$RestBP,data$Chol))


ranks=rank(data$Chol)
sorted=sort(data$Chol)
ordered=order(data$Chol)
view=data.frame(data$Chol,ranks,sorted,ordered)
print(view)

view1=data.frame(sorted,data$RestBP[ordered])
print(view1)

print(data[203,])

write.csv(view1,file="’lab5 ordered data.csv")

sub_data=data[1:6,3:8]
print(sub_data)
sub_data_mat=as.matrix(sub_data)
print(attributes(sub_data_mat))


newcol=data$Age+data$RestBP+data$Chol
newcoladded=data.frame(data,newcol)
print(colnames(newcoladded))

newcoladded2=cbind(newcoladded,newcol)
print(colnames(newcoladded2))


new_rows=data[25:36,]
new_dataframe=rbind(data,new_rows)
print(dim(new_dataframe))
print(dim(data))


tapply(data$Chol, data$AHD, mean)
tapply(data$Chol, data$AHD, mean, trim=0.1)
tapply(data$Chol, data$Sex, mean, trim=0.1)
tapply(data$Chol, data$Thal, mean)
tapply(data$Chol, data$RestECG, mean)








