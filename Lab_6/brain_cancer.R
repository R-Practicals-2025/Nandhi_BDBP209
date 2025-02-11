data1=read.csv("/home/ibab/Downloads/BrainCancer.csv",header = TRUE)

#print(data1)

print(dim(data1))
print(length(data1))

print(data$sex)
data1$sex=factor(data1$sex,levels = c("Male","Female"))

print(is.factor(data1$sex))
print(class(data1$sex))

print(levels(data1$sex))
print(nlevels(data1$sex))

print(gl(4,3))
print(gl(4,3,24))
print(gl(4,3,5))


temp <- gl(3,8,24,labels = c(0,1,2))
print(temp)

temp1 <- gl(3,8,24,labels = c("Hard","Medium","Soft"))
print(temp1)


fac_df=data.frame(temp,temp1)
print(fac_df)

Temp_col=c("Hot","Cold",'lukewarm')
temperature=gl(3,2,nrow(data1),labels = Temp_col)
new_data=data.frame(data1,temperature)
print(new_data)


#pmin, pmax

print(pmin(data1$gtv,data1$time, data1$ki))

x1=c(1,2,3,4)
x2=c(5,6,7,8)
print(pmax(x1,x2))


##dif btw rank, sort, order
ranks=rank(data1$gtv)
sorted=sort(data1$gtv)
ordered=order(data1$gtv)

view=data.frame(data1$gtv,ranks,sorted,ordered)
print(view)

print(data1$gtv[ordered])

#data frmae into matrix
filter1=data1[1:6,3:8]
filter1_matrix=as.matrix(filter1)
print(filter1_matrix)


print(class(filter1_matrix))
#print(mode(filter1_matrix))
print(attributes((filter1_matrix)))


newcol=data1$ki+data1$gtv+data1$time
new_col=data.frame(data1,newcol)
print(new_col)

print(colnames(new_col))
new2=cbind(data1,newcol)
print(new2)


filt2=data1[c(1,3,8),]
new3=rbind(data1,filt2)
print(new3)

new_rows=data1[25:36,]
new_dataframe=rbind(data1,new_rows)
print(dim(new_dataframe))
print(dim(data1))

tapply(data1$gtv, data1$ki, mean)
tapply(data1$gtv, data1$ki, mean, trim=0.1)
tapply(data1$gtv, data1$sex, mean)
tapply(data1$gtv, data1$diagnosis, mean)


#names of rows & cols
x<- matrix(c(1,0,5,3,1,1,3,1,3,3,1,0,2,2,1,0),nrow=4)
print(x)
print(rownames(x))
print(colnames(x))

rownames(x)=rownames(x,do.NULL=FALSE,prefix='Trial.')
print(x)

drug=c("aspirin","paracetamol","dolo","other")
colnames(x)=drug
print(x)

x1 = matrix(c(1,0,5,3,1,1,3,1,3,3,1,0,2,2,1,0),nrow=4)
dimnames(x1)=list(NULL,paste("drug",1:4,sep=""))
print(x1)

#calculations on rows/cols of a matrix
print(mean(x[,4]))
print(var(x[4,]))
print(rowSums(x))
print(colSums(x))

print(apply(x,1,sum))
print(apply(x,2,sum))

print(x)
print(apply(x,2,sqrt))
print(apply(x,2,function(x)x^2+x))
print(apply(x,1,sqrt))

print(rowMeans(x))
print(colMeans(x))
print(apply(x,1,mean))

group=c("A","B","B","a")
print(rowsum(x,group))

print(row(x))
print(col(x))

print(tapply(x,list(group[row(x)],col(x)),sum))
print(aggregate(x,list(group),sum))

