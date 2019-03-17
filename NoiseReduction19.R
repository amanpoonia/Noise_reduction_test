data_avail = read.table(file.choose(), head = TRUE, sep = "\t")								#Read given data
sample1_final = data_avail[0,]													#creating data frame for noise reduced data
sample2_final = data_avail[0,]
rows_final1 = 0																#rows in final data
rows_final2 = 0

boxplot(data_avail$testSample1~data_avail$chr, main="Available Sample1", xlab="chr" , ylab = "Log Ratio")	#Graphical represenation
boxplot(data_avail$testSample2~data_avail$chr, main="Available Sample2", xlab="chr" , ylab = "Log Ratio")	#Boxplot


d1 = density(data_avail$testSample1)                 										# returns the density data for sample1 
plot(d1) 																      # plots the results
polygon(d1, col="red", border="blue")
d2 = density(data_avail$testSample2) 												# returns the density data for sample2
plot(d2) 																	# plots the results
polygon(d2, col="blue", border="red")

																		#Inter quartile range
iqr1 = IQR(data_avail$testSample1)
iqr2 = IQR(data_avail$testSample2)

																		#SAMPLE1
X = 3																		#Found by function script


for(i in 1:nrow(data_avail))     
{
	if( data_avail[i,"testSample1"] - quantile(data_avail$testSample1,3/4) > X * iqr1)				#outliers not included
 	{
  }
 	else if( quantile(data_avail$testSample1,3/4) - data_avail[i,"testSample1"] > X * iqr1)
 	{
  }
	else
  	{sample1_final[rows_final1+1,] <- data_avail[i,]									#filling only acceptable data 
    	rows_final1 = rows_final1 + 1										
  }																		

}																		#Writing acceptable data points
write.csv(sample1_final, file = "noise_reduced_sample1.csv", row.names = TRUE)					#in new file


X = 2.7																	#SAMPLE2


for(i in 1:nrow(data_avail))    
{
	if( data_avail[i,"testSample2"] - quantile(data_avail$testSample2,3/4) > X * iqr2)
 	{
  }
 	else if( quantile(data_avail$testSample2,3/4) - data_avail[i,"testSample2"] > X * iqr2)
 	{
  }
	else
  	{sample2_final[rows_final2+1,] <- data_avail[i,]
    	rows_final2 = rows_final2 + 1
  }

}

write.csv(data_final, file = "noise_reduced_sample2.csv", row.names = TRUE)
boxplot(sample1_final$testSample1~sample1_final$chr, main="Final Sample1", xlab="chr" , ylab = "Log Ratio")	#Noise reduced data Boxplot
boxplot(sample2_final$testSample2~sample2_final$chr, main="Final Sample2", xlab="chr" , ylab = "Log Ratio")
