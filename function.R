#Script to find out appropriate factor(X) to be multiplied with Inter quartile range(iqr)
#to detect outliers
#Mild outlier........> Q3+1.5*iqr or <Q1-1.5*iqr (till extreme outlier range)
#Extreme outlier.....> Q3+3*iqr or <Q1-3*iqr


initial_data = read.table(file.choose(), head = TRUE, sep = "\t")

iqr1 = IQR(data_avail$testSample1)								#Inter quartile range
igr2 = IQR(data_avail$testSample2)

													
X = 1.5												#factor									
													#SAMPLE 1
for(j in seq(0, 1.5, .3))									#j start from 0, till 1.5, increment of .3
{

count_1 = 0
count_2 = 0

for(i in 1:nrow(data_avail))     								#going through every data point
{													#counting outliers outside whiskers of the box
	if( data_avail[i,"testSample1"]-quantile(data_avail$testSample1,3/4) > (X+j) * iqr1)
 	{count_1 = count_1+1
  }
 	else if( quantile(data_avail$testSample1,3/4) - data_avail[i,"testSample1"] > (X+j) * iqr1)
 	{count_2 = count_2+1
  }
 	
}
	if( count_1 + count_2 <= 108)								#Why 108.......*						
	{break
  }
	
}


count_1
count_2
X
j

													#SAMPLE 2
for(j in seq(0, 1.5, .3))
{

count_1 = 0
count_2 = 0

for(i in 1:nrow(data_avail))     
{
	if( data_avail[i,"testSample2"]-quantile(data_avail$testSample2,3/4) > (X+j) * iqr1)
 	{count_1 = count_1+1
  }
 	else if( quantile(data_avail$testSample2,3/4) - data_avail[i,"testSample2"] > (X+j) * iqr1)
 	{count_2 = count_2+1
  }
}
	if( count_1 + count_2 <= 108)
	{break
  }

}


count_1
count_2
X
j





# * Usually only maybe as much as about one percent of the data will ever be outlier, which in case of normal bell shaped data
#   is about .7 percent of total data, which in our case is .7% of 15488.
