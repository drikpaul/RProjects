#cleaning the outlier
value_mean=mean(debt_3$CreditorsDueWithinOneYear_Avg)
value_sd=sd(debt_3$CreditorsDueWithinOneYear_Avg)
benchmark_positive= value_mean + ( 2 * value_sd )
benchmark_negative= value_mean - ( 2 * value_sd )


cluster_itr_1 = subset(debt_3,debt_3$CreditorsDueWithinOneYear_Avg >= benchmark_negative & debt_3$CreditorsDueWithinOneYear_Avg <= benchmark_positive)

value_mean=mean(cluster_itr_1$CreditorsDueWithinOneYear_Avg)
value_sd=sd(cluster_itr_1$CreditorsDueWithinOneYear_Avg)
benchmark_positive= value_mean + ( 2 * value_sd )
benchmark_negative= value_mean - ( 2 * value_sd )

value_mean/value_sd

cluster_itr_2 =  subset(cluster_itr_1,cluster_itr_1$CreditorsDueWithinOneYear_Avg >= benchmark_negative & cluster_itr_1$CreditorsDueWithinOneYear_Avg <= benchmark_positive)

value_mean=mean(cluster_itr_2$CreditorsDueWithinOneYear_Avg)
value_sd=sd(cluster_itr_1$CreditorsDueWithinOneYear_Avg)
benchmark_positive= value_mean + ( 2 * value_sd )
benchmark_negative= value_mean - ( 2 * value_sd )

cluster_itr_3 =  subset(cluster_itr_2,cluster_itr_2$CreditorsDueWithinOneYear_Avg >= benchmark_negative & cluster_itr_2$CreditorsDueWithinOneYear_Avg <= benchmark_positive)

value_mean=mean(cluster_itr_3$CreditorsDueWithinOneYear_Avg)
value_sd=sd(cluster_itr_3$CreditorsDueWithinOneYear_Avg)
benchmark_positive= value_mean + ( 2 * value_sd )
benchmark_negative= value_mean - ( 2 * value_sd )


cluster_itr_4 =  subset(cluster_itr_3,cluster_itr_3$CreditorsDueWithinOneYear_Avg >= benchmark_negative & cluster_itr_3$CreditorsDueWithinOneYear_Avg <= benchmark_positive)

value_mean=mean(cluster_itr_4$CreditorsDueWithinOneYear_Avg)
value_sd=sd(cluster_itr_4$CreditorsDueWithinOneYear_Avg)
benchmark_positive= value_mean + ( 2 * value_sd )
benchmark_negative= value_mean - ( 2 * value_sd )

cluster_itr_5 =  subset(cluster_itr_4,cluster_itr_4$CreditorsDueWithinOneYear_Avg >= benchmark_negative & cluster_itr_4$CreditorsDueWithinOneYear_Avg <= benchmark_positive)

value_mean=mean(cluster_itr_5$CreditorsDueWithinOneYear_Avg)
value_sd=sd(cluster_itr_5$CreditorsDueWithinOneYear_Avg)
benchmark_positive= value_mean + ( 2 * value_sd )
benchmark_negative= value_mean - ( 2 * value_sd )

cluster_itr_6 =  subset(cluster_itr_5,cluster_itr_5$CreditorsDueWithinOneYear_Avg >= benchmark_negative & cluster_itr_5$CreditorsDueWithinOneYear_Avg <= benchmark_positive)

value_mean=mean(cluster_itr_6$CreditorsDueWithinOneYear_Avg)
value_sd=sd(cluster_itr_6$CreditorsDueWithinOneYear_Avg)
benchmark_positive= value_mean + ( 2 * value_sd )
benchmark_negative= value_mean - ( 2 * value_sd )

cluster_itr_7 =  subset(cluster_itr_6,cluster_itr_6$CreditorsDueWithinOneYear_Avg >= benchmark_negative & cluster_itr_6$CreditorsDueWithinOneYear_Avg <= benchmark_positive)

value_mean=mean(cluster_itr_7$CreditorsDueWithinOneYear_Avg)
value_sd=sd(cluster_itr_7$CreditorsDueWithinOneYear_Avg)
benchmark_positive= value_mean + ( 2 * value_sd )
benchmark_negative= value_mean - ( 2 * value_sd )


cluspart_filtered=cluster_itr_1 
cluspartSt=scale(cluspart_filtered[,14:14])   #indicate column name 
# FORM A SCREE PLOT TO SEE OPTIMAL NUMBER OF CLUSTERS :-
wss1= (nrow(cluspartSt)-1)*sum(apply(cluspartSt,2,var))
for (i in 1:3) wss1[i] <- sum(kmeans(cluspartSt,centers=i)$withinss)
plot(1:3, wss1, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# FORM THE CLUSTER METHOD :-
cluspart_kmeans=kmeans(x=cluspart_filtered[,14:14],centers=3)
#CHECK THE SIZE OF EACH CLUSTER :-




cluspart_kmeans$size
# ASSIGN THE CLUSTERS :-
clusters=cluspart_kmeans$cluster
# FORM THE CLUSTERS :-
cluster1=cluspart_filtered[clusters==1,]
cluster2=cluspart_filtered[clusters==2,]
cluster3=cluspart_filtered[clusters==3,]




# DATA DESCRIPTIONS FOR EACH CLUSTER :-
out1=data.frame(describe(cluster1[,14:14]))
out2=data.frame(describe(cluster2[,14:14]))
out3=data.frame(describe(cluster3[,14:14]))

# PLOT OF THE 3 CLUSTERS :-
clusplot(x=as.matrix(cluspart_filtered[,14:14]), cluspart_kmeans$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

#export the cluster data

write.xlsx(cluster1,"C:/Users/Debayan/Dropbox/Analytics-Work-Folder/Steve Williams/clusters/CreditorsDueWithinOneYear/CreditorsDueWithinOneYear_Avg/cluster1.xlsx")
write.xlsx(cluster2,"C:/Users/Debayan/Dropbox/Analytics-Work-Folder/Steve Williams/clusters/CreditorsDueWithinOneYear/CreditorsDueWithinOneYear_Avg/cluster2.xlsx")
write.xlsx(cluster3,"C:/Users/Debayan/Dropbox/Analytics-Work-Folder/Steve Williams/clusters/CreditorsDueWithinOneYear/CreditorsDueWithinOneYear_Avg/cluster3.xlsx")
