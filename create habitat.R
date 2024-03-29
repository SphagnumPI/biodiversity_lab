library(raster)
library(ggplot2)
library(cowplot)
library(colorRamps)
library(reshape2)

#####View community structure
habitat=raster("com.tif")   #load site raster

#####convert community structure to data frame
df=as.data.frame(rasterToPoints(habitat)) #convert site raster to a data frame
df$x=ceiling(df$x)
df$y=ceiling(df$y)

####create another 
unique(df$com)   #check what the numbers which apply to the communities in your site


index=data.frame(com=unique(df$com),   #create an index of site numbers
                 community=c(                 #and the communities
                       "CG2",
                       "CG3d",
                       
                       "CG5"),stringsAsFactors = FALSE)

df=merge(df,index,by="com")  #merge index with your site



####what do the communities look like produces a figure that how the communities will be distributed in all sites

together=ggplot(data=df,aes(x=x,y=y,fill=(community)))+
  geom_raster()+
  theme_cowplot()+
  scale_fill_manual(limits=c(
    "CG3d",
    "CG2",
    "CG5"),
    values=c(
      "red",
      "blue",
      
      "forestgreen"))+
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())+
  labs(fill="Community")

####Create Communities
communities=read.csv(list.files(pattern = "communities.csv"), #load species data with numbers and 
                     stringsAsFactors = FALSE)                #frequencies in habitats  

communities=split(communities,communities$community)  #split data frame to constituant communities

communities=lapply(communities,function(q){
  prob=q$freq/sum(q$freq)             #create a probablility vector based on proportion frequency in whole
  return(data.frame(q,                #community and produce data frame.
                    prob))
})

df=split(df,df$community)                            #split community dataframe by communities 
sp_rich_even=df                                      #create most species rich and even site
for (i in which(names(df)%in%names(communities))){
  com=names(df)[i]                                   #which community
  aa=df[[com]]                                       #Which cells in matrix apply to that community
  aa$sp=sample(x=communities[[com]]$sp.num,          #Select species for a cell
               size=length(aa[,1]),                  #just 1
               replace = TRUE,                       
               prob=communities[[com]]$prob)         #probablility of selection
  
  sp_rich_even[[i]]=aa                               #Replace new data 
  }

sp_rich_even=do.call("rbind",sp_rich_even)           #Turn list to data frame

#plot the species rich even site
rich_even=ggplot(data=sp_rich_even,aes(x=x,y=y,fill=(sp)))+
  geom_raster()+
  #ggtitle("Species rich even habitat")+
  theme_cowplot()+
  scale_fill_gradientn(colours = matlab.like2(100))+
  theme(strip.background = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())
rich_even

summry=table(sp_rich_even$community,sp_rich_even$sp)       #create contingency table of species and whole site data per community
df_sum=data.frame(site="rich_even",comm=rownames(summry),  #create data frame summarising Biodiveristy indicies
  richness=vegan::specnumber(summry),                      #How many species per community
shannon=vegan::diversity(summry,"shannon"),                #What is the shannon index of site communities
simpson=vegan::diversity(summry,"simpson"))                #Simpson index

df_sum=rbind(data.frame(site="rich_even",comm="ALL",                  #summarise data for WHOLE Site 
richness=vegan::specnumber(summary(factor(sp_rich_even$sp))),         #and bind to per community data
shannon=vegan::diversity(summary(factor(sp_rich_even$sp)),"shannon"),
simpson=vegan::diversity(summary(factor(sp_rich_even$sp)),"simpson")),
df_sum)


sp_rich_even=(dcast(data=sp_rich_even,x~y,value.var = "sp"))       #create original matrix with species data in the cells
row.names(sp_rich_even)=sp_rich_even$x
sp_rich_even=sp_rich_even[,-1]

##Create species rich uneven site see othe commments
sp_rich_uneven=df                                  
  for(i in 1:length(sp_rich_uneven)){
  nam=names(sp_rich_uneven)[i]
  com=communities[[nam]]
  aa=sp_rich_uneven[[nam]]
  aa$sp=sample(com$sp.num,size=length(aa$com),replace = TRUE,com$prob)
  
  n=sample(1:length(aa$com),                   
           round(length(aa$com)*0.3),              #choose random 30% of cells for a community
           replace = FALSE)

  aa$sp[n]=sample(com$sp.num,size=1,replace = FALSE,com$prob)   #replace 30% with high frequency species
  sp_rich_uneven[[i]]=aa
  }
sp_rich_uneven=do.call("rbind",sp_rich_uneven)

rich_uneven=ggplot(data=sp_rich_uneven,aes(x=x,y=y,fill=(sp)))+
  geom_raster()+
  #ggtitle("Species rich uneven habitat")+
  theme_cowplot()+
  scale_fill_gradientn(colours = matlab.like2(100))+
  theme(strip.background = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

rich_uneven

summry=table(sp_rich_uneven$community,sp_rich_uneven$sp)

df_sum=rbind(df_sum,
             data.frame(site="rich_uneven",comm="ALL",
                        richness=vegan::specnumber(summary(factor(sp_rich_uneven$sp))),
                        shannon=vegan::diversity(summary(factor(sp_rich_uneven$sp)),"shannon"),
                        simpson=vegan::diversity(summary(factor(sp_rich_uneven$sp)),"simpson")))


df_sum=rbind(df_sum,
             data.frame(site="rich_uneven",comm=rownames(summry),
                        richness=vegan::specnumber(summry),
                        shannon=vegan::diversity(summry,"shannon"),
                        simpson=vegan::diversity(summry,"simpson")))

sp_rich_uneven=(dcast(data=sp_rich_uneven,x~y,value.var = "sp"))
row.names(sp_rich_uneven)=sp_rich_uneven$x
sp_rich_uneven=sp_rich_uneven[,-1]

sp_poor_even=df
for(i in 1:length(sp_poor_even)){
  nam=names(sp_poor_even)[i]
  com=communities[[nam]]
  com=com[sample(1:length(com$sp),
                 size=length(com$sp)*0.6,         #remove 60% of speices
                 replace = FALSE),]
  com$prob=com$freq/sum(com$freq)                 #recalculate probabilities
  
  aa=sp_poor_even[[nam]]
  aa$sp=sample(com$sp.num,
               size=length(aa$com),
               replace = TRUE,
               prob=com$prob)
  
  
  sp_poor_even[[i]]=aa
}

sp_poor_even=do.call("rbind",sp_poor_even)

poor_even=ggplot(data=sp_poor_even,aes(x=x,y=y,fill=(sp)))+
  geom_raster()+
  #ggtitle("Species poor even habitat")+
  theme_cowplot()+
  scale_fill_gradientn(colours = matlab.like2(100))+
  theme(strip.background = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

poor_even

summry=table(sp_poor_even$community,sp_poor_even$sp)
df_sum=rbind(df_sum,
             data.frame(site="sp_poor_even",comm="ALL",
                        richness=vegan::specnumber(summary(factor(sp_poor_even$sp))),
                        shannon=vegan::diversity(summary(factor(sp_poor_even$sp)),"shannon"),
                        simpson=vegan::diversity(summary(factor(sp_poor_even$sp)),"simpson")))


df_sum=rbind(df_sum,
             data.frame(site="sp_poor_even",comm=rownames(summry),
                        richness=vegan::specnumber(summry),
                        shannon=vegan::diversity(summry,"shannon"),
                        simpson=vegan::diversity(summry,"simpson")))




sp_poor_even=(dcast(data=sp_poor_even,x~y,value.var = "sp"))
row.names(sp_poor_even)=sp_poor_even$x
sp_poor_even=sp_poor_even[,-1]


sp_poor_uneven=df
for(i in 1:length(sp_poor_uneven)){
  nam=names(sp_poor_uneven)[i]
  com=communities[[nam]]
  com=com[sample(1:length(com$sp),
                 size=length(com$sp)*0.6,
                 replace = FALSE),]
  com$prob=com$freq/sum(com$freq)
  aa=sp_poor_uneven[[nam]]
  n=sample(1:length(aa$com),
           round(length(aa$com)*0.3),       #uneveness as before 30% of cells
           replace = FALSE)
  
  aa$sp=sample(com$sp.num,
               size=length(aa$com),
               replace = TRUE,
               prob=com$prob)
  
  aa$sp[n]=sample(com$sp.num,size=1,replace = FALSE,com$prob)
  
  sp_poor_uneven[[i]]=aa
  rm(aa)
}

sp_poor_uneven=do.call("rbind",sp_poor_uneven)

poor_uneven=ggplot(data=sp_poor_uneven,aes(x=x,y=y,fill=(sp)))+
  geom_raster()+
  #ggtitle("Species poor uneven habitat")+
  theme_cowplot()+
  scale_fill_gradientn(colours = matlab.like2(100))+
  theme(strip.background = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

poor_uneven
summry=table(sp_poor_uneven$community,sp_poor_uneven$sp)

df_sum=rbind(df_sum,
             data.frame(site="sp_poor_uneven",comm="ALL",
                        richness=vegan::specnumber(summary(factor(sp_poor_uneven$sp))),
                        shannon=vegan::diversity(summary(factor(sp_poor_uneven$sp)),"shannon"),
                        simpson=vegan::diversity(summary(factor(sp_poor_uneven$sp)),"simpson")))

df_sum=rbind(df_sum,
             data.frame(site="sp_poor_uneven",comm=rownames(summry),
                        richness=vegan::specnumber(summry),
                        shannon=vegan::diversity(summry,"shannon"),
                        simpson=vegan::diversity(summry,"simpson")))

sp_poor_uneven=(dcast(data=sp_poor_uneven,x~y,value.var = "sp"))
row.names(sp_poor_uneven)=sp_poor_uneven$x
sp_poor_uneven=sp_poor_uneven[,-1]

habitat_fig=plot_grid(rich_even,rich_uneven,poor_even,poor_uneven,ncol=2,labels = c("(i)",  #plot of sites all together
                                                                        "(ii)",
                                                                        "(iii)",
                                                                        "(iv)"))
habitat_fig
rm(com,i,n,summry,top,pg)
row.names(df_sum)=NULL
df_sum
write.csv(df_sum,"site_summary.csv",row.names = FALSE)     #write site summaries
##plot site summaries
ggplot(data=melt(df_sum,id=c("site","comm")),aes(x=comm,y=value,colour=site))+
  facet_wrap(~variable,scale="free")+
  geom_point()


####Write sites as excel spreadsheets
write.csv(as.matrix(sp_poor_even),file="sp poor even.csv",row.names = TRUE)
write.csv(as.matrix(sp_poor_uneven),file="sp poor uneven.csv",row.names = TRUE)
write.csv(as.matrix(sp_rich_even),file="sp rich even.csv",row.names = TRUE)
write.csv(as.matrix(sp_rich_uneven),file="sp rich uneven.csv",row.names = TRUE)

