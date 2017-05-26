readAndClean <- function(file){
  df <- read.csv(file,na.strings=c(NA,""," "))
  df1<- df[,1:18]
  df1
}

#0. Count percentage of NAs
getPercentNAs <- function(df){
  l <- NULL 
  a <- dim(df)
  for(i in 1:a[2]){
    
    l[i] <- sum(is.na(df[,i]))
    print(colnames(df[i]))
    #print(format(round(l[i]*100/a[1],2),nsmall=2))
    p= l[i]*100/a[1]
    print(sprintf("%d   %2.2f %%",i, p))
  }
  
}

getEmptyColumns <- function(df){
  cols <- NULL
  l <- NULL 
  a <- dim(df)
  for(i in 1:a[2]){
    
    l[i] <- sum(is.na(df[,i]))
    #print(colnames(df[i]))
    
    p= l[i]*100/a[1]
    if(l[i] >=90){
      cols <- c(cols,i)
    }
  }
  cols
}

#0c. Check duplicates
checkDuplicates <- function(df){
  a <- df %>% distinct
  duplicates  <- dim(df)[1] - dim(a)[1] 
  dup <- paste("Number of duplicates is:",duplicates)
  print(dup)
}

# Size of dataframe
size <- function(df){
  a <- dim(df)
  print(paste("Size of dataframe is:",a[1],"x",a[2]),sep="")
}


listOrPlotTop10TotalFeat <- function(df1,feature,output,meanOrTotal="total",listOrPlot="list"){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  num2 <- which(n ==output)
  
  # Output column to be computed
  outputCol <- colnames(df1)[num2]
  
  # Use a string to select feature to summarize on meanOrTotal
  if(meanOrTotal == "total"){
    df2 <- df1 %>% select(num1,num2) %>% group_by_(colnames(df1)[num1])  %>%
      summarize_(feat=lazyeval::interp(~sum(var), var = as.name(outputCol)))
  }else {
    df2 <- df1 %>% select(num1,num2) %>% group_by_(colnames(df1)[num1])  %>%
      summarize_(feat=lazyeval::interp(~mean(var), var = as.name(outputCol)),
                 StdFeat=lazyeval::interp(~sd(var), var = as.name(outputCol))) 
  }
  
  # Format
  df2$feat <- format(round(df2$feat,2),nsmall=2) 
  # Convert to numeric
  df2$feat <- as.numeric(df2$feat)
  # Add percent column           
  df2 <- df2 %>%  mutate(Percent= format(round(feat*100/sum(feat),2),nsmall=2)) %>% 
    arrange(desc(Percent))
  top10 <- head(df2,10)
  
  totFeat <- paste("Total",output,sep="")
  meanFeat <- paste("Mean",output,sep="")
  stdDeviationFeat <- paste("StdDeviation",output,sep="")
  
  #Set the names of columns
  if(meanOrTotal == "total"){
    
    
    names(top10) <- c(colnames(df1)[num1],totFeat,"Percent")
    print(names(top10))
  } else {
    names(top10) <- c(colnames(df1)[num1],meanFeat,
                      stdDeviationFeat,"Percent")
    print(names(top10))
  }
  
  df2$Percent = paste(df2$Percent,"%",sep="")
  
  if(listOrPlot =="list"){
    #List top 10
    print("Here")
    top10
  } else {
    top10 <- head(df2,10)
    atitle =paste("Highest 10",meanOrTotal,output," for", feature)
    if(meanOrTotal == 'total'){
      #Total
      ggplot(top10,aes_string(x=paste0("reorder(",colnames(top10)[1],",-",colnames(top10)[2],")"),
                              y=colnames(top10)[2],fill=colnames(top10)[1]))+geom_bar(stat="identity") +
        scale_y_continuous(labels=comma) + 
        ggtitle(atitle)  + 
        xlab(feature) + ylab("Total TCV ($)") + 
        geom_text(aes_string(label=colnames(top10)[2]), vjust=-.3, color="black", size=4) +
        theme(plot.title = element_text(size=16, face="bold",hjust=0.5)) +
        theme(axis.text.x = element_text(size=14,angle = 90, hjust = 1)) +
        theme(text=element_text(size=14))
      
    } else {
      
      #Mean with error bar
      ggplot(top10,aes_string(x=paste0("reorder(",colnames(top10)[1],",-",colnames(top10)[2],")"),
                              y=colnames(top10)[2],fill=colnames(top10)[1]))+geom_bar(stat="identity") +
        geom_errorbar(width=.1, aes_string(ymin=paste0(colnames(top10)[2],"-",colnames(top10)[3]), 
                                           ymax=paste0(colnames(top10)[2],"+",colnames(top10)[3]))) +
        scale_y_continuous(labels=comma) + 
        ggtitle(atitle)  + 
        xlab(feature) + ylab("Mean TCV ($)") + 
        geom_text(aes_string(label=colnames(top10)[2]), vjust=-.3, color="black", size=4) +
        theme(plot.title = element_text(size=16, face="bold",hjust=0.5)) +
        theme(axis.text.x = element_text(size=14,angle = 90, hjust = 1)) +
        theme(text=element_text(size=14))
    }
  }
  
  
}

# Boxplot and Freq poly plots
plotBoxFreqPolyFeat <- function(df1,feature,output,plotType="boxplot"){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  num2 <- which(n ==output)
  
  # Output column to be computed
  outputCol <- colnames(df1)[num2]
  
  df2 <- df1 %>% select(num1,num2) %>% group_by_(colnames(df1)[num1])  %>%
    summarize_(feat=lazyeval::interp(~sum(var), var = as.name(outputCol)))
  
  # Format
  df2$feat <- format(round(df2$feat,2),nsmall=2) 
  # Convert to numeric
  df2$feat <- as.numeric(df2$feat)
  # Add percent column           
  df2 <- df2 %>%  mutate(Percent= format(round(feat*100/sum(feat),2),nsmall=2)) %>% 
    arrange(desc(Percent))
  
  totalFeat <- head(df2,10)
  names(totalFeat) <- c("feat",output,"Percent")
  featTotal <- totalFeat$feat
  
  df3 <- df1 %>% filter_(lazyeval::interp(~col %in% featTotal, 
                                          col=as.name(colnames(df1)[num1])))
  
  atitle =paste(output," for", feature)
  
  if(plotType =="boxplot"){
    ggplot(df3,aes_string(x=colnames(df1)[num1],y=output,col=colnames(df1)[num1])) + 
      geom_boxplot() +
      scale_y_continuous(labels=comma) + 
      ggtitle(atitle)  + ylab(output) +
      theme(axis.text.x = element_text(size=16,angle = 70, hjust = 1)) +
      theme(plot.title = element_text(size=16, face="bold",hjust=0.5)) 
  } else {
    
    
    ggplot(df3, aes_string(x=colnames(df1)[num2],col=colnames(df1)[num1])) +
      geom_freqpoly(bins = 20,size=1) +
      scale_x_continuous(labels=comma) +
      ggtitle(atitle) + xlab(output) + ylab("Count") +
      theme(plot.title = element_text(size=16, face="bold",hjust=0.5)) +
      theme(axis.text.x = element_text(size=16)) +
      theme(text=element_text(size=14)) 
  }
  
  
}


#1. List  count
listCounts <- function(df1,feature){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  
  df2 <- df1 %>% select(num1) %>% 
    group_by_(colnames(df1)[num1]) %>% summarise(Count=n()) %>% 
    arrange(desc(Count))
  Count <- head(df2,10)
  Count
  
  
}



#2. Plot counts
plotCounts <- function(df1, feature){
  # Get the column names
  n <-colnames(df1)
  # Determine the column number
  num1 <- which(n ==feature)
  
  df2 <- df1 %>% select(num1) %>% 
    group_by_(colnames(df1)[num1]) %>% summarise(Count=n()) %>% 
    arrange(desc(Count))
  Count <- head(df2,10)
  names(Count) <- c("feat","Count")
  
  atitle =paste("Total count ", feature)
  ggplot(Count,aes(x=reorder(feat,-Count),y=Count,
                   fill=feat)) + geom_bar(stat="identity") +
    ggtitle(atitle)  +
    scale_y_continuous(labels=comma) + 
    xlab(feature) + ylab("Total count") + 
    geom_text(aes(label=Count), vjust=-.3, color="black", size=4) +
    theme(plot.title = element_text(size=12, face="bold",hjust=0.5)) +
    theme(axis.text.x = element_text(size=12,angle = 90, hjust = 1)) +
    theme(text=element_text(size=16))
}



