
analyseData <- function(data){
  
  ra = nrow(data)
  col = ncol(data)
  halfdata = as.integer(ra/2)
  data_01 <- data[1:halfdata,]
  data_02 <- data[(halfdata+1):ra,]
  
  # plot the number of countries distribution per the survey
  # number of countries
  countries <- unique(data[,"Location of Leaving"])
  contryCount <- table(data[,"Location of Leaving"])
  countSlice <- rep(0,nrow(countries))
  
  for (i in 1:nrow(countries)){
    countSlice[i] = contryCount[toString(countries[i,])]
  }
  
  pie(countSlice,labels =  paste0(t(countries),"=",countSlice), col=rainbow(nrow(countries)), main="Pie Chart of Countries")
  
  #plot the distribution and count of occupation
  occupa <- unique(data[,"Occupation"])
  occupCount <- table(data[,"Occupation"])
  occupSlice <- rep(0,nrow(occupa))
  
  for (i in 1:nrow(occupa)){
    occupSlice[i] = occupCount[toString(occupa[i,])]
  } 
  pie(occupSlice,labels =  paste0(t(occupa),"=",occupSlice), col=rainbow(nrow(occupa)), main="Pie Chart of Occupation")
  
  
  #Plot the distribution and count of Age categories
  age <- unique(data[,"age"])
  ageCount <- table(data[,"age"])
  ageSlice <- rep(0,nrow(age))
  
  for (i in 1:nrow(age)){
    ageSlice[i] = ageCount[toString(age[i,])]
  } 
  pie(ageSlice,labels =  paste0(t(age),"=",ageSlice), col=rainbow(nrow(age)), main="Pie Chart of ages")
  
  
  #Plot the distribution and count of Length categories
  Length <- unique(data[,"Length"])
  LengthCount <- table(data[,"Length"])
  LengthSlice <- rep(0,nrow(Length))
  
  for (i in 1:nrow(Length)){
    LengthSlice[i] = LengthCount[toString(Length[i,])]
  } 
  pie(LengthSlice,labels =  paste0(t(Length),"=",LengthSlice), col=rainbow(nrow(Length)), main="Pie Chart of Length")
  
  
  #Plot the distribution and count of Weight categories
  weight <- unique(data[,"Weight"])
  weightCount <- table(data[,"Weight"])
  weightSlice <- rep(0,nrow(weight))
  
  for (i in 1:nrow(weight)){
    weightSlice[i] = weightCount[toString(weight[i,])]
  } 
  pie(weightSlice,labels =  paste0(t(weight),"=",weightSlice), col=rainbow(nrow(weight)), main="Pie Chart of Weight")
  
  
  
  #Plot the distribution and count of Gender categories
  gender <- unique(data[,"Gender"])
  genderCount <- table(data[,"Gender"])
  genderSlice <- rep(0,nrow(gender))
  for (i in 1:nrow(gender)){
    genderSlice[i] = genderCount[toString(gender[i,])]
  } 
  
  barplot(genderCount)
  #genderdf <- data.frame(rbind(genderCount))
  #ggplot(genderdf) + 
  #  geom_bar(stat = "identity") 
  
  pie(genderSlice,labels =  paste0(t(gender),"=",genderSlice), col=rainbow(nrow(gender)), main="Pie Chart of gender")
  
  # generate and calculate the validity of the questioner
  
  x = dim(data_01)
  y = dim(data_02)
  
  #corrlation of the two matrices 

  splitHalf = cor(data_01[1:x[1],9:col],data_02[1:x[1],9:col],method = "pearson") # use = "everything", 
  correlation = diag(splitHalf)
  data_01[,9:col]
  # Cronbach's alpha test 
  corn = cronbach.alpha(data[,9:col], CI = TRUE)
  
  container = c(corn, correlation)
  return (container)
}