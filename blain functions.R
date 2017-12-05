###Sample matrix for testing

samp = matrix(1:4, nrow = 2) 



###Sensitivity

sensitivity = function(data) {
  sens = data[1,1] / (data[1,1] + data[2,1])
  percent = sens * 100
  statement = paste("Sensitivity = ", percent, "%")
  interpret = paste(percent, "% of the people who have the disease will test positive.")
  answer = list(statement, interpret)
  return(answer)
}



###Specificity 

specificity = function(data) {
  spec = data[2,2] / (data[2,1] + data[2,2])
  percent = spec * 100
  statement = paste("Specificity = ", percent, "%")
  interpret = paste(percent, "% of the people who do not have the disease will test negative.")
  answer = list(statement, interpret)
  return(answer)
}




###PPV

ppv = function(data) {
  ppval = data[1,1] / (data[1,1] + data[1,2])
  percent = ppval * 100
  statement = paste("PPV = ", percent, "%")
  interpret = paste(percent, "% of the people who tested positive actually have the disease.")
  answer = list(statement, interpret)
  return(answer)
}



###NPV

npv = function(data) {
  npv = data[2,1] / (data[2,1] + data[2,2])
  percent = npv * 100
  statement = paste("NPV = ", percent, "%")
  interpret = paste(percent, "% of the people who tested negative do not have the disease.")
  answer = list(statement, interpret)
  return(answer)
}




