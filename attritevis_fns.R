#Attritevis functions

# ----------------------------------------------------------------------
# Function 1: skip_to_attrite() [not available/necessary to users]
# ----------------------------------------------------------------------

#This function takes a matrix of 0,1s in which 1 indicates missingness (NA) per respondent per question and removes `skippers`. Skippers are individuals who have a 0s in their row followed by 1s.

skip_to_attrite<-function(arg){
  n_col = length(arg)
  for (j in 1:n_col)
  {
    if (prod(arg[j:n_col])==1)
    {
      arg[j] = 1
    } 
    else 
    {
      arg[j] = 0
    }
  }
  return(arg)
}
# ----------------------------------------------------------------------
# Function 2: attrition()
# ----------------------------------------------------------------------

#Function to transform dataframe into an attrition dataframe. The attrition dataframe indicates, per variable, how many respondents attrited [note that this dataframe does not include `skippers`, i.e. respondents who skipped questions]. The dataframe also includes a variable that is the proportion of total N attrited, calculated as number of attrited respondents / starting N, and a variable that is the proportion of attrited by n at Qi, calculated as attrited respondents /  number of respondents entering into the question.

#only works if you know the order of survey questions. 

attrition <- function(data){
  #required packages
  require(ggplot2)
  require(viridis)
  require(Hmisc)
  require(dplyr)
  
  #make sure arguments are correct
  if(is.null(data))
    stop("Data is null, please supply value")
  
  if(class(data)!="data.frame")
    stop("Data must be data.frame")
  
  #for each missing value in the dataframe `dataset` give value 1, otherwise give 0.
  data <- apply(data,2,function(x) {ifelse(is.na(x),1,0)})
  
  #change `skippers` into 0 (we are only interested in respondents that attrited).
  
  data<-apply(data,1,skip_to_attrite)
  data<-t(data) #transpose data
  
  data2<-data.frame(data)
  
  #transform into a long dataframe, such that the variable `attrited` is the number of missing observations per variable.
  data <- data.frame(colSums(data2))
  colnames(data) <- "attrited"
  
  #transform `attrited` to measure how many respondents attrited during each question, rather than how many missing values are in each question.
  attrite_2<-data$attrited
  num_dropped <- data[-1,] - data[-nrow(data),]
  data$attrited<- c(data[1,], num_dropped)
  data$attrite_2<-attrite_2
  
  #add variable `proportion` = number of attrited respondents / number of respondents entering into the question
  data$n_prev <- nrow(data2) - as.numeric(data$attrite_2)
  data$n_prev <- Lag(data$n_prev, +1)
  data$n_prev[1] <- nrow(data2)
  data$proportion_q <-    round(data$attrited/data$n_prev,2)
  data$n_prev <- NULL
  data$attrite_2 <- NULL
  
  #proportion of attrited / starting N
  data$proportion <-    round(data$attrited/nrow(data2),2)
  
  #add variable `questions` = the name of each variable in the original dataframe.
  data$questions <- rownames(data)
  rownames(data) <- c()
  data <- data[, c(4,1,3,2)]
  
  #return dataframe
  return(data)
}

#attrition_dataset<-attrition(data = df)

# ----------------------------------------------------------------------
# Function 3: plot_attrition()
# ----------------------------------------------------------------------

#Function that allows you to plot attrition in survey data.

#`data` must be data.frame. Note that this function works only if the order of variables = order of questions in the survey.

#`freq` is a logical argument that notes the Y axis of the attrition plot. Default is freq=TRUE, which is the frequency of attrited respondents. When freq=FALSE Y axis is the proportion of total N attrited, calculated as number of attrited respondents / number of respondents entering into the question.

#`treatment` is a character of name(s) of question(s) in which treatments were administered. Marked in the plot with a red vertical line.

#`pre_treatment` is a character of name(s) of pre-treatment question(s). Marked in the plot with a green vertical line.

#`DV` is a character of name(s) of outcome question(s). Marked in the plot with a blue vertical line.

#`other_group_var` is a character of name(s) of question(s), corresponds to `other_group` category, specified by users. Marked in the plot with a purple vertical line. Note that both `other_group` and `other_group_var` must be specified to use one of the functions.

#`other_group` is a character of the name of the group of variables specified in `other_group_var`. Note that both `other_group` and `other_group_var` must be specified to use one of the functions.

plot_attrition <- function(data
                           ,freq = TRUE
                           ,treatment = NULL
                           ,pre_treatment = NULL
                           ,DV = NULL
                           ,other_group = NULL
                           ,other_group_var = NULL
                           ,title = NULL)
{ 
  #required packages
  require(ggplot2)
  require(viridis)
  require(Hmisc)
  require(dplyr)
  
  #make sure arguments are correctly specified
  if(is.null(data))
    stop("Data is null, please supply value")
  
  if(class(data)!="data.frame")
    stop("Data must be data.frame")
  
  if(class(freq)!="logical")
    stop("Freq must be logical. Default is freq=TRUE.")
  
  if(!is.null(treatment) & class(treatment)!="character")
    stop("Treatment must be character")
  
  if(!is.null(pre_treatment) & class(pre_treatment)!="character")
    stop("Pre_treatment must be character")
  
  if(!is.null(DV) & class(DV)!="character")
    stop("DV must be character")
  
  if(!is.null(other_group) & class(other_group)!="character")
    stop("Other_group must be character")
  
  if(!is.null(other_group_var) & class(other_group_var)!="character")
    stop("Other_group_var must be character")
  
  #both other_group_var and group_var must be specified to use either:
  if(!is.null(other_group_var) & is.null(other_group))
    stop("Specify name of other_group")
  
  if(is.null(other_group_var) & !is.null(other_group))
    stop("Specify other_group_var")
  
  
  #Begin by creating an attrition dataframe
  #for each missing value in the dataframe `dataset` give value 1, otherwise give 0.
  data <- apply(data,2,function(x) {ifelse(is.na(x),1,0)})
  
  #change `skippers` into 0 (we are only interested in respondents that attrited).
  
  data<-apply(data,1,skip_to_attrite)
  data<-t(data) #transpose data
  
  data2<-data.frame(data)
  
  #transform into a long dataframe, such that the variable `attrited` is the number of missing observations per variable.
  data <- data.frame(colSums(data2))
  colnames(data) <- "attrited"
  
  #transform `attrited` to measure how many respondents attrited during each question, rather than how many missing values are in each question.
  attrite_2<-data$attrited
  num_dropped <- data[-1,] - data[-nrow(data),]
  data$attrited<- c(data[1,], num_dropped)
  data$attrite_2<-attrite_2
  
  #add variable `proportion` = number of attrited respondents / starting N.
  
  # data$n_prev <- nrow(data2) - as.numeric(data$attrite_2)
  # data$n_prev <- Lag(data$n_prev, +1)
  # data$n_prev[1] <- nrow(data2)
  # data$proportion_q <-   round(data$attrited/data$n_prev,2)
  data$n_prev <- NULL
  data$attrite_2 <- NULL
  data$proportion <-    round(data$attrited/nrow(data2),2)
  
  #add variable `questions` = the name of each variable in the original dataframe.
  data$questions <- rownames(data)
  rownames(data) <- c()
  data$questions <- factor(data$questions, levels=data$questions)
  
  #Next, plot attrition
  #set colors for plots
  tmp_colors<-viridis(n=2,alpha=0.6,begin=0.25,end=1,direction=1,option="D")
  
  #create figure for if treatment is not NULL and freq = TRUE
  p <- data %>%
    ggplot(aes(questions,{if(freq==FALSE){proportion}else{attrited}})) + 
    #add if statement based on freq
    
    geom_histogram(color="#e9ecef", alpha=0.6, stat = 'identity') +
    
    
    scale_fill_manual(values=tmp_colors) 
  
  #vlines
  
  #add vline for other_group, only if it isn't null
  if(!is.null(other_group)) {
    p <- p + geom_vline(data= data.frame(type=other_group, 
                                         col=other_group, other_group_var = other_group_var),
                        aes(colour=col, xintercept = match(other_group_var,data$questions)), #other_group_var), 
                        size = 0.7, show.legend = TRUE)} 
  
  #add vline for pre_treatment, only if it isn't null   
  if(!is.null(pre_treatment)){
    p <- p + geom_vline(data= data.frame(type="Pre-Treatment", 
                                         col="Pre-Treatment", pre_treatment = pre_treatment),
                        aes(colour=col, xintercept = pre_treatment), 
                        size = 0.7, show.legend = TRUE)}
  #add vline for treatment, only if it isn't null   
  if(!is.null(treatment)){
    p <- p + geom_vline(data= data.frame(type="Treatment", 
                                         col="Treatment", treatment = treatment),
                        aes(colour=col, xintercept = treatment), 
                        size = 1.5, show.legend = TRUE)}
  
  #add vline for DV, only if it isn't null   
  if(!is.null(DV)){
    p <- p + geom_vline(data= data.frame(type="Outcome", 
                                         col="Outcome", DV = DV),
                        aes(colour=col, xintercept = DV), 
                        size = 0.7, show.legend = TRUE)} 
  
  #delete gray background  
  
  p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 #  panel.background = element_rect(fill = "lavenderblush1"),
                 #  plot.background = element_rect(fill = "lavenderblush1"),
                 #  panel.background = element_rect(fill = "gray93"),
                 #  plot.background = element_rect(fill = "gray93"),
                 panel.background = element_blank(),
                 axis.text.x = element_text(angle = 90, hjust = 1, size = 8))   
  
  #add legend details manually  
  p<- p + scale_colour_manual(name="Legend"
                              ,breaks = c("Pre-Treatment","Treatment","Outcome",other_group)
                              ,labels = c("Pre-Treatment","Treatment","Outcome",other_group)
                              ,values = c("goldenrod3","firebrick","royalblue3","seagreen")
  ) +
    labs(x = "Survey Questions") + #titles
    labs(y = {if(freq==FALSE){"Proportion of respondents attrited"}
      else{"Respondents attrited"}}) +#add if statement based on freq==FALSE
    
    ggtitle(if(is.null(title)){""}else{print(title)})
  if(freq == FALSE){print(p+ylim(0,1))}else{print(p)} #define limits of Y axis if freq==FALSE
  
}


# ----------------------------------------------------------------------
# Function 4: balance_cov()
# ----------------------------------------------------------------------

#If question is factor, define value of `factor` you are interested in. For example, `female`. 

balance_cov <- function(data, treatment = NULL, 
                        question,
                        value = NULL,
                        factor = FALSE,
                        factor_name = NULL)
{ 
  
  #make sure arguments are correctly specified
  if(is.null(data))
    stop("Data is null, please supply value")
  
  if(class(data)!="data.frame")
    stop("Data must be data.frame")
  
  if(is.null(value) & is.null(treatment))
    stop("Treatment is null, please supply value")
  
  if(!is.null(treatment) & class(treatment)!="character")
    stop("Treatment must be a character")
  
  if(is.null(question))
    stop("Question is null, please supply value")
  
  if(class(question)!="character")
    stop("Question must be a character")
  
  if(class(factor)!="logical")
    stop("Factor must be logical. Default is factor=FALSE")
  
  if((factor=FALSE) & !is.null(factor_name))
    stop("Factor must be TRUE if factor_name is defined")
  
  if(!is.null(factor_name) & class(factor_name)!="character")
    stop("Factor must be character")
  
  if(!is.null(value) & !is.null(treatment))
    stop("Covariate is being tested against specified `value`, 
       please specify `treatment` == NULL")
  
  
  if(!is.null(treatment) && is.null(value) && is.null(factor_name)){
    
    
    data <- rename(data, question1 = question, 
                   treatment1 = treatment)
    
    
    #subset datasets based on treatment and control arms
    
    treat_data<-data[ which(data$treatment1=='treatment'), ]
    control_data<- data[ which(data$treatment1=='control'), ]
    
    
    test <- t.test(treat_data$question1, control_data$question1) #if question is not a factor, run t.test
  }
  
  #!is.null(treatment) & is.null(value) & is.null(factor))
  if(!is.null(factor_name) && !is.null(treatment) && is.null(value) ){
    data <- rename(data, question1 = question, 
                   treatment1 = treatment)
    
    treat_data<-data[ which(data$treatment1=='treatment'), ]
    control_data<- data[ which(data$treatment1=='control'), ]
    
    
    #define factor treatment and control
    factor_treat<- treat_data[ which(treat_data$question1==factor_name), ]
    factor_control<- control_data[ which(control_data$question1==factor_name), ]
    
    #define not_factor treatment and control
    not_factor_treat<- treat_data[ which(treat_data$question1 != factor_name), ]
    not_factor_control<- control_data[ which(control_data$question1 != factor_name), ]
    
    x<-c(nrow(factor_treat), nrow(factor_control))
    n<-c(nrow(factor_treat)+nrow(not_factor_treat), 
         nrow(factor_control)+nrow(not_factor_control))
    
    #run two sample proportion test
    test <- prop.test(x,n)
  }
  
  if(!is.null(value) && is.null(factor_name) && is.null(treatment)){
    
    data <- rename(data, question1 = question)
    
    test <- t.test(data$question1, mu = value)
  }
  
  print(test)
}


# ----------------------------------------------------------------------
# Function 5: balance_attrite()
# ----------------------------------------------------------------------

balance_attrite <- function(data, treatment, 
                            question)
{ 
  
  #make sure arguments are correctly specified
  if(is.null(data))
    stop("Data is null, please supply value")
  
  if(class(data)!="data.frame")
    stop("Data must be data.frame")
  
  if(is.null(treatment))
    stop("Treatment is null, please supply value")
  
  if(class(treatment)!="character")
    stop("Treatment must be a character")
  
  if(is.null(question))
    stop("Question is null, please supply value")
  
  if(class(question)!="character")
    stop("Question must be a character")
  
  data <- rename(data, question1 = question, 
                 treatment1 = treatment)
  
  #for each missing value in the dataframe `data` give value 1, otherwise give 0.
  data1 <- apply(data,2,function(x) {ifelse(is.na(x),1,0)})
  
  #change `skippers` into 0 (we are only interested in respondents that attrited).
  
  data1<-apply(data1,1,skip_to_attrite)
  data1<-t(data1) #transpose data
  
  data1<-data.frame(data1)
  
  #subset datasets based on treatment and control arms
  
  treatment1<-data$treatment1 #from original data
  question1<-data1$question1 #from attrition data (0 remained in survey, 1 attrited)
  data2 <- data.frame(treatment1, question1)  
  
  model<-glm(question1~treatment1, data = data2, family=binomial(link="logit")) #run glm
  
  print(summary(model))
  
}


# ----------------------------------------------------------------------
# Function 6: bounds()
# ----------------------------------------------------------------------

bounds <- function(data, treatment, 
                   DV, type = "Manski")
{ 
  
  #make sure arguments are correctly specified
  if(is.null(data))
    stop("Data is null, please supply value")
  
  if(class(data)!="data.frame")
    stop("Data must be data.frame")
  
  if(is.null(treatment))
    stop("Treatment is null, please supply value")
  
  if(class(treatment)!="character")
    stop("Treatment must be a character")
  
  if(is.null(DV))
    stop("Question is null, please supply value")
  
  if(class(DV)!="character")
    stop("Question must be a character")
  
  if(class(type)!="character")
    stop("Question must be a character")
  
  data <- rename(data, DV1 = DV, 
                 treatment1 = treatment)
  
  data <- data[which(!is.na(data$treatment1)), ]
  data$Y <- data$DV1
  data$Z <- ifelse(data$treatment1=="treatment",1,0)
  data$R <- ifelse(is.na(data$Y),0,1)
  
  
  minY <- min(data$Y, na.rm = TRUE)
  maxY <- min(data$Y, na.rm = TRUE)
  
  if(type == "Lee"){print(
    attrition::estimator_trim(Y=all_of(Y), 
                              Z=Z, R=R, R1 = NULL, Attempt = NULL, 
                              R2 = NULL, strata = NULL, alpha = 0.05, 
                              data = data))}else{print(attrition::estimator_ev(Y=all_of(Y), 
                                                                               Z=Z, R=R, minY=minY, maxY=maxY, strata = NULL, alpha = 0.05, 
                                                                               data = data))}
}


# ----------------------------------------------------------------------
# Function 7: attrition_table()
# ----------------------------------------------------------------------

attrition_table <- function(data, condition = NULL,
                            treatment_var = NULL)
{
  #required packages
  require(ggplot2)
  require(viridis)
  require(Hmisc)
  require(dplyr)
  require(kableExtra)
  
  #make sure arguments are correct
  if(is.null(data))
    stop("Data is null, please supply value")
  
  if(class(data)!="data.frame")
    stop("Data must be data.frame")
  
  # if(condition!="both" | condition!="treatment" | condition!="control")
  #stop("conditon must be named `treatment`, `control` or `both`")
  
  if(!is.null(condition)& is.null(treatment_var))
    stop("treatment_var must be specified")
  #for each missing value in the dataframe `dataset` give value 1, otherwise give 0.
  if(is.null(condition)){
    
    data <- apply(data,2,function(x) {ifelse(is.na(x),1,0)})}else{
      
      data <-  rename(data,
                      treatment_var = treatment_var)
      
      data <- data[which(data$treatment_var == condition), ]
      
      data <- apply(data,2,function(x) {ifelse(is.na(x),1,0)})
    }
  
  # data <- apply(data,2,function(x) {ifelse(is.na(x),1,0)})
  
  #change `skippers` into 0 (we are only interested in respondents that attrited).
  
  data<-apply(data,1,skip_to_attrite)
  data<-t(data) #transpose data
  
  data2<-data.frame(data)
  
  #transform into a long dataframe, such that the variable `attrited` is the number of missing observations per variable.
  data <- data.frame(colSums(data2))
  colnames(data) <- "attrited"
  
  #transform `attrited` to measure how many respondents attrited during each question, rather than how many missing values are in each question.
  attrite_2<-data$attrited
  num_dropped <- data[-1,] - data[-nrow(data),]
  data$attrited<- c(data[1,], num_dropped)
  data$attrite_2<-attrite_2
  
  #add variable `proportion` = number of attrited respondents / number of respondents entering into the question
  data$n_prev <- nrow(data2) - as.numeric(data$attrite_2)
  data$n_prev <- Lag(data$n_prev, +1)
  data$n_prev[1] <- nrow(data2)
  data$proportion_q <-    round(data$attrited/data$n_prev,2)
  data$n_prev <- NULL
  data$attrite_2 <- NULL
  
  
  #proportion of attrited / starting N
  data$proportion <- round(data$attrited/nrow(data2),2)
  
  #add variable `questions` = the name of each variable in the original dataframe.
  data$questions <- rownames(data)
  rownames(data) <- c()
  data <- data[, c(4,1,3,2)]
  
  return(kable(data))
  
}