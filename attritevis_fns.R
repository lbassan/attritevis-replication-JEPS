# Lotem's macbook, version: Sept 2022
# ----------------------------------------------------------------------
# Function 1: skip_to_attrite() [not available/necessary to users]
# ----------------------------------------------------------------------

#This function takes a matrix of 0,1s in which 1 indicates missingness (NA) per respondent per question and removes `skippers`. Skippers are individuals who have a 0s in their row followed by 1s.


#skip_to_attrite function

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

attrition <- function(data)
{
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
  
  #save original dataset
  data_original<-data
  
  #for each missing value in the dataframe `dataset` give value 1, otherwise give 0.
  data <- apply(data_original,2,function(x) {ifelse(is.na(x),1,0)})
  #First, create `attrited` variable by removing skippers
  #change `skippers` into 0 (we are only interested in respondents that attrited).
  data<-t(apply(data,1,skip_to_attrite))
  data<-data.frame(colSums(data)) #sum the number of missing (minus skippers)
  colnames(data)<- "missing"
  #create variable `attrited` (completely left survey), rather than missing minus skippers  
  data$attrited<-c(data[1,], data[-1,] - data[-nrow(data),])
  #create variable `prop_q` = attrited / n entering into the question
  data$n_prev<- Lag(nrow(data_original) - as.numeric(data$missing), +1)
  data$n_prev[1] <- nrow(data_original)
  data$prop_q <- round(data$attrited/data$n_prev,2)
  #`proportion`= attrited / starting N
  data$proportion <- round(data$attrited/nrow(data_original),2)
  #add variable for question name
  data$questions<-colnames(data_original)
  
  #Next, we also create a `responded` variable that takes the number of individuals who responded
  #for each missing value in the dataframe `dataset` give value 0, otherwise give 1.
  data2 <- apply(data_original,2,function(x) {ifelse(is.na(x),0,1)})
  data2<-data.frame(colSums(data2)) #sum the number of responded
  colnames(data2)<- "responded"
  
  #create variable `prop_r` = responded / starting N
  data2$prop_r <- round(data2$responded/nrow(data_original),2)
  
  #combine the vars  
  data$responded<-data2$responded
  data$prop_r<-data2$prop_r
  
  data$missing<-NULL
  data$n_prev<-NULL
  
  rownames(data) <- c()    
  
  #return dataframe
  return(data)
}


# ----------------------------------------------------------------------
# Function 3: plot_attrition()
# ----------------------------------------------------------------------

#Function that allows you to plot attrition/response in survey data over time.

#`data` must be data.frame. Note that this function works only if the order of variables = order of questions in the survey.

#`y` is a character that corresponds to the Y axis. When y = `attrited`, attrition is plotted. When y= `responded` responses are plotted. Default is y = `attrited`.

#`freq` is a logical argument that notes whether Y axis is a raw number or a proportion. Default is freq=TRUE, which is the frequency of attrited/responded respondents. When freq=FALSE Y axis is the proportion of total N (attrited/responded), calculated as number of attrited/responded divided by the number of respondents entering into the question.

#`treatment_q` is a character of name(s) of question(s) in which treatments were administered. Marked in the plot with a red vertical line.

#`outcome_q` is a character of name(s) of outcome question(s). Marked in the plot with a blue vertical line.

#`mycolors` is a character of color names to be used as values in `scale_colour_manual` argument in ggplot. Default is mycolors=NULL, which defaults to grayscale. `mycolors` must be == length of the unique values of the `treatment_q` variable.

#`title` is a character to be used for plot title.

plot_attrition <- function(data
                           ,y = "attrited"
                           ,freq = TRUE
                           ,treatment_q = NULL
                           ,outcome_q = NULL
                           ,mycolors= NULL
                           ,title = NULL)
{ 
  #required packages
  require(ggplot2)
  require(viridis)
  require(Hmisc)
  require(dplyr)
  require(ggrepel)
  require(data.table)
  
  #make sure arguments are correct
  if(is.null(data))
    stop("Data is null, please supply value")
  
  if(class(data)!="data.frame")
    stop("Data must be data.frame")
  
  if((y != "attrited") & (y != "responded"))
    stop("`y` must either be `attrited` or `responded`." )
  
  if(class(y)!="character")
    stop("`y` must be a character.")
  
  if(class(treatment_q)!="NULL" & class(treatment_q)!="character")
    stop("`treatment_q` must be a character.")
  
  if(class(outcome_q)!="NULL" & class(outcome_q)!="character")
    stop("`outcome_q` must be a character.")
  
  if(class(mycolors)!="NULL" & class(mycolors)!="character")
    stop("`mycolors` must be a character.")
  
  if(class(title)!="NULL" & class(title)!="character")
    stop("`title` must be a character.")
  
  if(class(freq)!="logical")
    stop("Freq must be logical. Default is freq=TRUE.")
  
  
  if(!is.null(treatment_q)) {
    #measure length of the treatment variable
    data3 <- rename(data, cond_new = treatment_q)
    new_treat<-na.omit(data3$cond_new)
    #if length of mycolors is not equal fo length of treatment STOP
    if(!is.null(mycolors)&(length(mycolors))!=length(unique(new_treat)))
      stop("mycolors must be length of unique values of the `treatment_q`.")
    
  }
  
  data_original<-data #save this original data for reference
  
  #create an attrition/response dataset for ALL observations (not by condition)
  all_data<-attrition(data)
  all_data$treatment<-"Total" 
  #we only want to keep the following variables:
  myvars <- c("attrited", "prop_q", "questions", "treatment", "responded", "prop_r")
  all_data <- all_data[myvars]
  
  # if users sopecify treatment_q, we split data *by conditions*
  if(!is.null(treatment_q)) {
    
    data2 <- rename(data, cond_new = treatment_q) #create `cond_new` var based on conditions
    data$cond_new<-data2$cond_new   
    
    #split the dataset into a list by conditions
    data_split<-split(data, with(data, cond_new), drop = TRUE)
    
    #for loop to account for attrition by condition
    listofdfs1 <- list() 
    listofdfs2 <- list() 
    for (i in 1:length(data_split)) {
      #first remove the `cond_new` var we created before
      df<-as.data.frame(data_split[i])
      df[ncol(df)]<-NULL
      #for each missing value assign value 1, for complete response assign 0.
      df<- apply(df,2,function(x) {ifelse(is.na(x),1,0)})
      #apply "skip_to_attrite" to get rid of skippers
      df<-t(apply(df,1,skip_to_attrite))
      #sum the number of missing (minus skippers) per q
      df<- data.frame(colSums(df))
      #rename this variable `missing`
      colnames(df)<- "missing"
      #create variable `attrited`, rather than missing minus skippers
      df$attrited<-c(df[1,], df[-1,] - df[-nrow(df),])
      #`prop_q` = attrited / n entering into the question
      df$n_prev<- Lag(nrow(data_original) - as.numeric(df$missing), +1)
      df$n_prev[1] <- nrow(data_original)
      df$prop_q <- round(df$attrited/df$n_prev,2)
      #add variable for question name
      df$questions<-colnames(data_original)
      #based on rownames per dataset, create `treatment` var
      df$treatment<-rownames(df)
      df$treatment<-gsub("\\..*","",df$treatment)
      
      df$missing<-NULL
      df$n_prev<-NULL
      
      #create `responded` variable.
      df1<-as.data.frame(data_split[i])
      df1[ncol(df1)]<-NULL
      df1 <- apply(df1,2,function(x) {ifelse(is.na(x),0,1)})
      df1<-data.frame(colSums(df1)) #sum the number of responded
      colnames(df1)<- "responded"
      
      #create variable `prop_r` = responded / starting N
      df1$prop_r <- round(df1$responded/nrow(data_original),2)
      
      #remove rownames
      rownames(df) <- c()
      #save as a list
      listofdfs1[[i]] <- df
      listofdfs2[[i]] <- df1
    }
    
    #merge all datasets in the list
    data_combined_a<- rbindlist(listofdfs1)
    data_combined_b<- rbindlist(listofdfs2)
    
    data_combined_a$responded<-data_combined_b$responded
    data_combined_a$prop_r<-data_combined_b$prop_r
    
    #merge the combined dataset with the `all` data
    data<-rbindlist(list(all_data,data_combined_a))
    
  }else{data<-all_data}
  
  #create a vector for the unique values of the question names
  question_names<-unique(data$questions)
  #change question var to factor and numeric for plotting
  data$questions <- factor(data$questions,
                           levels=question_names)
  data$questions2<-as.numeric(data$questions)
  
  if(!is.null(treatment_q)){
    #create indicators for Vlines
    #treatment Vline
    treatment_vars<-as.data.frame(match(treatment_q, question_names))
    colnames(treatment_vars) <- "treatment_q"
    treatment_vars$label<-"Treatment Given" #labels
    treatment_vars$color<- "black" #color of vline
    #where the label appears on yaxis
    if(freq==FALSE){treatment_vars$ynum<- 0.5}
    if(freq==TRUE){treatment_vars$ynum<- nrow(data_original)/2}
    treatment_vars$size<-1
  }
  
  if(!is.null(outcome_q)){
    #outcome Vline
    DV<-as.data.frame(match(outcome_q, question_names))
    colnames(DV) <- "outcome_q"
    DV$label<-"Outcome Question"
    DV$color<- "gray48"
    if(freq==FALSE){DV$ynum<- 0.5}
    if(freq==TRUE){DV$ynum<- nrow(data_original)/2}
    DV$size<-1
  }
  
  #if `freq=TRUE` and `y = "attrited"` we plot the frequency of attrited (y=attrited)
  if(freq==TRUE & y == "attrited"){data$y<-data$attrited}
  if(freq==TRUE & y == "attrited"){yname<-"Attrited"}
  #if `freq=TRUE` and `y = "responded"` we plot the frequency of responded (y=responded)
  if(freq==TRUE & y == "responded"){data$y<-data$responded}
  if(freq==TRUE & y == "responded"){yname<-"Responded"}
  #if `freq=FALSE` and `y = "attrited"` we plot the proportion of attrited (y=prop_q)
  if(freq==FALSE & y == "attrited"){data$y<-data$prop_q}
  if(freq==FALSE & y == "attrited"){yname<-"Proportion of Attrited"}
  #if `freq=FALSE` and `y = "responded"` we plot the proportion of responded (y=prop_r)
  if(freq==FALSE & y == "responded"){data$y<-data$prop_r}
  if(freq==FALSE & y == "responded"){yname<-"Proportion of Responded"}
  
  if(!is.null(treatment_q)){
    p <- data %>%
      ggplot(aes(questions2,y, group = treatment)) +
      
      #scale x axis from 1:10
      scale_x_continuous(breaks=unique(data$questions2),
                         labels=question_names) + #label questions with Q
      
      #create geomlines for `treatment` and `control` 
      geom_line(data = data, aes(questions2, y, 
                                 color = treatment, 
                                 linetype=treatment),
                size = 1.1,
                show.legend = FALSE) +
      
      #label `treatment` and `control`
      
      geom_text_repel(data = data %>% filter(questions2 == length(question_names)),
                      aes(label = treatment, 
                          x = questions2, 
                          y = y, 
                          color = treatment),
                      min.segment.length = 0,
                      show.legend = FALSE)+
      
      #add a geom_point
      geom_point(size=2, aes(colour=factor(treatment), 
                             fill = factor(treatment)), show.legend = FALSE) 
    
  }else{
    
    p <- data %>%
      ggplot(aes(questions2,y)) +
      #scale x axis from 1:10
      scale_x_continuous(breaks=unique(data$questions2),
                         labels=question_names) + #label questions with Q
      geom_line(size = 1.1) +
      #add a geom_point
      geom_point(size=2) 
    
  }
  
  
  if(!is.null(mycolors)) {p <- p + scale_colour_manual(values=c(Total = "gray", mycolors))}else{
    p <- p + scale_colour_grey()
  }
  
  #make treatment red and control blue
  
  #remove gray background
  p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = 
                   element_blank(), panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
    
    labs(x = "Survey Questions", y = yname)
  
  
  if(!is.null(title)){p <- p + ggtitle(title)}
  
  
  if(freq==FALSE){p <- p + ylim (0, 1)}
  if(freq==TRUE){p <- p + ylim (0, nrow(data_original))}
  
  # add title and labels to axis  
  
  if(!is.null(outcome_q)){
    #add the vertical lines
    p<-p +  
      #DV vertical lines
      annotate(geom = "vline",
               x = c(DV$outcome_q),
               xintercept = c(DV$outcome_q),
               color = c(DV$color),
               size = c(DV$size)) +
      
      annotate(geom = "text",
               label = c(DV$label),
               x = c(DV$outcome_q),
               y = c(DV$ynum),
               color = c(DV$color),
               angle = 90, 
               vjust = 1.5)
  }
  
  
  if(!is.null(treatment_q)){  
    #treatments vertical lines
    p<-p + annotate(geom = "vline",
                    x = c(treatment_vars$treatment_q),
                    xintercept = c(treatment_vars$treatment_q),
                    color = c(treatment_vars$color),
                    size = c(treatment_vars$size)) +
      
      annotate(geom = "text",
               label = c(treatment_vars$label),
               x = c(treatment_vars$treatment_q),
               y = c(treatment_vars$ynum),
               color = c(treatment_vars$color),
               angle = 90, 
               vjust = 1.5)
  }
  
  print(p)
}  


# ----------------------------------------------------------------------
# Function 4: balance_cov()
# ----------------------------------------------------------------------

#If question is factor, define value of `factor` you are interested in. For example, `female`. 

balance_cov <- function(data, treatment, 
                        question,
                        factor = FALSE,
                        factor_name = NULL,
                        p_adjust = NULL)
{ 
  
  #required packages
  require(stats)
  
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
  
  if(class(factor)!="logical")
    stop("Factor must be logical. Default is factor=FALSE")
  
  if((factor=FALSE) & is.null(factor_name))
    stop("Factor must be TRUE if factor_name is defined")
  
  if(!is.null(factor_name)&class(factor_name)!="character")
    stop("factor_name must be character")
  
  if(!is.null(p_adjust)&class(p_adjust)!="numeric")
    stop("p_adjust must be numeric")
  
  
  #subset datasets based on treatment and control arms
  data <- rename(data, question1 = question, 
                 treatment1 = treatment)
  
  treat_data<-data[ which(data$treatment1=='treatment'), ]
  control_data<- data[ which(data$treatment1=='control'), ]
  
  if(is.null(factor_name)){
    test <- t.test(treat_data$question1, control_data$question1) #if question is not a factor, run t.test
  }else{
    
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
  
  
  print(test)
  
  if(!is.null(p_adjust)){
    
    a<- p.adjust(p = p_adjust, method = "BH", n = length(p_adjust))
    a<- data.frame(Original_p<-p_adjust,
                   Adjusted_p<-a)
    kable(a)
  }  
  
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

attrition_table <- function(data, treatment_q = NULL)
{
  #required packages
  require(kableExtra)
  
  list <- attrition(data)
  list <- kable(list)
  
  if(!is.null(treatment_q)){
    
    data <- rename(data, cond_new = all_of(treatment_q)) #create `cond_new` var based on conditions
    data_split<-split(data, with(data, cond_new), drop = TRUE)
    listofdfs<-list()
    for (i in 1:length(data_split)) {
      #first remove the `cond_new` var we created before
      df<-as.data.frame(data_split[i])
      df1<-attrition(df)
      listofdfs[[i]] <- df1
      
      #lapply(X = listofdfs, FUN = function(i) {
      #kable(x = i, caption = gsub("\\..*","",colnames(df)[1]))})
      
      list <- lapply(X = listofdfs, FUN = function(i) {
        knitr::kable(x = i, caption = i[1, treatment_q])})
      
    }}
  
  list
  
}


# ----------------------------------------------------------------------
# Function 8: vis_miss_treat()
# ----------------------------------------------------------------------

vis_miss_treat <- function(data ,treatment_q = NULL)
  
{ 
  #required packages
  require(visdat)
  require(grid)
  library(tidyverse)
  
  figure <- vis_miss(data)
  
  #split datasets
  if(!is.null(treatment_q)){
    data2 <- rename(data, cond_new = treatment_q) #create `cond_new` var based on conditions
    data$cond_new<-data2$cond_new
    data_split<-split(data, with(data, cond_new), drop = TRUE)
    
    listofdfs<-list()
    list<-list()
    
    for (i in 1:length(data_split)) {
      #first remove the `cond_new` var we created before
      df<-as.data.frame(data_split[i])
      colnames(df)<-colnames(data)
      df$cond_new<-NULL
      listofdfs[[i]] <- df
      
      list <- lapply(X = listofdfs, FUN = function(i) {
        vis_miss(x = i) + theme(legend.position = "right") + 
          geom_vline(xintercept = treatment_q
                     ,colour = "red") + 
          rremove("ylab") + 
          ggtitle(i[1, treatment_q])})
    }
    
    figure <-  ggarrange(plotlist=list, ncol = 1)
    
    figure <- annotate_figure(figure, left = textGrob("Observations", 
                                                      rot = 90, vjust = 1, gp = gpar(cex = 1.5)))
    
  }
  
  print(figure)
  
} 

