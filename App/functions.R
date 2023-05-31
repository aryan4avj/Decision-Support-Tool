barplot_fun <- function(data, x_var, group_var=NULL) {
  
  # data$x_var <- as.factor(data$x_var)
  
  # Create the plot
  if(group_var == ""){
    p <- ggplot(data, aes(x=.data[[x_var]])) +
    geom_bar(fill = "blue")  
  } else {
    p <- ggplot(data, aes(x=.data[[x_var]],fill=.data[[group_var]])) +
    geom_bar(position = "dodge")
  }
  p <- p + 
    xlab(x_var) + 
    ylab("Count") + 
    ggtitle("Barplot") +
    coord_flip()
  
  ggp <- ggplotly(p)
  
  ggp
}
scatterplot_fun <- function(data, x_var, y_var, group_var=NULL) {
  
  # Create the plot
  if(group_var == ""){
    p <- ggplot(data, aes(x=.data[[x_var]], y=.data[[y_var]]))
  } else {
    p <- ggplot(data, aes(x=.data[[x_var]], y=.data[[y_var]],color=.data[[group_var]]))
  }
  p <- p + geom_point(size=3) + 
    xlab(x_var) + 
    ylab(y_var) + 
    ggtitle("Scatterplot")
  
  # Group the points if a group column is provided
  if (!group_var == "") {
    p <- p + scale_color_discrete(name=group_var)
  }
  
  return(ggplotly(p))
}
histogram_fun <- function(data, x_var, group_var=NULL) {
  
  # Create the plot
  if(group_var == ""){
    p <- ggplot(data, aes(x=.data[[x_var]])) + 
      geom_histogram(binwidth=10, position="identity",alpha = 0.8,fill = "blue")
  } else {
    p <- ggplot(data, aes(x=.data[[x_var]],fill=.data[[group_var]])) + 
      geom_histogram(binwidth=10, position="identity",alpha = 0.6) 
  }
  p <- p + xlab(x_var) + 
    ylab("Count") + 
    ggtitle("Histogram")
  
  # Group the bars if a group column is provided
  if (!group_var == "") {
    p <- p + scale_fill_discrete(name=group_var)
  }
  
  return(ggplotly(p))
}
waterfallplot_fun <- function(data, y_var, group_var=NULL) {
  
  # Create the plot
  if(group_var == ""){
    p <- ggplot(data, aes(x=reorder(.data[["TCIA code"]], -.data[[y_var]]), y=.data[[y_var]],fill=.data[[y_var]]))
  } else {
    p <- ggplot(data, aes(x=reorder(.data[["TCIA code"]], -.data[[y_var]]), y=.data[[y_var]],fill=.data[[group_var]]))
  }
  p <- p + geom_bar(stat="identity", position="dodge") + 
    xlab("TCIA code") + 
    ylab(y_var) + 
    ggtitle("Waterfall Plot") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  # Group the bars if a group column is provided
  if (!group_var == "") {
    p <- p + scale_fill_discrete(name=group_var)
  }
  
  return(ggplotly(p))
}
boxplot_fun <- function(data, x_var, y_var, group_var=NULL) {
  
  # Create the plot
  if(group_var == ""){
    p <- ggplot(data, aes(x=.data[[x_var]], y=.data[[y_var]],fill = .data[[x_var]])) + 
      geom_boxplot()
  } else {
    p <- ggplot(data, aes(x=.data[[x_var]], y=.data[[y_var]], fill = .data[[group_var]])) + 
      geom_boxplot(position=position_dodge(3))
  }
  p <- p + xlab(x_var) + 
    ylab(y_var) + 
    ggtitle("Boxplot")
  
  return(ggplotly(p)%>%layout(boxmode = "group"))
}
violinplot_fun <- function(data, x_var, y_var, group_var=NULL) {
  
  # Create the plot
  if(group_var == ""){
    p <- ggplot(data, aes(x=.data[[x_var]], y=.data[[y_var]], fill=.data[[x_var]])) + 
      geom_violin()
  } else {
    p <- ggplot(data, aes(x=.data[[x_var]], y=.data[[y_var]], fill=.data[[group_var]])) + 
      geom_violin()
  }
  p <- p + xlab(x_var) + 
    ylab(y_var) + 
    ggtitle("Violin Plot")
  
  return(ggplotly(p))
}

