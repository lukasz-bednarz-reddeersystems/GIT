histogram <- function(data,title,x_ticks,y_data,y_label,x_label,group_names=NULL,stat="identity",fill="black",vertical=FALSE,theme_fn=NULL,facet_spec=NULL){
  if(length(group_names)>0){
    H <- ggplot(data = data, aes(x = x_ticks, y = y_data, fill = group_names)) + geom_bar(stat = stat) 
  } else {
    H <- ggplot(data = data, aes(x = x_ticks, y = y_data)) + geom_bar(stat = stat, fill = fill) 
  }
  H <- H + ylab(y_label) + xlab(x_label) + ggtitle(title) 
  if(length(facet_spec)>0)H <- H + facet_spec
  if(vertical)H <- H + coord_flip()
  if(length(theme_fn)>0)H <- H + theme_fn()
  return(H)
}
multiline_series_plot <- function(data,title,x_ticks,y_data,y_label,x_label,group_names,legend_title,stat="identity",line_size=0.7,colour="black",theme_fn=NULL,facet_spec=NULL){
  S <- ggplot(data = data, aes(x = x_ticks, y = y_data, group = factor(group_names))) +
    geom_line(stat = stat, aes(linetype = factor(group_names)), size = line_size, colour = colour) +
    ylab(y_label) + xlab(x_label) + ggtitle(title) + scale_linetype_discrete(legend_title)
  if(length(facet_spec)>0)S <- S + facet_spec
  if(length(theme_fn)>0)S <- S + theme_fn()
  return(S)
}
multicolor_series_plot <- function(data,title,x_ticks,y_data,y_label,x_label,group_names,legend_title,theme_fn=NULL,facet_spec=NULL){
  C <- ggplot(data=data, aes(x=x_ticks, y=y_data, group = group_names, colour = group_names))  + geom_line() 
    #+     geom_point( size=4, shape=21, fill="white") +
    if(length(facet_spec)>0)C <- C + facet_spec
    #if(length(theme_fn)>0)C <- C + theme_fn()
    return(C)
}
radar <- function(data,title,x_ticks,y_data,y_label,x_label,group_names,stat="identity",fill="black",theme_fn=NULL,facet_spec=NULL){
  R <- ggplot(data = data, aes(x = x_ticks, y = y_data, group = factor(group_names))) +
    geom_bar(stat = stat, fill = fill) + coord_polar() +
    ylab(y_label) + xlab(x_label) + ggtitle(title)
  if(length(facet_spec)>0)R <- R + facet_spec
  if(length(theme_fn)>0)R <- R + theme_fn()  
  return(R)
}