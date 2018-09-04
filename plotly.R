library(plotly)

# use matcars dataset for scatter plot

p1 = plot_ly(data= mtcars, x =~wt, y =~mpg, type = "scatter", mode = "markers", color = I("black"))
p1


# Factor variable in plot

p2 = plot_ly(data= mtcars, x =~wt, y =~mpg, type = "scatter", mode = "markers", color = ~as.factor(cyl))
p2

# define your own color:
pal <- c("red", "blue", "green")
p3 = plot_ly(data= mtcars, x =~wt, y =~mpg, type = "scatter", mode = "markers", color = ~as.factor(cyl), 
             colors = pal)
p3

# Color based on contunuous var
p4 = plot_ly(data= mtcars, x =~wt, y =~mpg, type = "scatter", mode = "markers", color = ~disp)
p4


# HIde legend

p5 = plot_ly(data= mtcars, x =~wt, y =~mpg, type = "scatter", mode = "markers", color = ~as.factor(cyl)) %>% layout(showlegend = FALSE)
p5



# Change legend position like below graph or on left side

p5 = plot_ly(data= mtcars, x =~wt, y =~mpg, type = "scatter", mode = "markers", color = ~as.factor(cyl)) %>% layout(legend = list(orientation = 'h'))
p5



 o <- plot_ly(data = mtcars, 
        x=~mpg, 
        y=~wt, 
        type = "scatter", 
        mode="markers") %>% 
  add_annotations(
    x=mtcars$mpg[which.max(mtcars$mpg)],
    y=mtcars$wt[which.max(mtcars$mpg)],
    text="Good mileage",
    showarrow=T
  )
 
o