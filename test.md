Install and load ggtext and glue

What does glue do?
It creates a more intuitive way to paste (glue) values and text together. Just like a normal string, you type your text between quotation marks (" ").
If there is a plot that youuse a lot for diffent datasets/players/people or anything, you can make it easy on yourself and change things in the title automatically.
You still type your text between quotation marks, but your values are between braces ({ }).

So let's look at this example

```{r}
name <- "Robin"
goals <- 4
title <- glue("{name} scored {goals} goals this season")
print(title)

```
As you see, everything between the braces is the underlying value. This can be other text or a number or anything else you can normaly store.

For an even better example let's use the sample data from r. 
You need the tidyverse package. But I cannot recommend it enough for everything

```{r}
df <- iris %>% filter(Species == "setosa")

ggplot(df,aes(Sepal.Length,Sepal.Width)) + geom_point() +
  labs(title = glue("This plot only shows {df$Species[1]} "),
       subtitle = glue("It's just that I like the {length(df$Species)} {df$Species[1]} the most"))

```

As you can see, your title contains now the one specie that I filtered. 
It also counts the number of observations with {length()}
So to be clear: in the braces you can do everything you can normally do in R code. You can do equations, rounding, flooring and so on in there and it will do that first and than make it text for the (sub)title.
If you change the filter and run the code again. You also get the correct one in your plot. see:

``` {r}
df <- iris %>% filter(Species == "virginica")

ggplot(df,aes(Sepal.Length,Sepal.Width)) + geom_point() +
  labs(title = glue("This plot only shows {df$Species[1]} "),
       subtitle = glue("It's just that I like the {length(df$Species)} {df$Species[1]} the most"))

```

The number of observations is the same in both data frames.
