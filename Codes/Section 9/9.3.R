# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Non-DPlyr Examples ---------------------------------------------------
mtcars
df1 <- subset(mtcars, cyl > 6)
df2 <- transform(df1, scale_hp=scale(hp))
head(df2)

df1 <- transform(subset(mtcars, cyl > 6), scale_hp=scale(hp))
head(df1)


# Dplyr ------------------------------------------------------------------------
library(dplyr) 
output <- mtcars %>% subset(cyl > 6) %>% transform(scale_hp = scale(hp))
head(output)

# 1
output <- mtcars %>% subset(cyl > 6, x=.) %>% transform(scale_hp = scale(hp))
head(output)

# 2
output <- mtcars %>% subset(.$cyl > 6) %>% transform(scale_hp = scale(.$hp))
head(output)

mtcars %>% subset(cyl > 6, hp > 200) 


# Dplyr: Data Manipulation Verbs -----------------------------------------------------
# Filter and Mutate
mtcars %>% filter(cyl > 6, hp > 200) %>% 
  mutate(scale_hp = scale(hp), pos_hp=abs(scale_hp))


# Select
mtcars %>% filter(cyl > 6, hp > 200) %>% 
  mutate(scale_hp = scale(hp), pos_hp=abs(scale_hp)) %>%
  select(mpg, scale_hp, pos_hp)

# transmute: retain only the created columns.
mtcars %>% filter(cyl > 6, hp > 200) %>% 
  transmute(scale_hp = scale(hp), pos_hp=abs(scale_hp))

# Slide: By row number
mtcars %>% filter(cyl > 6, hp > 200) %>% slice(1:5)

# arrange: Sort one or more variables
mtcars %>% filter(cyl > 6, hp > 200) %>% slice(1:5) %>% arrange(mpg)  # ascending sort
mtcars %>% filter(cyl > 6, hp > 200) %>% slice(1:5) %>% arrange(-mpg)  # descending sort

# Group and Summarize
mtcars %>% group_by(cyl) %>% summarise(mean_mpg=mean(mpg), max_disp=max(disp))

# Merging / Joins
df1 <- data.frame(x=letters[1:5], y= 1:5, stringsAsFactors = F)
df2 <- data.frame(x=letters[c(3,3:7)], y1= c(3,3:7), stringsAsFactors = F)
df1
df2

# Left Join
left_join(df1, df2)

# Right Join
right_join(df1, df2)

# Inner Join
inner_join(df1, df2)

# No duplication
semi_join(df1, df2)

# Full Join: Does not leave out any row.
full_join(df1, df2)

# Anti Join
anti_join(df1, df2)


# Answer
output <- iris %>% filter(Species %in% c("setosa", "virginica")) %>% 
  group_by(Species) %>% summarise(Petal.Length_min=min(Petal.Length), 
                                  Petal.Length_max=max(Petal.Length), 
                                  Sepal.Length_min=min(Sepal.Length), 
                                  Sepal.Length_max=max(Sepal.Length))

output
