# A couple of comments on using shiny to create a reactive dashboard that can 
# be hosted locally in your browser of choice. To run the app make sure the
# R project is open to teenage-glasgow-shiny-demo in the top right dropdown
# 
# Then click on Run and select Run all, or select all and Ctrl+Enter.
# 
# Issues so far, the Run Document as app button does not work, make sure the pacman
# load packages helper function, make sure to look into any cases of downloading
# different packages on a case by case basis, perhaps doing this manually with
# install.packages(" ")
# 
# The nodes once adding shapes to differentiate between female and male gender, 
# circle and square correspondingly, the hover label does not work. There is an 
# issue with the dropdown in the side bar not taking the specified default value
# so for ex. students are not filtered according to parents' smoking habits unless 
# a choice is selected. I have not been able to figure out how to reset filtering
# so far, so refreshing the page is the best way to go. It would also be nice to 
# add a legend to indicate the color intensity is linked to smoking dependency, 
# while the size of nodes is proportional to outdegree so it might be a bit redundant to 
# introduce a filter that only displays students with outdegree of a certain value
# or greater. 
# 
# Other than these issues there's some stylistic issues, but the main limitation is 
# the shareData table does not follow filtering rules applied to the graph, at this 
# point I don't know how to address this. Also sometimes if the graph plot disappears
# you'll need to click on that space to render it once more.
#
load('data/Glasgow-friendship.RData')
load('data/Glasgow-substances.RData')
load('data/Glasgow-demographic.RData')

#load('data/Glasgow-lifestyle.RData')
#load('data/Glasgow-geographic.RData')
load('data/Glasgow-various.RData')


# 3 - Read Data

friendships_t2 <- as.matrix(friendship.2)

# Convert adjacency matrix to a networkD3-compatible format

friendships_t2[friendships_t2==1] <- 1
friendships_t2[friendships_t2==2] <- 1
friendships_t2[friendships_t2==10] <- 0

friendships_t2 <- replace(friendships_t2, is.na(friendships_t2), 0)

tobacco <- as.matrix(tobacco)

student_smoking_t2 <- c(tobacco[,2])
student_smoking_t2 <- replace(student_smoking_t2, is.na(student_smoking_t2), 0)


friendships_t3 <- as.matrix(friendship.3)

# Convert adjacency matrix to a networkD3-compatible format

friendships_t3[friendships_t3==1] <- 1
friendships_t3[friendships_t3==2] <- 1
friendships_t3[friendships_t3==10] <- 0

friendships_t3 <- replace(friendships_t3, is.na(friendships_t3), 0)


student_smoking_t3 <- c(tobacco[,3])
student_smoking_t3 <- replace(student_smoking_t3, is.na(student_smoking_t3), 0)

friendships_t1 <- as.matrix(friendship.1)

# Convert adjacency matrix to a networkD3-compatible format

friendships_t1[friendships_t1==1] <- 1
friendships_t1[friendships_t1==2] <- 1
friendships_t1[friendships_t1==10] <- 0

friendships_t1 <- replace(friendships_t1, is.na(friendships_t1), 0)


student_smoking_t1 <- c(tobacco[,1])
student_smoking_t1 <- replace(student_smoking_t1, is.na(student_smoking_t1), 0)


EL2 <-  melt(friendships_t2)

names(EL2)[1] <- "source"
names(EL2)[2] <- "target"

EL2 <- filter(EL2, EL2$value!=0)
EL2$source <- as.character(EL2$source)
EL2$target <- as.character(EL2$target)

# Now change the shapes of the vertices in the network 
# 4- stands for square, 3- stands for triangle
sex_gender_shapes <- as.numeric(sex.F)
sex_gender_shapes <- recode(sex_gender_shapes, `2` = 'Girl', `1` = 'Boy')

NL2 <- data.frame(source = c(rownames(friendships_t2)), smokes = student_smoking_t2)
rownames(NL2) <- c(1:nrow(friendships_t2))

g2 <- graph_from_data_frame(d=EL2, vertices = NL2, directed=TRUE)

V(g2)$label <- c(rownames(friendships_t2))

V(g2)$indegree   <- degree(g2, mode = "in")
V(g2)$outdegree  <- degree(g2, mode = "out")

V(g2)$size  <- degree(g2, mode = "out")
#V(g)$type <- sex_gender_shapes

family_smoking <- as.numeric(familysmoking[, 2])
parent_smoking <- recode(family_smoking, `2` = 'Smoking', `1` = 'Non-Smoking')

V(g2)$parent_smoking <- parent_smoking
V(g2)$sex_variable <- sex_gender_shapes

E(g2)$type <-  "arrow"
E(g2)$size <-  5

vcolors_palette <- c('0' = "#6991c3", '1' = "#3b68a1", '2' = "#254b7c", '3' = "#13345e")
vcolors_t2 <- vcolors_palette[as.character(V(g2)$smokes)]
V(g2)$color <- vcolors_t2


vertices2 <- igraph::as_data_frame(g2, what = "vertices")
sd2 <- SharedData$new(vertices2, key = vertices2$name)

layout2 <- igraph::layout_with_fr(g2)

header.style <- "th { font-family: 'Arial'; font-weight: bold; color: white; background-color: #1d1145;}"
#pull header names from the table
header.names <- str_to_title(c(colnames(vertices)))
# The container parameter allows us to design the header of the table using CSS
my.container <- withTags(table(
  style(type = "text/css", header.style),
  thead(
    tr(
      lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
    )
  )
))

result2 <- rbind(
  format(round(vcount(g2)), nsmall = 0),
  format(ecount(g2), digits = 3),
  format(edge_density(g2), digits = 3),
  format(reciprocity(g2), digits = 3),
  format(centr_betw(g2)$centralization, digits = 3),
  format(mean_distance(g2), digits = 3)
)

rownames(result2) <- c("Nodes", "Edges", "Density", "Reciprocity", "Centrality", "Average Path Length")
colnames(result2) <- c("Value")

# Convert to a data frame for htmlTable
result_df2 <- as.data.frame(result2)

styled_table2 <- result_df2 %>% 
  htmlTable(.,theme = "scientific", css.table = "width:80%;border: none")

my.options <- list(autoWidth = FALSE, #smart width handling
                   searching = FALSE, #search box above table
                   ordering = T, #whether columns can be sorted
                   lengthChange = F, #ability to change number rows shown on page in table
                   paging = F, #whether to do pagination
                   info = FALSE,
                   scrollY= 400,
                   scrollX= 500) #notes whether or not table is filtered


EL1 <-  melt(friendships_t1)

names(EL1)[1] <- "source"
names(EL1)[2] <- "target"

EL1 <- filter(EL1, EL1$value!=0)
EL1$source <- as.character(EL1$source)
EL1$target <- as.character(EL1$target)

NL1 <- data.frame(source = c(rownames(friendships_t1)), smokes = student_smoking_t1)
rownames(NL1) <- c(1:nrow(friendships_t1))

g1 <- graph_from_data_frame(d=EL1, vertices = NL1, directed=TRUE)

V(g1)$label <- c(rownames(friendships_t1))

V(g1)$indegree   <- degree(g1, mode = "in")
V(g1)$outdegree  <- degree(g1, mode = "out")

V(g1)$size  <- degree(g1, mode = "out")
#V(g)$type <- sex_gender_shapes

V(g1)$parent_smoking <- parent_smoking
V(g1)$sex_variable <- sex_gender_shapes

E(g1)$type <-  "arrow"
E(g1)$size <-  5

vcolors_palette <- c('0' = "#6991c3", '1' = "#3b68a1", '2' = "#254b7c", '3' = "#13345e")
vcolors1 <- vcolors_palette[as.character(V(g1)$smokes)]
V(g1)$color <- vcolors1


vertices1 <- igraph::as_data_frame(g1, what = "vertices")
sd1 <- SharedData$new(vertices1, key = vertices1$name)

layout1 <- igraph::layout_with_fr(g1)


result1 <- rbind(
  format(round(vcount(g1)), nsmall = 0),
  format(ecount(g1), digits = 3),
  format(edge_density(g1), digits = 3),
  format(reciprocity(g1), digits = 3),
  format(centr_betw(g1)$centralization, digits = 3),
  format(mean_distance(g1), digits = 3)
)

rownames(result1) <- c("Nodes", "Edges", "Density", "Reciprocity", "Centrality", "Average Path Length")
colnames(result1) <- c("Value")

# Convert to a data frame for htmlTable
result_df1 <- as.data.frame(result1)

styled_table1 <- result_df1 %>% 
  htmlTable(.,theme = "scientific", css.table = "width:80%;border: none")

g <- g1
sd <- sd1
layout <- layout1

EL3 <-  melt(friendships_t3)

names(EL3)[1] <- "source"
names(EL3)[2] <- "target"

EL3 <- filter(EL3, EL3$value!=0)
EL3$source <- as.character(EL3$source)
EL3$target <- as.character(EL3$target)

NL3 <- data.frame(source = c(rownames(friendships_t3)), smokes = student_smoking_t3)
rownames(NL3) <- c(1:nrow(friendships_t3))

g3 <- graph_from_data_frame(d=EL3, vertices = NL3, directed=TRUE)

V(g3)$label <- c(rownames(friendships_t3))

V(g3)$indegree   <- degree(g3, mode = "in")
V(g3)$outdegree  <- degree(g3, mode = "out")

V(g3)$size  <- degree(g3, mode = "out")
V(g3)$sex_variable <- sex_gender_shapes

V(g3)$parent_smoking <- parent_smoking

E(g3)$type <-  "arrow"
E(g3)$size <-  5

vcolors_palette <- c('0' = "#6991c3", '1' = "#3b68a1", '2' = "#254b7c", '3' = "#13345e")
vcolors3 <- vcolors_palette[as.character(V(g3)$smokes)]
V(g3)$color <- vcolors3


vertices3 <- igraph::as_data_frame(g3, what = "vertices")
sd3 <- SharedData$new(vertices3, key = vertices3$name)

layout3 <- igraph::layout_with_fr(g3)


result3 <- rbind(
  format(round(vcount(g3)), nsmall = 0),
  format(ecount(g3), digits = 3),
  format(edge_density(g3), digits = 3),
  format(reciprocity(g3), digits = 3),
  format(centr_betw(g3)$centralization, digits = 3),
  format(mean_distance(g3), digits = 3)
)

rownames(result3) <- c("Nodes", "Edges", "Density", "Reciprocity", "Centrality", "Average Path Length")
colnames(result3) <- c("Value")

# Convert to a data frame for htmlTable
result_df3 <- as.data.frame(result3)

styled_table3 <- result_df3 %>% 
  htmlTable(.,theme = "scientific", css.table = "width:80%;border: none")

styled_table <- styled_table1
