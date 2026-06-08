# Part 1 (Python - R)
#First element	[0]	[1]
#Last element	[-1]	[length(x)]
#Range 2???5	[1:5] (end exclusive) -	[2:5] (end inclusive)
#Row 2, Col 3	.iloc[1, 2]	- [2, 3]
#Entire row 2	.iloc[1, :]	- [2, ]

#Part 2
#2.1
print("hi")
x <- 5L
print(x)
class(x)

x <- as.numeric(x)
print(x)
class(x)

x <- as.integer(x)
print(x)
class(x)

#2.2
s = "Greetings!"
class(s)
typeof(s)

#2.3
text1 <- "Hello"
text2 <- "World!"

combined <- paste(text1, " ", text2)
print(combined)

#2.4
log1 <- FALSE
log2 <- TRUE

log1 | log2
log1 & log2

log1 || log2
log1 && log2

#2.5
exp_groups <- factor(c("Control", "Treated1", "Treated2", "Control", "Treated1"))
print(exp_groups)
levels(exp_groups)
nlevels(exp_groups)   

#Part 3
numeric_vector <- c(10.2, 8.9, 12.5, 9.8, 15.3)

mean(numeric_vector)

max(numeric_vector) - min(numeric_vector)

sum(numeric_vector > 10 & numeric_vector <= 14)

# ----

text_vector <- c("R", "Bioinformatics", "DNA")

nchar(text_vector)

sort(text_vector, decrasing=TRUE)

# ----
logical_vector <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

which(logical_vector)

!logical_vector

filtered_values <- numeric_vector[numeric_vector > 10]
print(filtered_values)

#Part 4
set.seed(42)
Expression <- matrix(
  runif(12,1,10),
  nrow = 3,
  ncol = 4
)
print(Expression)

rownames(Expression) <- c("Gene1", "Gene2", "Gene3")
colnames(Expression) <- c("Sample1", "Sample2", "Sample3", "Sample4")
print(Expression)

rowMeans(Expression)

apply(Expression, 1, mean)

colMeans(Expression)

apply(Expression, 2, mean)

ExpressionT <- t(Expression)
print(ExpressionT)
print(Expression)

Expression[, "Sample1"]
Expression[, "Sample3"]

Sample1_3 <- Expression[, c("Sample1", "Sample3")]
print(Sample1_3)

#Part 5:
df <- data.frame(
  ID = 1:5,
  Age = c(25, 34, 28, 52, 40),
  Sex = c("Female", "Male", "Female", "Male", "Female"),
  Height = c(165, 180, 170, 175, 168),
  Weight = c(60, 80, 65, 75, 58),
  Health_status = c("Good", "Average", "Good", "Poor", "Good")
)

str(df)
summary(df)
head(df)

mean(df$Age)

df[df$HEalth_status == "Good" & df$Sex == "Female", ]

df[order(df$Age, decreasing = TRUE), ]

max(df$Height)
min(df$Height)

#Part 6
for(i in 1:10){
  print(i)
}

total <- 0
for(i in 1:100){
  total <- total + i
}
print(total)

for(i in c(1,2,3,4,5,6,7,8,9,10)){
  if(i %%2 == 0){
    print(i)
  }
}

for (i in 1:20) {
  if (i %% 3 == 0) next
  if (i == 15) break
  print(i)
}

#Part 7
calculate_mean <- function(vec){
  result <- sum(vec) / length(vec)
  return(result)
}
calculate_mean(c(10,20,30))

BMI_calc <- function(weight, height){
  bmi <- weight / (height / 100)^2
  return(bmi)
}
BMI_calc(70, 175)

df$BMI <- BMI_calc(df$Weight, df$Height)
print(df)

calculate_Statistics <- function(vec){
  stats <- list(
    mean = mean(vec),
    median = median(vec),
    minimum = min(vec),
    maximum = max(vec)
  )
  return(stats)
}

calculate_Statistics(c(2,3,1,5,7,9))

#Part 8
bio_list <- list(
  DNA_sequences = c("ATGCCTGAC", "GTCAGTCAG", "CTGATCGATGCTA"),
  species = c("Homo sapiens", "Mus musculus", "Drosophila melanogaster"),
  gene_expression = matrix(runif(9, 0, 100), nrow = 3),
  morphological_traits = data.frame(
    Species = c("Homo sapiens", "Mus musculus", "Drosophila melanogaster"),
    Height  = c(170, 10, 0.1),
    Weight  = c(70, 0.02, 0.0002)
  ),
  mutations = list(Homo_sapiens = c("BRCA1", "BRCA2"), Mus_musculus = "p53")
)

str(bio_list)

bio_list$species

bio_list[["species"]]

bio_list$DNA_sequences <- c(bio_list$DNA_sequences, "CGATGTAGCTA")
print(bio_list$DNA_sequences)

mean(bio_list$gene_expression)

bio_list$mutations$Homo_sapiens

bio_list[["mutations"]][["Homo_sapiens"]]

bio_list$mutations$Mus_musculus <- c(bio_list$mutations$Mus_musculus, "Lmna")
print(bio_list$mutations$Mus_musculus)

#Part 9
apply(Expression, 1, mean)
apply(Expression, 2, mean)
apply(Expression, 1, max)

my_list <- list(
  vector1 = c(1, 2, 3, 4, 5),
  vector2 = c(10, 20, 30),
  vector3 = c(100, 200, 300, 400)
)


lapply(my_list, length)
sapply(my_list, mean)


experimental_groups <- factor(c("Control", "Treated", "Control",
                                "Treated", "Control", "Treated"))

expression_values <- c(5.2, 8.9, 4.8, 9.2, 5.5, 8.7)

tapply(expression_values, experimental_groups, mean)

#Part 10
control_expression <- c(5.2, 4.8, 5.5, 4.9, 5.3, 5.1, 4.7, 5.4, 5.0, 4.6)
treated_expression <- c(7.8, 8.2, 7.5, 8.0, 7.9, 8.3, 7.7, 8.1, 7.6, 8.4)

mean(control_expression);   
mean(treated_expression);
median(control_expression);   
median(treated_expression);   
sd(control_expression);
sd(treated_expression);

max(control_expression) - min(control_expression)
max(treated_expression) - min(treated_expression)

t_result <- t.test(control_expression, treated_expression)
print(t_result)

#Anova Part
group_A <- c(5.1, 5.3, 4.9, 5.2, 5.0)
group_B <- c(6.8, 7.2, 6.9, 7.0, 7.1)
group_C <- c(8.5, 8.8, 8.3, 8.6, 8.7)

anova_data <- data.frame(
  value = c(group_A, group_B, group_C),
  group = factor(rep(c("A", "B", "C"), each = 5))
)

anova_result <- aov(value ~ group, data = anova_data)
summary(anova_result)

plot(df$Height, df$Weight,
     main = "Relationship between height and weight",
     xlab = "Height (cm)",
     ylab = "Weight (kg)",
     pch  = 19,
     col  = "blue")

hist(treated_expression,
     main   = "Histogram of Treated Expression Values",
     xlab   = "Expression Level",
     col    = "lightblue",
     border = "white")


all_values <- c(control_expression, treated_expression)
groups     <- factor(rep(c("Control", "Treated"), each = 10))

boxplot(all_values ~ groups,
        main = "Gene Expression: Control vs Treated",
        xlab = "Group",
        ylab = "Expression Level",
        col  = c("lightgreen", "lightcoral"))


group_means <- c(mean(group_A), mean(group_B), mean(group_C))


barplot(group_means,
        names.arg = c("Group A", "Group B", "Group C"),
        main      = "Average Expression per Group",
        xlab      = "Group",
        ylab      = "Mean Expression Level",
        col       = c("lightblue", "lightgreen", "lightcoral"))

