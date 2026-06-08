setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages("tidyverse")
install.packages("readxl")

library(tidyverse)
library(readxl)

# 1.1
patients  <- read_csv("patients.csv")
glimpse(patients)

# 1.2
sequences <- read_tsv("sequences1.txt")
print(sequences)

# 1.3
genes <- read_excel("genes1.xlsx")
nrow(genes)
ncol(genes)
dim(genes)

# 1.4
rna_seq <- read_csv("rna_seq_data.csv")
head(rna_seq, 10)

# 1.5
spec(rna_seq)
glimpse(rna_seq)

# 2.1
rna_seq %>%
  select(Gene_ID, Gene_Symbol, Expression_Level, Log2FC, P_value)

# 2.2
rna_seq %>%
  select(contains("value"))

# 2.3
rna_seq %>%
  select(Gene_ID:Length)

# 2.4
rna_seq %>%
  filter(P_value < 0.01)

# 2.5
rna_seq %>%
  filter(Gene_Type == "protein_coding", Chromosome == "1")

# 2.6
rna_seq %>%
  filter(Log2FC > 2 | Log2FC < -2)

# 2.7
rna_seq %>%
  filter(Sample_Quality == "high", Condition == "control")

# 3.1
rna_seq <- rna_seq %>%
  mutate(Significant = Adjusted_P_value < 0.05)

# 3.2
rna_seq <- rna_seq %>%
  mutate(Expression_Category = case_when(
    Expression_Level < 50                            ~ "Low",
    Expression_Level >= 50 & Expression_Level <= 100 ~ "Medium",
    Expression_Level > 100                           ~ "High"
  ))

# 3.3
rna_seq <- rna_seq %>%
  mutate(Length_Expression_Ratio = Length / Expression_Level)

# 3.4
rna_seq %>%
  arrange(P_value)

# 3.5
rna_seq %>%
  arrange(desc(abs(Log2FC)))

# 4.1
rna_seq %>%
  group_by(Gene_Type) %>%
  summarise(Mean_Expression = mean(Expression_Level))

# 4.2
rna_seq %>%
  group_by(Condition) %>%
  summarise(
    N      = n(),
    Mean   = mean(Log2FC),
    Median = median(Log2FC),
    SD     = sd(Log2FC)
  )

# 4.3
rna_seq %>%
  group_by(Chromosome) %>%
  summarise(Gene_Count = n()) %>%
  arrange(desc(Gene_Count))

# 4.4
rna_seq %>%
  group_by(Condition, Time_Point) %>%
  summarise(
    Mean_Expression = mean(Expression_Level),
    N               = n(),
    .groups         = "drop"
  )

# 4.5
rna_seq %>%
  group_by(Chromosome) %>%
  summarise(Mean_Expression = mean(Expression_Level)) %>%
  arrange(desc(Mean_Expression)) %>%
  slice(1)

# 5.1
rna_seq %>%
  count(Gene_Symbol)

# 5.2
rna_seq %>%
  distinct(Gene_Type)

# 5.3
rna_seq %>%
  distinct(Condition, Time_Point) %>%
  nrow()

# 5.4
rna_seq %>%
  group_by(Gene_Type) %>%
  summarise(Significant_Count = sum(Adjusted_P_value < 0.05, na.rm = TRUE))

# 6.1
genes_long <- genes %>%
  pivot_longer(
    cols      = c(Control_1, Control_2, Control_3,
                  Treatment_1, Treatment_2, Treatment_3),
    names_to  = "Sample",
    values_to = "Expression"
  )
print(genes_long)

# 6.2
genes_long <- genes_long %>%
  mutate(Condition = str_extract(Sample, "^[A-Za-z]+"))
print(genes_long)

# 6.3
patients %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# 6.4
patients_clean <- patients %>%
  drop_na()
print(patients_clean)
nrow(patients_clean)

# 6.5
patients_imputed <- patients %>%
  mutate(
    Hemoglobin = replace_na(Hemoglobin, median(Hemoglobin, na.rm = TRUE)),
    Leukocytes = replace_na(Leukocytes, median(Leukocytes, na.rm = TRUE)),
    Platelets  = replace_na(Platelets,  median(Platelets,  na.rm = TRUE))
  )
print(patients_imputed)

# 7.1
sequences <- sequences %>%
  mutate(Calculated_Length = str_length(Sequence))
sequences %>%
  select(ID, Gene, Length, Calculated_Length) %>%
  mutate(Match = Length == Calculated_Length)

# 7.2
sequences %>%
  mutate(G_count = str_count(Sequence, "G")) %>%
  select(ID, Gene, Sequence, G_count)

# 7.3
sequences %>%
  mutate(Has_Start_Codon = case_when(
    Type == "DNA" ~ str_detect(Sequence, "^ATG"),
    Type == "RNA" ~ str_detect(Sequence, "^AUG")
  )) %>%
  select(ID, Gene, Type, Sequence, Has_Start_Codon)

# 7.4
sequences <- sequences %>%
  mutate(Sequence_RNA = if_else(
    Type == "DNA",
    str_replace_all(Sequence, "T", "U"),
    Sequence
  ))
sequences %>% select(ID, Gene, Type, Sequence, Sequence_RNA)

# 7.5
sequences %>%
  mutate(Start_Codon = str_sub(Sequence, 1, 3)) %>%
  select(ID, Gene, Sequence, Start_Codon)

# 7.6
sequences <- sequences %>%
  mutate(
    G_count       = str_count(Sequence, "G"),
    C_count       = str_count(Sequence, "C"),
    GC_calculated = (G_count + C_count) / str_length(Sequence) * 100,
    GC_diff       = round(abs(GC_content - GC_calculated), 2)
  )
sequences %>%
  select(ID, Gene, GC_content, GC_calculated, GC_diff)

# 7.7
sequences %>%
  filter(str_detect(Sequence, "GATC")) %>%
  select(ID, Gene, Sequence)

# 8.1
ggplot(rna_seq, aes(x = Expression_Level)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of gene expression levels",
    x     = "Expression Level",
    y     = "Count"
  ) +
  theme_minimal()

# 8.2
ggplot(rna_seq, aes(x = Condition, y = Expression_Level, fill = Condition)) +
  geom_boxplot() +
  labs(
    title = "Expression Levels by Experimental Condition",
    x     = "Condition",
    y     = "Expression Level"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 8.3
rna_seq <- rna_seq %>%
  mutate(Highlight = Adjusted_P_value < 0.05 & abs(Log2FC) > 1)

ggplot(rna_seq, aes(x = Log2FC, y = -log10(P_value), color = Highlight)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(
    values = c("FALSE" = "grey70", "TRUE" = "red"),
    labels = c("Not significant", "Significant")
  ) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = c(-1, 1),     linetype = "dashed", color = "blue") +
  labs(
    title = "Volcano Plot",
    x     = "Log2 Fold Change",
    y     = "-log10(P-value)",
    color = "Significant"
  ) +
  theme_minimal()

# 8.4
ggplot(rna_seq, aes(x = Length, y = Expression_Level)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Gene Length vs Expression Level (log scale)",
    x     = "Gene Length (log10)",
    y     = "Expression Level (log10)"
  ) +
  theme_minimal()

# 8.5
rna_seq %>%
  count(Gene_Type) %>%
  mutate(Gene_Type = fct_reorder(Gene_Type, n, .desc = TRUE)) %>%
  ggplot(aes(x = Gene_Type, y = n, fill = Gene_Type)) +
  geom_col() +
  labs(
    title = "Number of Genes per Gene Type",
    x     = "Gene Type",
    y     = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 8.6
ggplot(rna_seq, aes(x = Log2FC, color = Condition, fill = Condition)) +
  geom_density(alpha = 0.2) +
  labs(
    title = "Log2FC Distribution by Condition",
    x     = "Log2 Fold Change",
    y     = "Density"
  ) +
  theme_minimal()

# 8.7
ggplot(rna_seq, aes(x = Time_Point, y = Expression_Level, fill = Time_Point)) +
  geom_boxplot() +
  facet_wrap(~ Condition) +
  labs(
    title = "Expression Level by Time Point and Condition",
    x     = "Time Point",
    y     = "Expression Level"
  ) +
  theme_minimal() +
  theme(legend.position = "none")