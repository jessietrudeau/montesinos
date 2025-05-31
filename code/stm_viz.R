library(ggplot2)
library(tibble)
library(dplyr)
library(quanteda)
library(stm)
library(tidyr)


dfm_general<-readRDS("../dfm_general.rds")
dfm_turns<-readRDS("../dfm_turns.rds")






#---------------------------------Prep-----------------------------------


# 1. Convert dfm → STM inputs (with ALL docs & metadata)
stm_input <- convert(
  x       = dfm_general,
  to      = "stm",
  docvars = docvars(dfm_general)
)
stm_input$meta$type <- as.factor(stm_input$meta$type)

# 2. Subset *before* prepping: drop any docs with NA type
keep <- !is.na(stm_input$meta$type)
docs2 <- stm_input$documents[keep]
meta2 <- stm_input$meta[keep, , drop = FALSE]

# 3. Now prep these docs & vocab together
processed2 <- prepDocuments(
  documents = docs2,
  vocab     = stm_input$vocab,   # original vocab
  meta      = meta2
)






#-------------------------------20 Topics--------------------------------


# Fit intercept‐only STM on clean subset
set.seed(123)
stm_filt <- stm(
  documents  = processed2$documents,
  vocab      = processed2$vocab,       # remapped, gap‐free
  K          = 20,
  prevalence = ~ 1,
  data       = processed2$meta,
  max.em.its = 75
)

# Regress topic proportions on type
effect_type2 <- estimateEffect(
  1:20 ~ type,
  stmobj      = stm_filt,
  metadata    = processed2$meta,
  uncertainty = "Global"
)

# Inspect
summary(effect_type2)
plot(effect_type2, covariate = "type", topics = 18, model = stm_filt,
     main = "Topic 2 Prevalence by Actor Type")



pdf("topic_plot_(20_topics).pdf", width = 12, height = 10)

# Loop over all 20 topics
for (k in 1:20) {
  plot(
    effect_type2,
    covariate = "type",
    topics = k,
    model = stm_filt,
    method = "pointestimate",
    labeltype = "prob",
    main = paste("Topic", k, "Prevalence by Actor Type"),
    xlab = "Type",
    ylab = "Expected Topic Proportion"
  )
}

dev.off()






#--------------------------------10 Topics-----------------------------


# Fit your intercept‐only STM on the clean subset
set.seed(123)
stm_filt_2 <- stm(
  documents  = processed2$documents,
  vocab      = processed2$vocab,       # remapped, gap‐free
  K          = 10,
  prevalence = ~ 1,
  data       = processed2$meta,
  max.em.its = 75
)

# Regress topic proportions on type
effect_type2_2 <- estimateEffect(
  1:10 ~ type,
  stmobj      = stm_filt_2,
  metadata    = processed2$meta,
  uncertainty = "Global"
)


# Inspect
summary(effect_type2_2)
plot(effect_type2_2, covariate = "type", topics = 1, model = stm_filt_2,
     main = "Topic 1 Prevalence by Actor Type")






#---------------------------------5 Topics-----------------------------


# Fit your intercept‐only STM on the clean subset
set.seed(123)
stm_filt_3 <- stm(
  documents  = processed2$documents,
  vocab      = processed2$vocab,       # remapped, gap‐free
  K          = 5,
  prevalence = ~ 1,
  data       = processed2$meta,
  max.em.its = 75
)

# Regress topic proportions on type
effect_type2_3 <- estimateEffect(
  1:5 ~ type,
  stmobj      = stm_filt_3,
  metadata    = processed2$meta,
  uncertainty = "Global"
)


# Inspect
summary(effect_type2_3)
plot(effect_type2_3, covariate = "type", topics = 1, model = stm_filt_3,
     main = "Topic 1 Prevalence by Actor Type")






#---------------------------------0 Topics------------------------------


# Fit your intercept‐only STM on the clean subset
set.seed(123)
stm_filt_4 <- stm(
  documents  = processed2$documents,
  vocab      = processed2$vocab,       # remapped, gap‐free
  K          = 0,
  prevalence = ~ 1,
  data       = processed2$meta,
  max.em.its = 75
)

K <- length(stm_filt_4$mu)

# Regress topic proportions on type
effect_type2_4 <- estimateEffect(
  1:K ~ type,
  stmobj      = stm_filt_4,
  metadata    = processed2$meta,
  uncertainty = "Global"
)


# Inspect
summary(effect_type2_4)
plot(effect_type2_4, covariate = "type", topics = 1, model = stm_filt_4,
     main = "Topic 1 Prevalence by Actor Type")






#------------------------------Date Regressions---------------------------






#------------------------------------Prep---------------------------------


# Convert dfm → STM inputs (with ALL docs & metadata)
stm_input <- convert(
  x       = dfm_general,
  to      = "stm",
  docvars = docvars(dfm_general)
)
stm_input$meta$date <- as.factor(stm_input$meta$date)

# Subset *before* prepping: drop any docs with NA type
keep <- !is.na(stm_input$meta$date)
docs2 <- stm_input$documents[keep]
meta2 <- stm_input$meta[keep, , drop = FALSE]

# Now prep these docs & vocab together
processed2 <- prepDocuments(
  documents = docs2,
  vocab     = stm_input$vocab,   # original vocab
  meta      = meta2
)






##-------------------------------20 Topics--------------------------------


set.seed(123)
date_stm <- stm(
  documents  = processed2$documents,
  vocab      = processed2$vocab,       # remapped, gap‐free
  K          = 20,
  prevalence = ~ 1,
  data       = processed2$meta,
  max.em.its = 75
)

# Regress topic proportions on type
effect_type2 <- estimateEffect(
  1:20 ~ date,
  stmobj      = date_stm,
  metadata    = processed2$meta,
  uncertainty = "Global"
)


# Inspect
summary(effect_type2)
plot(effect_type2, covariate = "date", topics = 1, model = date_stm,
     main = "Topic 1 Prevalence by Date")






##---------------------------------10 Topics--------------------------------


# Fit your intercept‐only STM on the clean subset
set.seed(123)
date_stm_2 <- stm(
  documents  = processed2$documents,
  vocab      = processed2$vocab,       # remapped, gap‐free
  K          = 10,
  prevalence = ~ 1,
  data       = processed2$meta,
  max.em.its = 75
)

# Regress topic proportions on type
effect_type2_2 <- estimateEffect(
  1:10 ~ date,
  stmobj      = date_stm_2,
  metadata    = processed2$meta,
  uncertainty = "Global"
)


# Inspect
summary(effect_type2_2)
plot(effect_type2_2, covariate = "date", topics = 1, model = date_stm_2,
     main = "Topic 1 Prevalence by Date")






##-----------------------------------5 Topics-------------------------------


set.seed(123)
date_stm_3 <- stm(
  documents  = processed2$documents,
  vocab      = processed2$vocab,       # remapped, gap‐free
  K          = 5,
  prevalence = ~ 1,
  data       = processed2$meta,
  max.em.its = 75
)

# Regress topic proportions on type
effect_type2_3 <- estimateEffect(
  1:5 ~ date,
  stmobj      = date_stm_3,
  metadata    = processed2$meta,
  uncertainty = "Global"
)


# Inspect
summary(effect_type2_3)
plot(effect_type2_3, covariate = "date", topics = 1, model = date_stm_3,
     main = "Topic 1 Prevalence by Date")






##-----------------------------------0 Topics-------------------------------


set.seed(123)
date_stm_4 <- stm(
  documents  = processed2$documents,
  vocab      = processed2$vocab,       # remapped, gap‐free
  K          = 0,
  prevalence = ~ 1,
  data       = processed2$meta,
  max.em.its = 75
)

K <- length(date_stm_4$mu)

# Regress topic proportions on type
effect_type2_4 <- estimateEffect(
  1:K ~ date,
  stmobj      = date_stm_4,
  metadata    = processed2$meta,
  uncertainty = "Global"
)


# Inspect
summary(effect_type2_4)
plot(effect_type2_4, covariate = "date", topics = 1, model = date_stm_4,
     main = "Topic 1 Prevalence by Date")






