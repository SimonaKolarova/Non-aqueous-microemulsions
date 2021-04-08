# Statistical analysis
  # For normally distributed data of homogeneous variance run one-way ANOVA test and return Tukey Honest Significant Differences
  # For normally distributed data of non-homogeneous variance run Welch one-way test and return pairwise t-test
  # For non-normally distributed data run Kruskal-Wallis rank sum test and return Wilcoxon pairwise comparison

run_statistical_analysys <- function(data, dep_variable,ind_variable) {
  if (normality_test(data, dep_variable,ind_variable) == TRUE && variance_test(data, dep_variable,ind_variable) == TRUE) {
    cat("Data normally distributed.\n")
    cat("Variance comparable across groups.\n")
    if (anova_test(data, dep_variable,ind_variable) == TRUE) {
      results <- tukey_test(data, dep_variable,ind_variable)
      return(results)
    }
  } else if (normality_test(data, dep_variable,ind_variable) == TRUE && variance_test(data, dep_variable,ind_variable) == FALSE) {
    cat("Data normally distributed.\n")
    cat("Homogeneity of variance assumption not met.\n")
    if (welch_test(data, dep_variable,ind_variable) == TRUE) {
      results <- pairwise_t_test(data, dep_variable,ind_variable)
      return(results)
    }  
  } else {
    cat("Data not normally distributed.\n")
    if (kruskal_test(data, dep_variable,ind_variable) == TRUE) {
      results <- wilcoxon_test(data, dep_variable,ind_variable)
      return(results)
    }  
  }
}


# Normality test (Shapiro-Wilk test)
normality_test <- function(data, dep_variable,ind_variable) {
  data_aov <- aov(dep_variable ~ ind_variable, data = data)
  data_shapiro <- shapiro.test(residuals(data_aov))
  if (data_shapiro[[2]] > 0.05) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Homogeneity of variance test (Bartlett’s test)
variance_test <- function(data, dep_variable,ind_variable) {
  data_bartlett <- bartlett.test(dep_variable ~ ind_variable, data = data)
  if (data_bartlett[[3]] > 0.05) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# One-way ANOVA test  
anova_test <- function(data, dep_variable,ind_variable) {
  data_anova <- aov(dep_variable ~ ind_variable, data = data)
  if (summary(data_anova)[[1]]["Pr(>F)"] < 0.05) {
    cat("One-way ANOVA test: There are significant differences between the groups. \n")
    return(TRUE)
  } else {
    cat("One-way ANOVA test: There are no significant differences between the groups. \n")
    return(FALSE)
  }
}

# Welch one-way test
welch_test <- function(data, dep_variable,ind_variable) {
  data_welch <- oneway.test(dep_variable ~ ind_variable, data = data)
  if (data_welch[[3]] < 0.05) {
    cat("One-way Welch test: There are significant differences between the groups. \n")
    return(TRUE)
  } else {
    cat("One-way Welch test: There are no significant differences between the groups. \n")
    return(FALSE)
  }  
}

# Kruskal-Wallis rank sum test
kruskal_test <- function(data, dep_variable,ind_variable) {
  data_kruskal <- kruskal.test(dep_variable ~ ind_variable, data = data)
  if (data_kruskal[[3]] < 0.05) {
    cat("Kruskal-Wallis rank sum test: There are significant differences between the groups. \n")
    return(TRUE)
  } else {
    cat("Kruskal-Wallis rank sum test: There are no significant differences between the groups. \n")
    return(FALSE)
  }  
}

# Pairwise comparisons between groups (Tukey Honest Significant Differences)
tukey_test <- function(data, dep_variable,ind_variable) {
  data_aov <- aov(dep_variable ~ ind_variable, data = data)
  data_tukey <- TukeyHSD(data_aov)
  return(data_tukey)
}  

# Pairwise t-test
pairwise_t_test <- function(data, dep_variable,ind_variable) {
  data_pttest <- pairwise.t.test(dep_variable, ind_variable, p.adjust.method = "BH", pool.sd = FALSE)
  return(data_pttest)
}  

# Pairwise comparisons (Wilcoxon rank sum test)
wilcoxon_test <- function(data, dep_variable,ind_variable) {
  data_wilcox <- pairwise.wilcox.test(dep_variable, ind_variable, exact=FALSE, p.adjust.method = "BH")
  return(data_wilcox)
}  