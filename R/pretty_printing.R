## Helper functions for formatting and pretty-printing values


tidy_fixed_effects <- function(model) {
  UseMethod("tidy_fixed_effects")
}

tidy_fixed_effects.glmerMod <- function(model) {
  fixefs <- broom::tidy(model, "fixed") %>%
    rename(Parameter = term, Estimate = estimate, SE = std.error,
           z = statistic, p = p.value)
  fixefs
}

tidy_fixed_effects.lmerMod <- function(model) {
  fixefs <- broom::tidy(model, "fixed") %>%
    rename(Parameter = term, Estimate = estimate, SE = std.error,
           t = statistic) %>%
    # p values
    mutate(p = normal_approx(t))
  fixefs
}

# Extract formula from a model, convert to string
get_formula_literal <- . %>%
  formula %>%
  as.character %>%
  extract(c(2,1,3)) %>%
  paste(collapse = " ")

# Rewrite variable names inside of formulas to more desciptive labels
rewrite_formula <- . %>%
  str_replace_all("ot(\\d)", "Time^\\1^") %>%
  str_replace_all("Subj", "Child") %>%
  str_replace_all("elogit", "LogOdds") %>%
  str_replace_all("cAge", "Age") %>%
  str_replace_all("cEVT_GSV", "EVT") %>%
  str_replace_all("bbias", "TargetInitial") %>%
  str_replace_all("Bias", "TargetInitial")







# Low-level text manipulation
remove_leading_zero <- function(xs) str_replace(xs, "^0", "")
leading_minus_sign <- function(xs) str_replace(xs, "^-", "&minus;")

# Replace NAs with a given value
make_na_replacer <- function(y) {
  function(xs) ifelse(is.na(xs), y, xs)
}
blank_nas <- make_na_replacer("")
zero_nas <- make_na_replacer(0)

# Make an html tag function
make_tagger <- function(tag) {
  template <- paste0("<", tag, ">%s</", tag, ">")
  function(xs) sprintf(template, xs)
}
emphasize <- make_tagger("em")
subscript <- make_tagger("sub")
h3 <- make_tagger("h3")

# Wrap text in parentheses
parenthesize <- function(xs, parens = c("(", ")")) {
  paste0(parens[1], xs, parens[2])
}

# Make an equation with non-breaking spaces
html_equation <- function(x, y) paste0(x, "&nbsp;=&nbsp;", y)

# convert "a:b" into "b:a"
flip_over_sep <- function(xs, sep = ":") {
  p1 <- paste0("(\\w+)", sep, "(\\w+)")
  p2 <- paste0("\\2", sep, "\\1")
  str_replace(xs, p1, p2)
}

# paste_after("a","b") yields "ba". Used for building up equations in pipelines.
paste_after <- function(xs, ys) paste0(ys, xs)

mean_range <- function(label, xs) {
  sprintf("%s = %.2f (%s-%s)", label, mean(xs, na.rm = TRUE), min(xs), max(xs))
}


# P-values: Include up to 2 sig digits in decimal notation, drop leading zero,
# use less-than .001 for really small values.
format_pval <- function(ps, html = FALSE) {
  template <- ifelse(html, "&lt;&nbsp;.001", "< .001")
  p_num <- ps
  p_chr <- formatC(p_num, digits = 2, format = "fg") %>%
    remove_leading_zero %>%
    ifelse(p_num < 0.001, template, .)
  p_chr
}


is_same_as_last <- function(xs) {
  same_as_last <- xs == lag(xs)
  # Overwrite NA from lag(xs)
  same_as_last[1] <- FALSE
  same_as_last
}



# Given an ANOVA model comparison and a model name, give the model comparison
# results in the format "chi-square(x) = y, p = z" (as pretty markdown, e.g.
# "_&chi;_^2^(1) = 2.9, _p_ = 0.091")
chi_line <- function(anova_results, model_name) {
  row <- anova_results[which(row.names(anova_results) == model_name), ]

  # Format p
  sp_eq_sp <- "&nbsp;=&nbsp;"
  p_guide <- ifelse(row[["Pr(>Chisq)"]] < 0.001, "&nbsp;", sp_eq_sp)
  p <- format_pval(row[["Pr(>Chisq)"]], html = TRUE)

  sprintf("_&chi;_^2^(%s)%s%.2g, _p_%s%s",
          row[["Chi Df"]], sp_eq_sp, row[["Chisq"]], p_guide, p)
}

# Remove ot terms from a parameter name
remove_times <- function(x) {
  x <- x %>%
    # Break up interactions
    str_split(":") %>%
    # Remove times
    lapply(function(xs) str_replace(xs, "ot\\d|.Intercept.", "")) %>%
    unlist %>%
    Filter(Negate(is_blank), .)

  # Restore interactions when there is more than one term
  ifelse(length(x > 1), str_c(x, collapse = ":"), x)
}

is_blank <- function(xs) xs == ""

# Convert parameter names into HTML gammas with subscripts, so that
# "ot2:cAge_1:cEVT_GSV" becomes
# "<em>&gamma;</em><sub>2<em>Age&times;EVT</em></sub>"
make_gammas <- function(model) {
  coef_names <- model %>%
    tidy_fixed_effects %>%
    select(Parameter) %>%
    unlist(use.names = FALSE)

  times <- coef_names %>% str_extract(regex("(?<=ot)\\d")) %>% zero_nas

  # Remove time-related terms from the parameter names
  no_times <- coef_names %>% lapply(remove_times) %>% unlist %>% blank_nas

  gammas <- no_times %>%
    # Rename variables
    str_replace("cEVT_GSV", "EVT") %>%
    str_replace("cPPVT_GSV", "PPVT") %>%
    str_replace("Age_1", "Age") %>%
    str_replace("cAge", "Age") %>%
    str_replace_all(":", "&times;") %>%
    str_replace("bbiasTarget", "OnTarget")  %>%
    str_replace("BiasTarget", "OnTarget")  %>%
    str_replace("biasPhonological", "Distractor(Phon.)")  %>%
    str_replace("biasSemantic", "Distractor(Sem.)")  %>%
    str_replace("biasUnrelated", "Distractor(Unrel.)")  %>%
    # Italicize variable names, except for blank ones
    emphasize %>% str_replace("<em></em>", "") %>%
    # Complete the subscript
    paste_after(times) %>% subscript %>%
    paste_after(emphasize("&gamma;"))
  gammas
}

# Print fixed-effect rows as [γ1Age: β = 0.11, SE = 0.02, t = 3.86, p <].
report_rows <- function(model) {
  fixed_effects <- tidy_fixed_effects(model) %>%
    rename(B = Estimate) %>%
    select(-Parameter)

  # Format values in each column
  round_2 <- function(xs) round(xs, 2) %>% leading_minus_sign
  round_3 <- function(xs) round(xs, 3) %>% leading_minus_sign
  fixed_effects <- fixed_effects %>%
    mutate(B = round_3(B), SE = round_3(SE), p = format_pval(p, html = TRUE))

  # format z column if glmerMod, otherwise format t
  stat_name <- ifelse(class(model) == "glmerMod", "z", "t")
  fixed_effects[[stat_name]] <- round_2(fixed_effects[[stat_name]])

  # set B to beta and emphasize all column names
  names(fixed_effects) <- names(fixed_effects) %>%
    str_replace_all("^[B]$", "&beta;") %>%
    emphasize


  # Make an equation for each value in a column, using form Colname = Value
  report_column <- function(column) {
    Map(report_cell, names(column), column) %>% unlist %>% unname
  }

  # Create HTML equations for each column
  Bs <- report_column(fixed_effects[1])
  SEs <- report_column(fixed_effects[2])
  stats <- report_column(fixed_effects[3])
  ps <- report_column(fixed_effects[4])

  # Generate inline stat report
  gammas <- paste0(make_gammas(model), ": ")
  paste(Bs, SEs, stats, ps, sep = ", ") %>% paste_after(gammas)
}

# Pretty print equation for fixed effect
report_cell <- function(name, value) {
  cell <- html_equation(name, value) %>%
    # Protect p < .0001 cells
    str_replace("=&nbsp;&lt;", "&lt;")
  cell
}

