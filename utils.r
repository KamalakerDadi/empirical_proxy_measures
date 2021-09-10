
my_quants <- function(x)
{
  r <- quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

my_ggsave <- function(fname, plot, dpi = 300, ...)
{
  ggsave(paste0(fname, ".pdf"), plot = plot, useDingbats = F, bg = "#FFFFFF",
         ...)
  embedFonts(file = paste0(fname, ".pdf"),
             outfile = paste0(fname, ".pdf"))
  ggsave(paste0(fname, ".png"), plot = plot, dpi = dpi, bg = "#FFFFFF",
         ...)
}


get_extras <- function(path){
  # extra files
  extras <-  c(
    "alcohol.csv",
    "physical_activity.csv",
    "sleep.csv",
    "smoking.csv")

  # build lookup tables
  # This allows us to to elegantly (vectortized dict!) add extra info later.
  lookup_names <- fread(file.path(extra_path, "names.csv"))

  lookup_names <- setNames(
    unlist(lookup_names[2,-1]),
    unlist(lookup_names[1,-1]))

  lookup_family <- setNames(
    rep(c("Alcohol", "Sleep", "Smoking", "Activity"), c(16, 7, 2, 9)),
    unname(lookup_names))

  # read first file and do recursive join using DT syntax
  DT <- fread(file.path(extra_path, extras[1]))
  for (ii in 2:4){
    this_DT <- fread(file.path(extra_path, extras[ii]))
    DT <- DT[this_DT, on = "eid"]
  }

  # move to long format
  # kick out variable columns
  DT[,c('variable', 'i.variable', 'i.variable.1', 'i.variable.2')] <- NULL
  DT <- melt(
    DT,
    id.vars = c("eid"), measure.vars = names(DT)[-c(1)],
    variable.factor = F,
    variable.name = "var_code")

  # Add new column inplace using lookup trick. Then order by individual Ids
  DT[, name := lookup_names[var_code]]
  DT[, family := lookup_family[name]]
  setorder(DT, eid)
  DT
}


stack_wide <- function(DT)
{
  DT_wide <- data.table(expand.grid(
    eid = unique(DT$eid), variable = unique(DT$variable)))

  DT_tmp <- merge(
    DT_wide,
    DT[target == 'Age'][, c("eid", "variable")])

  setkey(DT_wide, eid, variable)
  DT_tmp <- DT[target == 'Age'][,
    .(age = true, age_pred = predicted, eid = eid, variable = variable)]

  setkey(DT_tmp, eid, variable)
  DT_wide[DT_tmp, `:=`(age = i.age, age_pred = i.age_pred)]

  DT_tmp <- DT[variable != 'Age, Sex' & target == 'Neuroticism'][,
      .(N = true, N_pred = predicted, eid = eid, variable = variable)]
  setkey(DT_tmp, eid, variable)
  DT_wide[DT_tmp, `:=`(N = i.N, N_pred = i.N_pred)]

  DT_tmp <- DT[variable != 'Age, Sex' & target == 'Fluid intelligence'][,
      .(FI = true, FI_pred = predicted, eid = eid, variable = variable)]
  setkey(DT_tmp, eid, variable)
  DT_wide[DT_tmp, `:=`(FI = i.FI, FI_pred = i.FI_pred)]

  DT_tmp <- DT[variable == 'Age, Sex' & target == 'Neuroticism'][,
      .(N = true, N_pred = predicted, eid = eid, variable = variable)]
  setkey(DT_tmp, eid, variable)
  DT_wide[DT_tmp, `:=`(N = i.N, N_pred = i.N_pred)]

  DT_tmp <- DT[variable == 'Age, Sex' & target == 'Fluid intelligence'][,
      .(FI = true, FI_pred = predicted, eid = eid, variable = variable)]
  setkey(DT_tmp, eid, variable)
  DT_wide[DT_tmp, `:=`(FI = i.FI, FI_pred = i.FI_pred)]
  DT_wide
}


get_extras2 <- function(path)
{
  # extra files
  extras <-  c(
    "alcohol.csv",
    "physical_activity.csv",
    "sleep.csv",
    "smoking.csv")

  summary_vars <- list(
    "Alcohol" = c(
      "Average weekly intake of other alcoholic drinks",
      "Average weekly fortified wine intake",
      "Average weekly spirits intake",
      "Average weekly beer plus cider intake",
      "Average weekly champagne plus white wine intake",
      "Average weekly red wine intake"),
    "Activity" = c("Summed MET minutes per week for all activity"),
    "Sleep" = c("Sleep duration"),
    "Smoking" = c("Pack years of smoking"))

  summary_names <- setNames(
    c("Summed weekly drinks", "Summed MET actvity", "Sleep duration",
      "Pack years"),
    names(summary_vars)
  )
  # build lookup tables
  # This allows us to to elegantly (vectortized dict!) add extra info later.
  lookup_names <- fread(file.path(extra_path, "names.csv"))

  lookup_codes <- setNames(
    unlist(lookup_names[1,-1]),
    unlist(lookup_names[2,-1]))

  lookup_family <- setNames(
    rep(c("Alcohol", "Sleep", "Smoking", "Activity"), c(16, 7, 2, 9)),
    names(lookup_codes))

  # read first file and do recursive join using DT syntax
  DTS <- NULL
  for (ii in seq_along(extras)){
    DT <- fread(file.path(extra_path, extras[ii]))
    family <- names(summary_vars)[[ii]]

    cols <- summary_vars[[family]]
    sel <- unname(lookup_codes[cols])
    DT <- DT[, c("eid", sel), with = F]
    # set  values < 0 to NA
    for(j in seq_along(DT)[-1]){
      set(DT, i = which(DT[[j]] < 0), j = j, value = NA)
    }
    # get absolutely NA rows
    na_index <- rowSums(is.na(DT[, sel, with = F])) == length(sel)
    # compute sums
    DT[, value := rowSums(.SD, na.rm = T), .SDcols = sel]
    # replaces wrong zeros for NAs
    DT$value[na_index] <- NA
    DT[, family := family]  # set family
    DT[, name := summary_names[family]]
    DT <- DT[, -sel, with = F]  # kick out inputs
    DTS[[ii]] <- DT  # append to list
  }
  DTS <- do.call(rbind, DTS)
  DTS[, eid := factor(eid)]
  setorder(DTS, eid)
  DTS
}

fit_train_deconfound <- function(train, test, var, confound,
                                 default.fml = "%s ~ poly(%s, degree = 2)")
{
  # fit confounding model on train set
  train <- na.omit(train[, c(confound, var), with = F])
  var_s <- paste0(var, "_s")
  conf_s <- paste0(confound, "_s")

  train[[var_s]] <- scale(train[[var]])
  train[[conf_s]] <- scale(train[[confound]])

  fml <- as.formula(
    sprintf(default.fml, var_s, conf_s))
  mod_confound <- lm(fml, train)

  # apply on test set
  test[[var_s]] <- scale(test[[var]],
                         attr(train[[var_s]], "scaled:center"),
                         attr(train[[var_s]], "scaled:scale"))
  test[[conf_s]] <- scale(test[[confound]],
                          attr(train[[conf_s]], "scaled:center"),
                          attr(train[[conf_s]], "scaled:scale"))
  attributes(test[[var_s]]) <- NULL
  attributes(test[[conf_s]]) <- NULL
  list(pred = predict(mod_confound, test),
       mod = mod_confound)
}
read_file_parts <- function(fname, path){
  fnames <- list.files(
    path, paste0(strsplit(fname, ".csv")[[1]], "_"), full.names = T)
  rbindlist(lapply(fnames, fread))
}
