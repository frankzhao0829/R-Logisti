TransClassInputs <- function(x, cols, cutoff, keep.levels = NULL, group.na = FALSE,
                             prefix = 'TG_', rare.group.label = '_OTHER_',
                             max.label.length = NULL, label.case = 'default') {

  # Simulate Transfom Variables Node in SAS Enterprise Miner specificed to class inputs
  # Group rare levels based on frequency cutoff
  # NOTE:
  # 1. group.na sets whether to group NA into the rare group label
  # 2. keep.levels will control which levels are kept even its frequency is below cutoff
  # 3. The new level label length is thresheld by max.label.length
  # 4. The new level label can be turned to uppercase by setting upper.label = TRUE

  t <- subset(x, select = cols)
  col_names <- names(t)

  res <- NULL
  level_map <- list()

  attr(level_map, 'cutoff') <- cutoff
  attr(level_map, 'keep.levels') <- keep.levels
  attr(level_map, 'group.na') <- group.na
  attr(level_map, 'prefix') <- prefix
  attr(level_map, 'rare.group.label') <- rare.group.label
  attr(level_map, 'max.label.length') <- max.label.length
  attr(level_map, 'label.case') <- label.case

  for(n in col_names) {

    if(cutoff >= 1) {

      freq_table <- sort(table(t[[n]], exclude = c(NA, keep.levels)), decreasing = TRUE)
      name_map <- ifelse(seq_along(freq_table) <= cutoff, names(freq_table), rare.group.label)
      name_map <- setNames(name_map, names(freq_table))

    } else {

      freq_table <- table(t[[n]], exclude = c(NA, NaN, keep.levels)) / length(t[[n]][!is.na(t[[n]])])
      name_map <- ifelse(freq_table > cutoff, names(freq_table), rare.group.label)

    }

    if (!is.null(keep.levels)) {
      name_map <- c(name_map, setNames(keep.levels, keep.levels))
    }

    if (!is.null(max.label.length)) {
      name_map <- substr(name_map, 1, max.label.length)
    }

    if (label.case == 'upper') {
      name_map <- toupper(name_map)
    } else if (label.case == 'lower') {
      name_map <- tolower(name_map)
    }

    r <- unname(name_map[match(as.character(t[[n]]), names(name_map))])

    if (group.na) {
      r[is.na(r)] <- rare.group.label
    }

    level_map[[n]] <- name_map
    res <- cbind(res, r)

  }

  res <- as.data.frame(res)

  names(res) <- paste0(prefix, col_names)

  return(list(output = res, mapping = level_map))

}

ApplyClassTrans <- function(x, mapping, group.new.level = 'keep') {
  # Apply the Class Transformation to x based on mapping passed in
  # Use this function to transform test/validate dataset

  res <- NULL

  for(n in names(mapping)) {

    m <- mapping[[n]]
    t <- as.character(x[[n]])

    if (group.new.level == 'rare') {
      t <- ifelse(t %in% names(m), m[t], ifelse(is.na(t), NA, attr(mapping, 'rare.group.label')))
    } else if (group.new.level == 'missing') {
      t <- ifelse(t %in% names(m), m[t], NA)
    } else {
      t <- ifelse(t %in% names(m), m[t], t)
    }

    if (attr(mapping, 'label.case') == 'upper') {
      t <- toupper(t)
    } else if (attr(mapping, 'label.case') == 'lower'){
      t <- tolower(t)
    }

    ml <- attr(mapping, 'max.label.length')
    if (!is.null(ml)) {
      t <- substr(t, 1, ml)
    }

    if (attr(mapping, 'group.na')) {
      t[is.na(t)] <- attr(mapping, 'rare.group.label')
    }

    res <- cbind(res, t)
  }

  res <- as.data.frame(res)
  names(res) <- paste0(attr(mapping, 'prefix', TRUE), names(mapping))

  return(res)
}
