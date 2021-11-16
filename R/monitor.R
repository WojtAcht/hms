MONITOR_LEVEL_NONE <- 0
MONITOR_LEVEL_BASIC <- 1
MONITOR_LEVEL_BASIC_TREE <- 2
MONITOR_LEVEL_VERBOSE_TREE <- 3

getMonitorLevel <- function(level_name) {
  if (level_name == "verbose_tree") {
    return(MONITOR_LEVEL_VERBOSE_TREE)
  }
  if (level_name == "basic_tree") {
    return(MONITOR_LEVEL_BASIC_TREE)
  }
  if (level_name == "basic") {
    return(MONITOR_LEVEL_BASIC)
  }
  if (level_name == "none") {
    return(MONITOR_LEVEL_NONE)
  }
  warning("Monitor level should be one of {'none', 'basic', 'basic_tree', 'verbose_tree'}")
  MONITOR_LEVEL_NONE
}
