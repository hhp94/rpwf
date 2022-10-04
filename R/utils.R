# use_description()
# ?use_description()
# ?devtools::install_deps()

dep_imp = c("DBI", "parsnip", "hardhat", "recipes", "dplyr", "rlang")
for(i in dep_imp){
 use_package(i, type = "Imports", min_version = NULL)
}
