already_installed <- as.data.frame(installed.packages())
rownames(already_installed) <- NULL
already_installed <- already_installed[, "Package"]
head(already_installed)
# write.table(out, file="packages_list.txt", sep="\t", 
#             row.names=FALSE, col.names=FALSE)

to_install <- read.table("packages_list.txt")
to_install <- to_install[,1]
head(to_install)

uninstalled <- setdiff(to_install, already_installed)
print(uninstalled)
install.packages(uninstalled, repos="https://cloud.r-project.org")
