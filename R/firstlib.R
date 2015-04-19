.First.lib <- function(lib, pkg)
{
   library.dynam("stl3", pkg, lib)
   cat("stl3 1.0 loaded\n")
}

