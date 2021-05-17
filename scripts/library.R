if (!require("pacman")) install.packages("pacman")
pkgs =
  c("distill",
    "here",
    "tidyverse",
    "plotly",
    "boot")

pacman::p_load(pkgs, character.only = T)
