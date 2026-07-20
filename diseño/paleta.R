library(scales)
# library(colorspace)
# 
# sequential_hcl(5, h = c(300, 100))

colores <- c("#593957", "#E3467C")

# extender la paleta a 8 colores
degradado <- colorRampPalette(colores)(4)

shades::swatch(degradado)
