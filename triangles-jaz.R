# Title: Triangles Postcard
# Description: Postcard with multiple colored triangles depicting JAZ pattern
# Author: Gaston Sanchez
# Date: April-6-2024

#-------------------------------------------
# triangles of odd rows
#-------------------------------------------
xs1_up = c(0.0, 0.1, 0.05)
ys1_up = c(0.0, 0.0, 0.1)

# inverted triangles
xs1_down = c(0.05, 0.15, 0.1)
ys1_down = c(0.1, 0.1, 0.0)

#-------------------------------------------
# triangles of even rows
#-------------------------------------------
xs2_up = c(0.05, 0.15, 0.1)
ys2_up = c(0.0, 0.0, 0.1)
xsteps = seq(from = 0.0, to = 0.9, by = 0.1)
ysteps = seq(from = 0.0, to = 0.9, by = 0.1)

# inverted triangles
xs2_down = c(0.0, 0.1, 0.05)
ys2_down = c(0.1, 0.1, 0.0)


#-------------------------------------------
# indices
#-------------------------------------------
num_of_rows = 10
triangs_per_row = 2 * length(xsteps) - 1
indices = seq(from = 1, to = num_of_rows * triangs_per_row)
inds = matrix(indices, nrow = num_of_rows, ncol = triangs_per_row, byrow = TRUE)

# selected triangles (by index)
selected1 = c(99, 100, 101, 102, 121, 122, 141, 142) # J
selected2 = c(63, 64, 83, 84, 103, 104, 123, 105, 86, 87, 68, 69) #A
selected3 = c(88:90, 69, 70, 49:53) # Z

v = as.vector(inds)


#-------------------------------------------
# color palette (desert like hues)
#-------------------------------------------
set.seed(2024)

cols = sample(c("#FBF1E2", "#D7CEC2", "#84C9BD"), 
              prob = c(0.4, 0.4, 0.15), 
              size = nrow(inds) * ncol(inds), replace = TRUE)

# color-coding selected traingles
cols[v %in% selected1] = "#ff3a24"  # J
cols[v %in% selected2] = "#ff8f45"  # A
cols[v %in% selected3] = "#FFC84E"  # Z

cols = matrix(cols, nrow(inds), ncol(inds))


#----------------------------------------------
# Plot exported to PNG file (dimensions 6x8 in)
#----------------------------------------------
png("triangles-jaz.png", width = 6, height = 8, units="in", res = 300)
op = par(bg = "white", mar = rep(0,4))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
# triangles in odd rows
for (row in c(1, 3, 5, 7, 9)) {
  for (col in 1:10) {
    polygon(xs1_up + xsteps[col], ys1_up + ysteps[row], 
            col = cols[row, 2*(col-1)+1], border = "white", lwd = 2)
  }
  for (col in 1:9) {
    polygon(xs1_down + xsteps[col], ys1_down + ysteps[row], 
            col = cols[row, 2*col], border = "white", lwd = 2)
  }
}
# triangles in even rows
for (row in c(2, 4, 6, 8, 10)) {
  for (col in 1:9) {
    polygon(xs2_up + xsteps[col], ys2_up + ysteps[row], 
            col = cols[row, 2*col], border = "white", lwd = 2)
  }
  for (col in 1:10) {
    polygon(xs2_down + xsteps[col], ys2_down + ysteps[row], 
            col = cols[row, 2*(col-1)+1], border = "white", lwd = 2)
  }
}
par(op)
dev.off()
