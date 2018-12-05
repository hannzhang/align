# This is an example workflow of my project.
#
#
# First, draw the x and y axis for the plot.
  draw_compare(x, y)

# Align the two inputs sequences.
# In this example, i will randomly choose two inputs.
  align("ACTCGCAATATGVTAGGVVAG", "ACTTTATGCTATGCGC")

# Call getColor to give different colors for each nucleotide.
  getColor("A")
  getColor("C")
  getColor("G")
  getColor("T")

# The output can be found in a file called plot.png
# [END]
