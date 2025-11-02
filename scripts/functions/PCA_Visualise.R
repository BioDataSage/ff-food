#' Create Interactive PCA Biplot with Plotly
#'
#' Creates a customized interactive PCA biplot using plotly with purple theme.
#' Displays all individuals as points, labels top contributors, and shows scaled variable loadings.
#'
#' @param pca_result A PCA result object from FactoMineR::PCA() containing:
#'   - $ind$coord: Individual coordinates on principal components
#'   - $ind$contrib: Individual contributions to each component
#'   - $var$coord: Variable loadings (correlations with PCs)
#'   - $var$contrib: Variable contributions to each component
#'   - $eig: Eigenvalues and variance explained
#'
#' @param n_top_individuals Integer. Number of top contributing individuals to label.
#'   Default is 5. These are selected based on total contribution to PC1 and PC2.
#'
#' @param n_top_variables Integer. Number of top contributing variables to display as arrows.
#'   Default is 5. These are selected based on total contribution to PC1 and PC2.
#'
#' @param scale_factor Numeric. Scaling factor to multiply variable loadings for visibility.
#'   Default is 25. Increase if arrows are too small, decrease if too large.
#'
#' @param label_nudge Numeric. Factor to position variable labels beyond arrow tips.
#'   Default is 1.15 (15% beyond arrow tip). Increase to avoid overlap.
#'
#' @param x_range Numeric vector of length 2. Fixed range for x-axis (PC1) as c(min, max).
#'   Default is c(-10, 2). Set to NULL for automatic range.
#'
#' @param y_range Numeric vector of length 2. Fixed range for y-axis (PC2) as c(min, max).
#'   Default is c(-5, 30). Set to NULL for automatic range.
#'
#' @param individuals_name Character. Name for individuals in the legend.
#'   Default is "Countries".
#'
#' @param variables_name Character. Name for variables in the legend.
#'   Default is "Indicators".
#'
#' @param plot_title Character. Main title for the plot.
#'   Default is "PCA Biplot: Top Contributors & Key Variables".
#'
#' @param point_size Numeric. Size of individual points. Default is 6.
#'
#' @param point_color Character. Hex color code for individual points. Default is '#9370DB' (medium purple).
#'
#' @param arrow_color Character. Hex color code for variable arrows. Default is '#FF6B9D' (pink).
#'
#' @param arrow_width Numeric. Width of variable arrows. Default is 3.
#'
#' @param fixedrange Logical. If TRUE, disables zoom and pan. Default is FALSE.
#'
#' @return A plotly object containing the interactive PCA biplot.
#'
#' @examples
#' # Basic usage
#' library(FactoMineR)
#' library(plotly)
#' data(decathlon2)
#' res.PCA <- PCA(decathlon2[, 1:10], graph = FALSE)
#' plot_pca_biplot(res.PCA)
#'
#' # Customize parameters
#' plot_pca_biplot(
#'   pca_result = res.PCA,
#'   n_top_individuals = 10,
#'   n_top_variables = 7,
#'   scale_factor = 30,
#'   x_range = c(-5, 5),
#'   y_range = c(-5, 5),
#'   plot_title = "My Custom PCA Biplot"
#' )
#'
#' @export
plot_pca_biplot <- function(pca_result,
                            n_top_individuals = 5,
                            n_top_variables = 5,
                            scale_factor = 25,
                            label_nudge = 1.15,
                            x_range = c(-10, 2),
                            y_range = c(-5, 30),
                            individuals_name = "Countries",
                            variables_name = "Indicators",
                            plot_title = "PCA Biplot: Top Contributors & Key Variables",
                            point_size = 6,
                            point_color = '#9370DB',
                            arrow_color = '#FF6B9D',
                            arrow_width = 3,
                            fixedrange = FALSE) {
  
  # Validate inputs
  if (!inherits(pca_result, "PCA")) {
    stop("pca_result must be a PCA object from FactoMineR::PCA()")
  }
  
  if (n_top_individuals < 0 || n_top_variables < 0) {
    stop("n_top_individuals and n_top_variables must be non-negative")
  }
  
  # Get all individuals coordinates
  all_ind <- as.data.frame(pca_result$ind$coord[, c("Dim.1", "Dim.2")])
  
  # Get top contributing individuals for labeling
  ind_contrib_total <- rowSums(pca_result$ind$contrib[, 1:2])
  top_ind_names <- names(sort(ind_contrib_total, decreasing = TRUE)[1:n_top_individuals])
  special <- as.data.frame(pca_result$ind$coord[top_ind_names, c("Dim.1", "Dim.2")])
  
  # Get variance explained percentages
  var_explained <- pca_result$eig[1:2, 2]
  pc1_var <- round(var_explained[1], 2)
  pc2_var <- round(var_explained[2], 2)
  
  # Get top contributing variables
  var_contrib <- rowSums(pca_result$var$contrib[, 1:2])
  top_var_names <- names(sort(var_contrib, decreasing = TRUE)[1:n_top_variables])
  var_coords <- as.data.frame(pca_result$var$coord[top_var_names, c("Dim.1", "Dim.2")])
  
  # Scale variable coordinates for visibility
  var_coords_scaled <- var_coords * scale_factor
  
  # Position variable labels to avoid overlap
  var_labels <- var_coords_scaled * label_nudge
  var_labels$textposition <- sapply(1:nrow(var_labels), function(i) {
    x <- var_labels$Dim.1[i]
    y <- var_labels$Dim.2[i]
    if (x > 0 & y > 0) return("top right")
    if (x < 0 & y > 0) return("top left")
    if (x > 0 & y < 0) return("bottom right")
    return("bottom left")
  })
  
  # Create plotly plot
  p <- plot_ly() %>%
    # All individuals as points
    add_markers(data = all_ind, x = ~Dim.1, y = ~Dim.2, 
                name = individuals_name,
                marker = list(size = point_size, 
                              color = point_color,
                              opacity = 0.7,
                              line = list(color = '#6A0DAD', width = 1)),
                hoverinfo = 'text',
                text = ~rownames(all_ind)) %>%
    # Top contributing individual labels
    add_text(data = special, x = ~Dim.1, y = ~Dim.2, 
             text = ~rownames(special),
             textposition = "top center",
             textfont = list(size = 12, color = '#4B0082', family = 'Arial Black'),
             showlegend = FALSE) %>%
    # Variable arrows (scaled)
    add_segments(data = var_coords_scaled, 
                 x = 0, y = 0, xend = ~Dim.1, yend = ~Dim.2,
                 line = list(color = arrow_color, width = arrow_width),
                 name = variables_name,
                 showlegend = TRUE,
                 hoverinfo = 'text',
                 text = ~rownames(var_coords_scaled)) %>%
    # Variable labels
    add_text(data = var_labels, x = ~Dim.1, y = ~Dim.2,
             text = ~rownames(var_labels),
             textposition = ~textposition,
             textfont = list(color = '#C71585', size = 8, family = 'Arial', weight = 'bold'),
             showlegend = FALSE) %>%
    layout(
      title = list(
        text = plot_title,
        font = list(size = 18, color = '#4B0082', family = 'Arial Black')
      ),
      xaxis = list(
        title = paste0("PC1 (", pc1_var, "% variance)"),
        gridcolor = '#E6E6FA',
        zerolinecolor = '#9370DB',
        titlefont = list(color = '#6A0DAD'),
        range = x_range,
        fixedrange = fixedrange
      ),
      yaxis = list(
        title = paste0("PC2 (", pc2_var, "% variance)"),
        gridcolor = '#E6E6FA',
        zerolinecolor = '#9370DB',
        titlefont = list(color = '#6A0DAD'),
        range = y_range,
        fixedrange = fixedrange
      ),
      plot_bgcolor = '#F8F8FF',
      paper_bgcolor = 'white',
      legend = list(
        bgcolor = '#F0E6FF',
        bordercolor = '#9370DB',
        borderwidth = 1
      )
    )
  
  return(p)
}

# Example usage:
# plot_pca_biplot(res.PCA)
# 
# Or with custom parameters:
# plot_pca_biplot(
#   pca_result = res.PCA,
#   n_top_individuals = 10,
#   n_top_variables = 7,
#   scale_factor = 30,
#   x_range = c(-8, 6),
#   y_range = c(-10, 40),
#   individuals_name = "Observations",
#   variables_name = "Features",
#   plot_title = "My Custom Biplot"
# )