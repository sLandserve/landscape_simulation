#' nlm_es
#'
#' @description This is based on the \code{nlm_neigh} function in the
#'   \code{NLMR} package (Sciaini et al. 2018). Create a neutral landscape model
#'   for ecosystem services where we control the fragmentation of supply and
#'   demand seperately, as well as the interspersion of these. We also control
#'   the amount of each. This also has applications in situations where the
#'   amount, fragmentation and interspersion of two habitats within a matrix
#'   need to be simulated.
#'
#' @details The algorithm draws a random cell and turns it into a given category
#'   (supply or demand) based on the probabilities \code{p_supply} and
#'   \code{p_demand}, respectively. For assigning supply cells, the decision is
#'   based on the probability \code{p_supply}, if there is any cell in the
#'   Moore- or Von-Neumann-neighborhood, otherwise it is based on
#'   \code{p_empty}. Similarly, for demand cells, the decision is based on the
#'   probability \code{p_demand}, if there is any cell in the Moore- or
#'   Von-Neumann-neighborhood, otherwise it is based on \code{p_empty}. By
#'   default, the Von-Neumann-neighborhood is used to check adjacent cells.
#'
#' @param ncol [\code{numerical(1)}]\cr Number of columns for the raster.
#' @param nrow  [\code{numerical(1)}]\cr Number of rows for the raster.
#' @param resolution [\code{numerical(1)}]\cr Resolution of the raster.
#' @param f_supply [\code{numerical(1)}]\cr Probability to turn a cell into
#'   supply if any neighbour cell is supply (in the range [0, 1] with 1 being
#'   the least fragmented)
#' @param p_supply [\code{numerical(1)}]\cr Total proportion of supply cells in
#'   the landscape
#' @param f_demand [\code{numerical(1)}]\cr Probability to turn into supply if
#'   any neighbour cell is supply (in the range [0, 1] with 1 being the least
#'   fragmented)
#' @param p_demand [\code{numerical(1)}]\cr Total proportion of demand cells in
#'   the landscape
#' @param f_empty [\code{numerical{1}}]\cr Probability an empty cell receives a
#'   value (supply or demand)
#' @param inter [\code{numerical{1}}]\cr Amount of interspersion between supply
#'   and demand (in the range [0, 1] with 1 being complete interspersion)
#' @param neighborhood [\code{string(1)}]\cr The neighborhood used to determined
#'   adjacent cells: `"Moore"` takes the eight surrounding cells, while
#'   `"Von-Neumann"` takes the four adjacent cells (i.e. left, right, upper and
#'   lower cells).
#'
#' @return RasterLayer with 0, 1, 2. 0 = neutral, 1 = supply, 2 = demand
#'
#' @references Scherer, Cédric, et al. "Merging trait-based and individual-based
#'   modelling: An animal functional type approach to explore the responses of
#'   birds to climatic and land use changes in semi-arid African savannas."
#'   \emph{Ecological Modelling} 326 (2016): 75-89. \cr Marco Sciaini, Matthias
#'   Fritsch, Craig E. Simpkins (2018). {NLMR}: Simulating neutral landscape
#'   models. R package version 0.2.0. URL
#'   https://CRAN.R-project.org/package=NLMR
#'
#' @examples
#' # simulate neighborhood model
#' es_raster <- nlm_neigh(ncol = 20, nrow = 20, f_supply = 0.5, p_supply = 0.1,
#'                        f_demand = 0.8, p_demand = 0.1, inter = 0.5)
#'
#' \dontrun{
#' # visualize the NLM
#' util_plot(es_raster)
#' }
#'
#' @aliases nlm_es
#' @rdname nlm_es
#'
#' @export

# for now, while still being sourced in, need to load the appropriate libraries

nlm_neigh <-
  function(ncol,
           nrow,
           resolution = 1,
           f_supply,
           p_supply,
           f_demand,
           p_demand,
           inter,
           f_empty = 0.5, # I may actually just want to fix this in the code, rather than allowing it to be fixed. 
           neighborhood = "Von-Neumann") { # think about which is the more sensible neighbourhood to take
    
    # Check function arguments ----
    checkmate::assert_count(ncol, positive = TRUE)
    checkmate::assert_count(nrow, positive = TRUE)
    checkmate::assert_numeric(f_supply, lower = 0, upper = 1)
    checkmate::assert_numeric(p_supply, lower = 0, upper = 1)
    checkmate::assert_numeric(f_demand, lower = 0, upper = 1)
    checkmate::assert_numeric(p_demand, lower = 0, upper = 1)
    checkmate::assert_numeric(inter, lower = 0, upper = 1)
    checkmate::assert_numeric(f_empty, lower = 0, upper = 1)
    checkmate::assert_subset(neighborhood, c("Von-Neumann", "Moore"))
    
    # Determine cells per category
    no_cells <- c(p_supply * nrow * ncol, p_demand * nrow * ncol)
    f_vec <- c(f_supply, f_demand)
    
    # Create an empty matrix of correct dimensions + additional 2 rows and
    # columns ----
    matrix <- matrix(0, ncol + 2, nrow + 2)
    
    # Keep applying random clusters until all elements have a value -----
    
    for(i in 1:2) {
      j <- 0
      
      while (j < no_cells[i]) {
        
        # Pick random cell within correct dimensions and with value 0 ----
        s <- which(matrix[2:(nrow + 1), 2:(ncol + 1)] == 0, arr.ind = TRUE)
        s <- s[sample(nrow(s), 1), ]
        row <- as.integer(s[1]) + 1
        col <- as.integer(s[2]) + 1
        
        # Check neighborhood of that cell ----
        if (neighborhood == "Von-Neumann") {
          adjacent <- c(
            matrix[row - 1, col    ], # upper
            matrix[row, col - 1], # left
            matrix[row, col + 1], # right
            matrix[row + 1, col    ]
          ) # lower
        }
        if (neighborhood == "Moore") {
          adjacent <- c(
            matrix[row - 1, col - 1], # upper left
            matrix[row - 1, col    ], # upper
            matrix[row - 1, col + 1], # upper right
            matrix[row, col - 1], # left
            matrix[row, col + 1], # right
            matrix[row + 1, col - 1], # lower left
            matrix[row + 1, col    ], # lower
            matrix[row + 1, col + 1]
          ) # lower right
        }
        
        # calculate r upfront to make comparison to two probs in latter step
        r <- stats::runif(1, 0, 1)
        # get the unique values of adjacent cells
        u <- unique(adjacent)
        
        # This is where I have made the main change. We have 3 options:
        
        # 1. u = 0 - use f_empty
        if (u == 0) {
          if (r < f_empty) {
            matrix[row, col] <- i
            j <- j + 1
          }
        }
        
        # 2. u = [0, i] or i - use f_vec[i]
        if (identical(u, c(0, i)) || u == i) {
          if(r < f_vec[i]) {
            matrix[row, col] <- i
            j <- j + 1
          }
        }
          
        # 3. u = [0, 1, i] or [1, i] - use f_vec[i] & inter
        if (identical(u, c(0, 1, i)) || identical(u, c(1, i))) {
          if (r < f_vec[i] & r < inter) {
            matrix[row, col] <- i
            j <- j + 1
          } 
        }
          
        # Update boundary conditions
        matrix[1, ] <- matrix[nrow + 1, ]
        matrix[nrow + 2, ] <- matrix[2, ]
        matrix[, 1] <- matrix[, ncol + 1]
        matrix[, ncol + 2] <- matrix[, 2]
      } # close while j
    } # close for i
    
    # Cut additional cells and transform to raster ----
    rndes_raster <- raster::raster(matrix[
      1:nrow + 1,
      1:ncol + 1
      ])
    
    # specify resolution ----
    raster::extent(rndes_raster) <- c(
      0,
      ncol(rndes_raster) * resolution,
      0,
      nrow(rndes_raster) * resolution
    )

    return(rndes_raster)
  }
