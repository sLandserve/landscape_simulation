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
#' @return RasterLayer
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

