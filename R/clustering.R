check_required_features <- function(model_object, scoring_df) {
  # Ensure the scoring dataset has all the features needed for the model, minus the target
  model_features_and_target <- datarobot::ListModelFeatures(model_object) # NB: This will include the target!
  model_features_minus_target <- model_features_and_target[model_features_and_target!=model_object$projectTarget]
  missing_features <- setdiff(model_features_minus_target, colnames(scoring_df))
  if (length(missing_features) > 0) {
    missing_features_string <- paste(missing_features, collapse=' ')
    error_message <- paste("DataFrame for scoring is missing required features for the model:", missing_features_string)
    stop(error_message)
  }
}

get_prediction_scoring_object <- function(model_object, scoring_df) {
  project_id <- model_object$projectId

  # Getting Predictions -----------------------------------------------------

  # Uploading a dataset
  scoring <- datarobot::UploadPredictionDataset(project_id, scoring_df)

  # Requesting prediction
  predict.job.id <- datarobot::RequestPredictions(project = project_id,
                                       modelId = model_object$modelId,
                                       datasetId = scoring$id)
  # Wait for job
  datarobot::WaitForJobToComplete(project_id, predict.job.id, maxWait = 6000)

  return(scoring)
}

get_prediction_explanation_df <- function(model_object, scoring_object, max_explanations = 3) {
  project_id <- model_object$projectId

  # Prediction Explanations -------------------------------------------------

  # Computes Feature Impact (prereq for PEs)
  fi.request<-datarobot::GetFeatureImpact(model_object)

  pe.request.init<-datarobot::RequestPredictionExplanationsInitialization(model_object)
  datarobot::WaitForJobToComplete(project_id, pe.request.init, maxWait = 6000)

  pe.request <- datarobot::RequestPredictionExplanations(model_object, scoring_object$id, maxExplanations = max_explanations)

  pe.request.metadata <- datarobot::GetPredictionExplanationsMetadataFromJobId(project_id, pe.request)
  pe.frame <- datarobot::GetPredictionExplanationsRowsAsDataFrame(project_id, pe.request.metadata$id)
  return(pe.frame)
}

create_strength_matrix <- function(pe_frame) {
  # Creating a dataset of feature strength scores
  out <- list()
  for(i in 1:length(grep("FeatureName", colnames(pe_frame)))){
    tmp <- pe_frame[,c(paste0("explanation", i, "FeatureName"),
                       paste0("explanation", i, "Strength"))]
    colnames(tmp) <- c("name", "strength")
    out[[i]] <- cbind.data.frame(rowId = pe_frame$rowId, tmp)
  }
  strength_matrix <- reshape2::dcast(reshape2::melt(out, id = c('rowId', "name")), rowId+variable ~ name)
  strength_matrix[is.na(strength_matrix)] <- 0
  strength_matrix <- strength_matrix[,-c(1,2)]
  return(strength_matrix)
}

reduce_dimensionality_strengths <- function(strength_matrix, num_neighbors = 30, min_dist = 10^-100) {
  set.seed(10)
  custom.settings = umap::umap.defaults
  custom.settings$n_neighbors = num_neighbors # 30 = double of default
  custom.settings$min_dist = min_dist # threw warning at 0
  umap_out <- umap::umap(strength_matrix, custom.settings)
  return(umap_out$layout)
}

cluster_reduced_strengths <- function(reduced_strengths, min_points = 150) {
  # Clustering with HDBSCAN
  set.seed(10)
  cluster_output <- dbscan::hdbscan(reduced_strengths, minPts = min_points)
  return(cluster_output$cluster)
}

create_cluster_plot_data <- function(reduced_strengths, cluster_ids) {
  plot.data <- as.data.frame(reduced_strengths)
  colnames(plot.data) <- c("dim1", "dim2")
  plot.data$clusterID <- as.factor(cluster_ids)
  return(plot.data)
}

get_top_feature_names <- function(model_object, num_feature_summarizations) {
  fi <- datarobot::GetFeatureImpact(model_object) # need to block if FI is not already available

  # Calculate the number of features we can actually return, constrained by the number of available
  # features in the model.
  num_features_with_impact <- nrow(fi)
  num_feature_summarizations_actual <- min(num_features_with_impact, num_feature_summarizations)

  top_feature_names <- as.character(fi$featureName[1:num_feature_summarizations_actual])
  return(top_feature_names)

}

create_cluster_summarization_data <- function(features_to_summarize, cluster_plot_data, scoring_df) {
  if(!(all(features_to_summarize %in% colnames(scoring_df)))) {
    missing_features <- setdiff(features_to_summarize, colnames(scoring_df))
    stop(paste("Requested features not in scoring data:",paste(missing_features, collapse=",")))
  }

  prep.sum <- cbind(cluster_plot_data, scoring_df[,features_to_summarize]) %>%
    select(-c(.data$dim1, .data$dim2)) %>%
    group_by(.data$clusterID) %>%
    mutate(n = n())

  # Numeric summary
  num.sum <- prep.sum  %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)

  # Categorical summary
  cat.sum <- prep.sum  %>%
    summarise_if(is.factor, function(x) utils::tail(names(sort(table(x))),1))

  output <- full_join(num.sum, cat.sum, by = "clusterID")
  return(output)
}


# Internal method for coming up with the prediction explanation frame from source data
get_prediction_explanation_df_outer <- function(model_object, scoring_df, max_explanations) {
  check_required_features(model_object, scoring_df)
  scoring_object <- get_prediction_scoring_object(model_object, scoring_df)
  pe_frame <- get_prediction_explanation_df(model_object, scoring_object, max_explanations = max_explanations)
  return(pe_frame)
}



# Internal method for the bulk of operations, taking in the data provided by DataRobot
calculate_results <- function(
  pe_frame,
  scoring_df,
  features_to_summarize,
  num_neighbors,
  min_dist,
  min_points
  ) {
  strength_matrix <- create_strength_matrix(pe_frame)
  reduced_strengths <- reduce_dimensionality_strengths(strength_matrix, num_neighbors = num_neighbors, min_dist)
  cluster_ids <- cluster_reduced_strengths(reduced_strengths, min_points = min_points)
  cluster_plot_data <- create_cluster_plot_data(reduced_strengths, cluster_ids)
  cluster_summarization_data <- create_cluster_summarization_data(features_to_summarize, cluster_plot_data, scoring_df)

  results <- dataRobotPEClusterResults(
    plot_data = cluster_plot_data,
    summary_data = cluster_summarization_data,
    cluster_ids = cluster_ids,
    pe_frame = pe_frame,
    strength_matrix = strength_matrix
  )

  return(results)
}


#' Constructor for dataRobotPEClusterResults S3 class
#'
#' Constructor for dataRobotPEClusterResults S3 class. This constructor packages up provided data into a list with assigned class \code{dataRobotPEClusterResults}
#'
#' @param ... Results data to package together
dataRobotPEClusterResults <- function(...) {
  results <- list(...)
  class(results) <- 'dataRobotPEClusterResults'
  return(results)
}

#' Check for dataRobotPEClusterResults S3 class
#'
#' Function to check if an object is a dataRobotPEClusterResults object.
#'
#' @param object any \code{R} object
#' @export
is.dataRobotPEClusterResults <- function(object) {
  inherits(object, "dataRobotPEClusterResults")
}

#' Summary method for S3 objects of class dataRobotPEClusterResults
#'
#' Summarizes each Prediction Explanation cluster by aggregating feature values for the cluster members.
#'
#' Numerical features are summarized by their mean, whereas categorical variables are summarized
#' by their most frequent value.
#'
#' @param object The \code{dataRobotPEClusterResults} object
#' @param ... Additional arguments (unused)
#'
#' @return A summary data frame.
#' @export
summary.dataRobotPEClusterResults <- function(object, ...) {
  return((object$summary_data))
}

#' Plot method for S3 objects of class dataRobotPEClusterResults
#'
#' Plots observations on the reduced Prediction Explanation space, coloring by cluster.
#'
#' @param x The \code{dataRobotPEClusterResults} to plot
#' @param ... Additional arguments to pass to \code{plot.default}
#'
#' @export
plot.dataRobotPEClusterResults <- function(x, ...) {
  plot_data <- x$plot_data
  cluster_ids <- unique(plot_data$clusterID)
  cluster_colors <- grDevices::rainbow(n=length(cluster_ids))
  cluster_colors_by_point <- cluster_colors[match(plot_data$clusterID, cluster_ids)]
  with(plot_data,
       plot(dim1,dim2, col=cluster_colors_by_point, type='p', pch=19,
            xlab='Reduced Dimension 1',
            ylab='Reduced Dimension 2',
            main='Records by Prediction Explanation Cluster',
            ...)
  )
}

#' Cluster and Summarize Prediction Explanations
#'
#' Runs dimensionality reduction and clustering on prediction explanations, returning a dataset
#' suitable for visualization, a cluster-level summarization, and detailed results.
#'
#' @param model_object The \code{dataRobotModel} from which to source prediction explanations
#' @param scoring_df The DataFrame on which prediction explanations will be calculated
#' @param num_neighbors The number of neighbors argument for dimensionality reduction (see \code{n_neighbors} argument in \code{\link[umap]{umap.defaults}})
#' @param min_dist The minimum distance argument for dimensionality reduction (see \code{min_dist} argument in \code{\link[umap]{umap.defaults}})
#' @param min_points The minimum points parameter for clustering (see \code{minPts} argument in \code{\link[dbscan]{hdbscan}})
#' @param max_explanations The maximum number of prediction explanations to retrieve for each observation.
#' @param num_feature_summarizations How many features to include in the summarization of clusters (ordered by feature importance).
#' @return Returns an object of class \code{dataRobotPEClusterResults}, which has
#'   \code{\link[=summary.dataRobotPEClusterResults]{summary}} and
#'   \code{\link[=plot.dataRobotPEClusterResults]{plot}} methods. An object of
#'   this class is a list with the following components: \itemize{
#'   \item
#'   \code{plot_data}. A data frame containing the cluster IDs for each point
#'   from \code{scoring_df}, along with their location in the reduced prediction
#'   explanation space. See \code{\link{plot.dataRobotPEClusterResults}}.
#'   \item \code{summary_data}. A data frame containing one row per cluster,
#'   along with summarizations of the top features. See
#'   \code{\link{summary.dataRobotPEClusterResults}}.
#'   \item \code{cluster_ids} A
#'   vector of cluster IDs for each row from \code{scoring_df}.
#'   \item \code{strength_matrix}: A data frame of the prediction
#'   explanation strengths of each feature, for each row from \code{scoring_df}.
#'   \item \code{pe_frame}: The data frame returned by
#'   \code{\link[datarobot]{GetPredictionExplanationsRowsAsDataFrame}}
#'  }
#' @export
cluster_and_summarize_prediction_explanations <- function(
  model_object, scoring_df, num_neighbors = 30, min_dist = 10^-100, min_points = 150,
  max_explanations = 3, num_feature_summarizations = 10) {

  pe_frame <-  get_prediction_explanation_df_outer(
    model_object,
    scoring_df,
    max_explanations = max_explanations
    )

  features_to_summarize <- get_top_feature_names(model_object, num_feature_summarizations = num_feature_summarizations)


  results <- calculate_results(
    pe_frame,
    scoring_df,
    features_to_summarize,
    num_neighbors = num_neighbors,
    min_dist = min_dist,
    min_points = min_points
  )

  return(results)
}

