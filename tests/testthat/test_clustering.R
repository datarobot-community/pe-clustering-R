# Primary unit tests.
# See helper_init_test_resources.R for configuration on test behavior.
# Se helper_mocks.R for mock functions.

test_that("Score object can be retrieved", {
  test_func <- function() {
    num_test_rows <- 2 # We don't need many rows for this test
    test_data <- with(test_resources, head(prediction_data, 2))
    scoring_object <- with(test_resources,
                           get_prediction_scoring_object(best_model, test_data)
    )
    expect_gte(nchar(scoring_object$id), 1)
  }

  if (!(TEST_WITH_LIVE_DATAROBOT_CONNECTION)) {
    with_mock(
      `datarobot::UploadPredictionDataset`=mock_UploadPredictionDataset,
      `datarobot::RequestPredictions`=mock_RequestPredictions,
      `datarobot::WaitForJobToComplete`=mock_WaitForJobToComplete,
      test_func()
      )
  } else {
    test_func()
  }
})

test_that("Error message is produced with missing features", {
  num_test_rows <- 2 # We don't need many rows for this test
  test_data <- with(test_resources, head(prediction_data, 2))
  mock_features_in_model <- mock_ListModelFeatures()
  mock_features_in_model <- mock_features_in_model[mock_features_in_model!=test_resources$outcome_name] # remove target
  features_to_test <- head(mock_features_in_model, 1) # prepare a reduced set of features, less than what is required for model
  expect_lt(length(features_to_test), length(mock_features_in_model))
  test_data <- test_data[,features_to_test] # remove required features
  with_mock(`datarobot::ListModelFeatures`=mock_ListModelFeatures, {
    expect_error({
      with(test_resources,
           check_required_features(best_model, test_data)
      )
    }, "missing required features")
  })
})

test_that("Required feature check does not fail with extraneous features", {
  with_mock(`datarobot::ListModelFeatures`=mock_ListModelFeatures, {

    num_test_rows <- 2 # We don't need many rows for this test
    test_data <- with(test_resources, head(prediction_data, 2))
    mock_features_in_model <- mock_ListModelFeatures()
    mock_features_in_model <- mock_features_in_model[mock_features_in_model!=test_resources$outcome_name] # remove target
    extraneous_feature <- 'asdf'
    expect_false(extraneous_feature %in% mock_features_in_model) # necessary for this test to make sense
    test_data[[extraneous_feature]] <- 1:nrow(test_data) # fill in random feature
    expect_silent(check_required_features(test_resources$best_model, test_data))
  })
})

test_that("Required feature check does not fail with missing target", {
  with_mock(`datarobot::ListModelFeatures`=mock_ListModelFeatures, {

    num_test_rows <- 2 # We don't need many rows for this test
    test_data <- with(test_resources, head(prediction_data, 2))
    expect_false(test_resources$outcome_name %in% colnames(test_data)) # necessary for this test to make sense
    expect_silent(check_required_features(test_resources$best_model, test_data))
  })
})


test_that("Prediction explanation dataframe can be retrieved", {
  test_func <- function() {
    num_test_rows <- 5 # We don't need many rows for this test
    test_data <- with(test_resources, head(prediction_data, num_test_rows))
    scoring_object <- with(test_resources,
                           get_prediction_scoring_object(best_model, test_data)
    )
    num_explanations <- 3 # We don't need many explanations for this test
    prediction_explanation_df <- with(test_resources,
                                      get_prediction_explanation_df(best_model, scoring_object, max_explanations = num_explanations)
    )

    if (TEST_WITH_LIVE_DATAROBOT_CONNECTION) {
      # This test is only meaningful if we are testing with a live connection.
      # Otherwise the explanation df has no relation to the test_data above
      expect_equal(nrow(prediction_explanation_df), num_test_rows)
    }

    expect_type(prediction_explanation_df$explanation1Strength, "double")
  }

  if (!(TEST_WITH_LIVE_DATAROBOT_CONNECTION)) {
    with_mock(
      `datarobot::UploadPredictionDataset`=mock_UploadPredictionDataset,
      `datarobot::RequestPredictions`=mock_RequestPredictions,
      `datarobot::WaitForJobToComplete`=mock_WaitForJobToComplete,
      `datarobot::GetFeatureImpact`=mock_GetFeatureImpact,
      `datarobot::RequestPredictionExplanations`=mock_RequestPredictionExplanations,
      `datarobot::GetPredictionExplanationsMetadataFromJobId`=mock_GetPredictionExplanationsMetadataFromJobId,
      `datarobot::GetPredictionExplanationsRowsAsDataFrame`=mock_GetPredictionExplanationsRowsAsDataFrame,
      `datarobot::RequestPredictionExplanationsInitialization`=mock_RequestPredictionExplanationsInitialization,
      test_func()
    )
  } else {
    test_func()
  }

})

test_that("Strength matrix can be created", {
  pe_frame <- test_resources$example_prediction_explanation_df
  num_explanations <- length(grep("FeatureName", colnames(pe_frame)))
  num_rows <- nrow(pe_frame)
  strength_matrix <- create_strength_matrix(pe_frame)

  expect_equal(nrow(strength_matrix), num_rows)

  num_nonzero_entries <- sum(strength_matrix!=0)
  # There should be only as many strength values as num_explanations*num_rows
  # We may have fewer, if there are strengths of zero.
  expect_lte(num_nonzero_entries, num_explanations*num_rows)
})


test_that("Reduction is correct", {
  strength_matrix <- test_resources$example_strength_matrix
  reduced_strengths <- reduce_dimensionality_strengths(strength_matrix, num_neighbors = 2)
  expect_equal(nrow(reduced_strengths), nrow(strength_matrix))
  expect_equal(ncol(reduced_strengths), 2)
})

test_that("cluster IDs are valid", {
  reduced_strengths <- test_resources$example_reduced_strengths

  # Test clustering
  cluster_ids <- cluster_reduced_strengths(reduced_strengths, min_points=30)
  expect_equal(length(cluster_ids), nrow(reduced_strengths))
  expect_lte(length(unique(cluster_ids)), nrow(reduced_strengths))
})

test_that("Top features can be extracted with a request for few features", {
  test_func <- function() {
    num_possible_features <- nrow(datarobot::GetFeatureImpact(test_resources$best_model))
    num_features_to_get <- num_possible_features - 1 # test requesting fewer features than possible
    top_feature_names <- with(test_resources, get_top_feature_names(best_model, num_features_to_get))
    expect_equal(length(top_feature_names), num_features_to_get)
    expect_type(top_feature_names, "character")
  }

  if (!(TEST_WITH_LIVE_DATAROBOT_CONNECTION)) {
    with_mock(
      `datarobot::GetFeatureImpact`=mock_GetFeatureImpact,
      test_func()
    )
  } else {
    test_func()
  }

})

test_that("Top features can be extracted with a request for too many features", {

  with_mock(
    `datarobot::GetFeatureImpact`=mock_GetFeatureImpact,
    {

      mock_fi <- datarobot::GetFeatureImpact(NULL)
      max_features <- nrow(mock_fi)
      requested_features <- max_features + 10
      top_feature_names <- with(test_resources, get_top_feature_names(best_model, requested_features))
      expect_equal(0, sum(is.na(top_feature_names)))
      expect_equal(length(top_feature_names), max_features)

    }
  )


})

test_that("Cluster summarization data can be created", {
  # Get example cluster plot data
  reduced_strengths <- test_resources$example_reduced_strengths
  cluster_ids <- cluster_reduced_strengths(reduced_strengths, min_points=30)
  num_clusters <- length(unique(cluster_ids))
  cluster_plot_data <- create_cluster_plot_data(reduced_strengths, cluster_ids)

  # subset test data to match example strengths
  scoring_df <- head(test_resources$prediction_data, nrow(reduced_strengths))

  # Choose an arbitrary set of columns to summarize
  features_to_summarize <- head(colnames(scoring_df), 3)

  # Create summary data
  output <- create_cluster_summarization_data(features_to_summarize, cluster_plot_data, scoring_df)

  # Test properties
  expect_equal(nrow(output), num_clusters)
  expect_equal(ncol(output), length(features_to_summarize) + 2) # plus 2 for cluster ID and n

  output_colnames <- colnames(output)
  expect_true("n" %in% output_colnames)
  expect_true("clusterID" %in% output_colnames)
  expect_true(all(features_to_summarize %in% output_colnames))
})

test_that("Computation of results works when supplied with mock data", {
  # Note: This is required for the vignette to be able to be generated!
  pe_frame <- test_resources$example_prediction_explanation_df
  scoring_df <- test_resources$prediction_data
  features_to_summarize <- names(table(test_resources$example_prediction_explanation_df$explanation1FeatureName)[1:3])
  results <- calculate_results(
    pe_frame,
    scoring_df,
    features_to_summarize,
    num_neighbors=50,
    min_dist=10^-100,
    min_points=25
  )

  expect_s3_class(results, "dataRobotPEClusterResults")
  expect_false(is.null(results))

})

test_that("Cluster and Summarize Prediction Explanations Function works completely", {
  test_func <- function(prediction_data) {

    results <- with(test_resources,
                    cluster_and_summarize_prediction_explanations(
                      best_model,
                      prediction_data,
                      num_neighbors = 30,
                      min_points = 5
                    )
    )

    expect_s3_class(results, "dataRobotPEClusterResults")

    expect_false(is.null(results))
    plot_data <- results$plot_data
    summary_data <- results$summary_data

    expect_equal(nrow(prediction_data), nrow(plot_data))
    num_clusters <- length(unique(plot_data$clusterID))
    expect_equal(nrow(summary_data), num_clusters)

    plot(results)

  }
  if (!(TEST_WITH_LIVE_DATAROBOT_CONNECTION)) {
    with_mock(
      `datarobot::ListModelFeatures`=mock_ListModelFeatures,
      `datarobot::UploadPredictionDataset`=mock_UploadPredictionDataset,
      `datarobot::RequestPredictions`=mock_RequestPredictions,
      `datarobot::WaitForJobToComplete`=mock_WaitForJobToComplete,
      `datarobot::GetFeatureImpact`=mock_GetFeatureImpact,
      `datarobot::RequestPredictionExplanations`=mock_RequestPredictionExplanations,
      `datarobot::GetPredictionExplanationsMetadataFromJobId`=mock_GetPredictionExplanationsMetadataFromJobId,
      `datarobot::GetPredictionExplanationsRowsAsDataFrame`=mock_GetPredictionExplanationsRowsAsDataFrame,
      `datarobot::RequestPredictionExplanationsInitialization`=mock_RequestPredictionExplanationsInitialization,
      {
        # must match mock prediction explanation rownum for the full functionality to work
        prediction_data_subset <- with(test_resources,
                                       head(prediction_data, nrow(example_prediction_explanation_df))
        )
        test_func(prediction_data_subset)
      }
    )
  } else {
    test_func(test_resources$prediction_data)
  }
})


