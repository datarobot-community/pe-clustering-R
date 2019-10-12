# A series of mock functions to imitate live DataRobot client connectivity.
# WARNING: These mocks are far from complete, and only imitate the limited features
# needed for this package to work.

mock_ListModelFeatures <- function(...) {
  # Mock for datarobot::ListModelFeatures
  test_feature_names <- test_resources$feature_names
  # Note: datarobot::ListModelFeatures() returns target too
  target <- test_resources$outcome_name
  # Return target plus a few features, to simulate a model using a reduced set of features
  return(c(target, head(test_feature_names,3)))
}

mock_UploadPredictionDataset <- function(...) {
  return(list(id='test'))
}

mock_RequestPredictions <- function(...) {
  return('test')
}

mock_WaitForJobToComplete <- function(...) {
  return()
}

mock_GetFeatureImpact <- function(...) {
  test_feature_names <- test_resources$feature_names
  # Note that as this package only uses feature impact to order features, no impact scores are
  # mocked here.
  out <- data.frame(featureName=head(test_feature_names,5))
  return(out)
}

mock_RequestPredictionExplanations <- function(...) {
  return('job_id')
}

mock_GetPredictionExplanationsMetadataFromJobId <- function(...) {
  return(list(id='metadata_id'))
}

mock_GetPredictionExplanationsRowsAsDataFrame <- function(...) {
  return(
    test_resources$example_prediction_explanation_df
  )
}

mock_RequestPredictionExplanationsInitialization <- function(...) {
  return('job_id')
}
