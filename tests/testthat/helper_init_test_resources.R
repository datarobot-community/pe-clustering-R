# Script for creation of test resources.
# Configure with defined constants below.

# Enable this flag to test against a live DataRobot instance.
# See below variables for options on whether a
# new project is created, versus an existing project.
TEST_WITH_LIVE_DATAROBOT_CONNECTION <- FALSE

# Enable this flag to refresh the prediction explanation data used for testing.
# This should only be necessary if the prediction explanation return data format changes.
# This will only be used if TEST_WITH_LIVE_DATAROBOT_CONNECTION is set to true.
REFRESH_OFFLINE_TEST_DATA_WITH_LIVE_DATAROBOT_CONNECTION <- FALSE && TEST_WITH_LIVE_DATAROBOT_CONNECTION

# Data to use for testing (Note: paths for files will be relative to the testthat folder)
data('PimaIndiansDiabetes', package='mlbench')
DATA_FOR_TESTING <- PimaIndiansDiabetes


# Outcome name for testing. Must be present in DATA_FOR_TESTING
OUTCOME_NAME_FOR_TESTING <- 'diabetes'

# Whether, in the case of live testing, to use an existing project,
# versus create an entirely new project.
# This will only be used if TEST_WITH_LIVE_DATAROBOT_CONNECTION is true.
# The project and model IDs must be speicifed in tests/testthat/test_project_model_ids.yaml
# WARNING: If you create a new project, you will be responsible for cleaning it up!
# The project must correspond to the dataset specified in DATASET_FOR_TESTING
# as well as the outcome name specified in OUTCOME_NAME_FOR_TESTING
USE_EXISTING_PROJECT_FOR_LIVE_TESTING <- TRUE

# Create the resources necessary for testing. If the TEST_WITH_LIVE_DATAROBOT_CONNECTION flag is
# enabled, a live connection to DataRobot will be used, otherwise only mock tests will run.
# In the case of a live connection, you may optionally specific project ID
# and model ID to use if you have an existing project using the test data,
# otherwise a new project will be created.
create_test_resources <- function(dataset, outcome_name) {

  # Verify outcome fits
  if(!(outcome_name %in% colnames(dataset))) {
    stop(paste("Outcome column specified: \"",outcome_name,"\" is not in specified test data", sep=""))
  }

  training = dataset

  # we don't need truly separate testing data to verify functionality, but make sure we exclude the target
  training_colnames <- colnames(dataset)
  testing_colnames <- training_colnames[training_colnames!=outcome_name]
  testing = dataset[,testing_colnames]


  if(TEST_WITH_LIVE_DATAROBOT_CONNECTION) {

    if(!(USE_EXISTING_PROJECT_FOR_LIVE_TESTING)) {
      # Creating project in cloud
      PROJECT_NAME<-"Automated testing for R package datarobot.pe.clustering"
      project_object <- datarobot::SetupProject(dataSource = training, projectName = PROJECT_NAME)
      project_id <- project_object$projectId
      print("Project ID for created project is:")
      print(project_id)

      # Sets target feature and kicks off modeling
      SetTarget(project_object, target = outcome_name, mode = 'manual')

      # Isolate a logistic regression blueprint
      blueprints_list <- datarobot::ListBlueprints(project_id)
      logistic_regression_blueprint <- Filter(
        function(x) ("Logistic Regression"==x$modelType) && ("DataRobot"==x$blueprintCategory),
        blueprints_list
      )[[1]]

      # Run just the logistic regression model
      model_job_id <- datarobot::RequestNewModel(project_id, logistic_regression_blueprint)
      best_model <- datarobot::GetModelFromJobId(project_id, model_job_id)

    } else {

      # Read in project and (optional) model IDs from a YAML file.
      if (!requireNamespace("yaml", quietly = TRUE)) {
        stop("Package \"yaml\" needed to read in an existing project ID from disk. Please install it, or alter this code to hardcode project and model IDs.",
             call. = FALSE)
      }

      project_and_model_ids <- yaml::read_yaml('test_project_model_ids.yaml')
      project_id <- project_and_model_ids$project
      model_id <- project_and_model_ids$model

      # Fetch project based on ID. This project should use the same test dataset
      project_object = tryCatch({
        datarobot::GetProject(project_id)
      }, error = function(e) {
        stop(paste('Error fetching project for testing. Check tests/testthat/test_project_model_ids.yaml. Exception: ',e))
      })
      if (is.null(model_id)) {
        # Extract best model from project
        models_list <- Filter(function(x) x$samplePct >= 64, datarobot::ListModels(project_object))
        if (length(models_list) >= 1) {
          best_model <- models_list[[1]]
        } else {
          stop('Unable to find a model for testing in the supplied project with >= 64% sample.')
        }
      } else {
        # Fetch the specified model
        best_model = tryCatch({
          datarobot::GetModel(project_id, model_id)
        }, error = function(e) {
          stop(paste('Error fetching model for testing. Check tests/testthat/test_project_model_ids.yaml. Exception: ',e))
        })
      }
    }

    if(REFRESH_OFFLINE_TEST_DATA_WITH_LIVE_DATAROBOT_CONNECTION) {
      # Refresh the saved test data for offline testing from these results
      pe_frame <- datarobot.pe.clustering:::get_prediction_explanation_df_outer(
        best_model,
        testing,
        max_explanations=3)
      saveRDS(pe_frame, 'example_prediction_explanation_df.Rds')
    }

  } else {
    # Local tests
    project_object <- list(projectId='test')
    #project_object <- datarobot:::as.dataRobotProject(project_object) # not exported
    best_model <- list(modelId='test', projectId='test', projectTarget=outcome_name)
    #best_model <- datarobot:::as.dataRobotModel(best_model) # not exported
  }

  # Prepare example reduced strength matrix suitable for clustering
  cluster_sizes <- c(100,100)
  cluster_x_means <- c(5, 0)
  cluster_y_means <- c(10, 2)
  reduced_strengths <- rbind(
    data.frame(x=rnorm(cluster_sizes[1], cluster_x_means[1], 1), y=rnorm(cluster_sizes[1], cluster_y_means[1], 1)),
    data.frame(x=rnorm(cluster_sizes[2], cluster_x_means[2], 1), y=rnorm(cluster_sizes[2], cluster_y_means[2], 1))
  )

  # Extract feature names (i.e., not the outcome)
  test_feature_names <- colnames(testing)
  test_feature_names <- test_feature_names[test_feature_names!=outcome_name]


  # Prepare a simple Prediction Explanation strength matrix suitable for reduction
  strength_matrix <- data.frame(
    feature_a=c(0,0.2,-0.3,0,0),
    feature_b=c(0,0.2,0,-0.3,0),
    feature_c=c(0,0,0.2,0,-0.3)
  )


  return(
    list(
      project = project_object,
      prediction_data = testing,
      best_model = best_model,
      example_prediction_explanation_df = readRDS("example_prediction_explanation_df.Rds"),
      example_strength_matrix = strength_matrix,
      example_reduced_strengths = reduced_strengths,
      outcome_name = outcome_name,
      feature_names = test_feature_names
    )
  )
}

# Create test resources based on user parameters
test_resources <- create_test_resources(dataset = DATA_FOR_TESTING,
                                        outcome_name = OUTCOME_NAME_FOR_TESTING
                                        )
