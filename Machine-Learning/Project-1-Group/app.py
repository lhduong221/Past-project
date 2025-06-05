############################
####### INFORMATION ########
############################
# for best view of the app #
#  please open in browser  #
############################


###################
#### LIBRAIRES ####
###################
from shiny import App, render, ui
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.tree import DecisionTreeClassifier, plot_tree
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import roc_curve, auc, confusion_matrix, accuracy_score
import seaborn as sns

#########################################
rng = 22  # set random seed (22 was used)
#########################################

##############
#### DATA ####
##############
df = pd.read_csv("Assignment 1/smm636-GCW1_data.csv")

# Preprocessing
df.drop(columns='id', inplace=True)                              # drop ID column
df['diagnosis'] = LabelEncoder().fit_transform(df['diagnosis'])  # encode diagnosis as 0 or 1

# Sample data to balance classes and reduce numeber of observations to 500
df_m = df[df['diagnosis'] == 1]                                             # dataset of malignant tumors
df_b = df[df['diagnosis'] == 0].sample(n=500 - len(df_m), random_state=rng)  # under-sampled dataset of benign tumors
df_sample = pd.concat([df_m, df_b])                                         # combine datasets
diagnosis = df_sample['diagnosis']                                          # target variable
features = df_sample.drop(columns=['diagnosis'])                            # features

# Split data into training and testing sets ensuring the same proportion of classes test and train
X_train, X_test, y_train, y_test = train_test_split(
    features, 
    diagnosis, 
    test_size=0.2,        # 80% train, 20% test (i.e. 400 train, 100 test)
    random_state=rng, 
    stratify = diagnosis  # ensures the same proportion of classes in test and train
)


####################
#### DEFINE UI #####
####################
app_ui = ui.page_fluid(
    ui.h1("Breast Cancer Diagnosis", class_="text-center"),                                    # heading
    ui.h4("Machine Learning Models' Results for Tumor Classification", class_="text-center"),  # subheading

    # Navigation panel allows to switch between different tabs, first tab is the introduction
    ui.navset_pill(
        ui.nav_panel("Introduction",
                ui.column(12,
                    ui.h2("Context"),
                    ui.p("This application uses 2 machine learning models to classify tumors as either benign or malignant. "
                         "We use Decision Trees and Random Forest models to analyze feature importance, visualize decision trees, "
                         "and compare performance using ROC curves."),
                    ui.p("Navigate through the tabs to explore different aspects of the analysis."),
                    ui.h2("Objective"),
                    ui.p("The goal is to understand how different features contribute to tumor classification "
                         "and visualize model performance."),
                    ui.h2("Dataset Overview"),
                    ui.p("Tumors are either malignant or benign, meaning they are either cancerous or non-cancerous respectively. "
                         "The dataset contains measurements of tumor nuclei with 30 different attributes,"
                         "measuring the tumor nuclei with 3 different measurements: "
                         "mean, standard deviation, and worst (most extreme value). Malignant tumors tend to be larger, "
                         "with a more jagged/irregular shape.")
                )
        ),

        # P.S. inputs of nav_panel vary little, therefore will be explained once
        ## DECISION TREE
        ui.nav_panel("Decision Tree",
            ui.layout_sidebar(                                   # layout_sidebar allows to have a sidebar and a plot for a cleaner look
                ui.sidebar(
                    ui.input_slider(                             # input_slider allows to select the alpha within a range, 
                                    "alpha_dt",                  # 'alhpa_dt' is the variable name of the input to use in the server
                                    "Select alpha for Decision Tree:", 
                                    min=0,                       # minimum value of alpha
                                    max=0.05,                    # maximum value of alpha
                                    value=0,                     # default value of alpha (i.e. best alpha found)
                                    step=0.001                   # step size to increase/decrease alpha
                    ),
                    style="height: 850px;"                       # style to adjust the height/width of the sidebar and plot
                ),
                ui.output_plot("decision_tree_plot")             # output_plot to display the plot
            )
        ),

        ## FEATURE IMPORTANCE
        ui.nav_panel("Feature Importance",
            ui.layout_sidebar(
                ui.sidebar(
                    ui.input_slider("num_trees_rf", "Number of trees for Random Forest:", min=1, max=1000, value=500, step=1),    # 500 trees were used to create the Random Forest model
                    ui.input_slider("num_features_rf", "Number of features for Random Forest:", min=1, max=30, value=2, step=1),  # 2 featuer found to be best to maximize AUC
                    style="height: 850px;"
                ),
                ui.output_plot("feature_importance_plot")
            )
        ),

        ## ROC CURVE
        ui.nav_panel("ROC Curve",
            ui.layout_sidebar(
                ui.sidebar(
                    ui.input_slider("alpha_roc", "Select alpha (ccp_alpha) for ROC Curve:", min=0, max=0.05, value=0, step=0.001),           # alpha slider for ROC
                    ui.input_slider("num_trees_roc", "Number of trees for ROC (0 for Decision Tree):", min=1, max=1000, value=500, step=1),  # number of trees slider for ROC
                    ui.input_slider("num_features_roc", "Number of features (max_features) for ROC:", min=1, max=30, value=2, step=1),       # number of features slider for ROC
                    style="height: 800px;"
                ),
                ui.output_plot("roc_curve_plot"),
                style = "width: 1200px;"
            )
        ),

        ## CONFUSION MATRICES & ACCURACY
        ui.nav_panel("Confusion Matrices",
            ui.layout_sidebar(
                ui.sidebar(
                    ui.input_slider("alpha_confm", "Select alpha (ccp_alpha) for Decision Tree Confusion Matrix:", min=0, max=0.05, value=0, step=0.001),  # alpha slider for Confusion Matrix
                    ui.input_slider("num_trees_confm", "Number of trees for RF Confusion Matrix:", min=1, max=1000, value=500, step=1),                    # number of trees slider for Confusion Matrix
                    ui.input_slider("num_features_confm", "Number of features for RF Confusion Matrix:", min=1, max=30, value=2, step=1),                  # number of features slider for Confusion Matrix
                    ui.input_slider("threshold_confm", "Threshold for RF Prediction:", min=0.1, max=0.9, value=0.5, step=0.1),                             # threshold slider for Confusion Matrix
                    style="height: 700px;"
                ),
                ui.output_plot("confusion_matrix_combined"),  # confusion matrix plots
                ui.output_text("confusion_matrix_accuracy"),  # confusion matrix accuracy values
                style = "width: 1100px;"
            )
        )
    )
)


#############################
#### DEFINE SERVER LOGIC ####
#############################
def server(input, output, session):  # input are the sliders, output are the plot (and accuracy)
    ## DECISION TREE ##
    @output
    @render.plot                                                                   # specifies that function below is a plot
    def decision_tree_plot():
        # Fit the dt model
        dt = DecisionTreeClassifier(ccp_alpha=input.alpha_dt(), random_state=rng)  # makes the dt model dynamic, through the alpha slider
        dt.fit(X_train, y_train)
        
        # Plot the decision tree
        fig, ax = plt.subplots(figsize=(6, 18), dpi=300)
        plot_tree(dt,
                  feature_names=X_train.columns,        # extracts the col names of the dataset (i.e. features)
                  class_names=['Benign', 'Malignant'],  # specifies nodes to predict 'malignant' or 'benign' instead of 1 & 0 for interpretability
                  filled=True,
                  ax=ax,
                  impurity=True,                        # Ensures to show the Gini coef.
                  rounded=True)
        
        return fig
    
    ## FEATURE IMPORTNACE ##
    @output
    @render.plot
    def feature_importance_plot():
        # Fit the rf model
        rf = RandomForestClassifier(n_estimators=input.num_trees_rf(),            # makes the rf model dynamic, through the tree number slider
                                           max_features=input.num_features_rf(),  # makes the rf model dynamic, through the number of tuning hyperparameters slider
                                           bootstrap=True,                        # use bootstrap sampling
                                           oob_score=True,                        # use out-of-bag observations for model evaluation
                                           random_state=rng)
        rf.fit(X_train, y_train)

        feature_importances = rf.feature_importances_                               # collects the rf assigned feature importances, based on reduction of impurity
        forest_importances = pd.Series(feature_importances, index=X_train.columns)  # convert the feature importance array into pd series
        sorted_importances = forest_importances.sort_values()                       # sort the feature importances in ascending order
        
        # Plot the feature importance bar chart
        fig, ax = plt.subplots(figsize=(12, 6))
        sorted_importances.plot.bar(ax=ax, width=0.8)
        ax.set_ylabel("Mean Decrease in Impurity")
        ax.set_title("Feature Importance Ranking")
        ax.set_ylim(0, 0.55)
        ax.set_xticks(range(len(sorted_importances)))  # ensure to show all features at all times for comparison purpose
        ax.set_xticklabels(sorted_importances.index, rotation=45, ha="right", fontsize=10)
        fig.tight_layout()
        
        return fig

    ## ROC CURVE ##
    @output
    @render.plot
    def roc_curve_plot():
        # Fit dt model
        dt = DecisionTreeClassifier(ccp_alpha=input.alpha_roc(), random_state=rng)
        dt.fit(X_train, y_train)
        yscores_tree_m = dt.predict_proba(X_test)[:, 1]  # Get dt probabilities for positive class

        # Fit rf model
        rf = RandomForestClassifier(n_estimators=input.num_trees_roc(),
                                        max_features=input.num_features_roc(),
                                        bootstrap=True,
                                        oob_score=True,
                                        random_state=rng)
        rf.fit(X_train, y_train)
        yscores_rf_m = rf.predict_proba(X_test)[:, 1]  # Get dt probabilities for positive class

        # Compute dt ROC curve
        fpr_dt, tpr_dt, _ = roc_curve(y_test, yscores_tree_m, pos_label=1)
        roc_auc_dt = auc(fpr_dt, tpr_dt)  # compute the dt auc value to compare in dynamic mode

        # Compute rf ROC cuve
        fpr_rf, tpr_rf, _ = roc_curve(y_test, yscores_rf_m, pos_label=1)
        roc_auc_rf = auc(fpr_rf, tpr_rf)  # compute the rf auc value to compare in dynamic mode

        # Plot ROC Curves
        fig, ax = plt.subplots(figsize=(8, 6))
        ax.plot(fpr_dt, tpr_dt, 'b', label=f'Decision Tree AUC = {roc_auc_dt:.4f}')
        ax.plot(fpr_rf, tpr_rf, 'g', label=f'Random Forest AUC = {roc_auc_rf:.4f}')
        ax.plot([0, 1], [0, 1], 'k--', alpha=0.7, label='Random Guess')  # add a random guess line for comparison purpose
        ax.set_xlim([0, 1])
        ax.set_ylim([0, 1])
        ax.set_xlabel('False Positive Rate')
        ax.set_ylabel('True Positive Rate')
        ax.set_title('ROC Curve Comparison')
        ax.legend(loc='lower right')
        fig.tight_layout()

        return fig
    
    ## CONFUSION MATRICIES ##
    @output
    @render.plot
    def confusion_matrix_combined():
        # Fit the dt model
        dt = DecisionTreeClassifier(ccp_alpha=input.alpha_confm(), random_state=rng)
        dt.fit(X_train, y_train)
        ypred_dt = dt.predict(X_test)               # make prediction on test data using dt model
        cm_dt = confusion_matrix(y_test, ypred_dt)  # compute dt confusion matrix
        
        # Fit the rf model
        rf = RandomForestClassifier(n_estimators=input.num_trees_confm(),
                                    max_features=input.num_features_confm(),
                                    bootstrap=True,
                                    oob_score=True,
                                    random_state=rng)
        rf.fit(X_train, y_train)
        yprobs_rf = rf.predict_proba(X_test)[:, 1]                     # extraxt probabilities of malignant
        ypred_rf = (yprobs_rf >= input.threshold_confm()).astype(int)  # makes the confucion matrix dynamic through the threshhold value
        cm_rf = confusion_matrix(y_test, ypred_rf)                     # compute rf confusion matrix

        # Plot Confusion Matricies
        labels = ["Benign (0)", "Malignant (1)"]  # add Benign and Maliganant labels for negative and positive classes for interpretabilty
        fig, axes = plt.subplots(1, 2, figsize=(12, 6))
        
        sns.heatmap(cm_dt.T, annot=True, fmt='d', cmap='Blues', ax=axes[0], cbar=False, xticklabels=labels, yticklabels=labels)  # dt confusion matrix
        axes[0].set_title("Decision Tree Confusion Matrix")
        axes[0].set_xlabel("True")
        axes[0].set_ylabel("Predicted")
        axes[0].xaxis.set_label_position('top')
        axes[0].xaxis.tick_top()
        
        sns.heatmap(cm_rf.T, annot=True, fmt='d', cmap='Greens', ax=axes[1], cbar=False, xticklabels=labels, yticklabels=labels)  # rf confusion matrix
        axes[1].set_title("Random Forest Confusion Matrix")
        axes[1].set_xlabel("True")
        axes[1].set_ylabel("Predicted")
        axes[1].xaxis.set_label_position('top')
        axes[1].xaxis.tick_top()
        plt.tight_layout()

        return fig
    
    ## ACCURACIES ##
    @output
    @render.text
    def confusion_matrix_accuracy():
        # Fit dt model
        dt = DecisionTreeClassifier(ccp_alpha=input.alpha_confm(), random_state=rng)
        dt.fit(X_train, y_train)
        ypred_dt = dt.predict(X_test)
        dt_acc = accuracy_score(y_test, ypred_dt)  # compute dt model accuracy
        
        # Fit rf model
        rf = RandomForestClassifier(n_estimators=input.num_trees_confm(),
                                    max_features=input.num_features_confm(),
                                    random_state=rng)
        rf.fit(X_train, y_train)
        yprobs_rf = rf.predict_proba(X_test)[:, 1]
        ypred_rf = (yprobs_rf >= input.threshold_confm()).astype(int)
        rf_acc = accuracy_score(y_test, ypred_rf)  # compute rf model accuracy
        
        return f"Decision Tree Accuracy: {dt_acc:.4f} | Random Forest Accuracy: {rf_acc:.4f}"


###############
#### SHINY ####
###############
app = App(app_ui, server)
