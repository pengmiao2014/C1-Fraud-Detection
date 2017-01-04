import pickle
import pandas as pd

from sklearn.ensemble import AdaBoostClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsClassifier

from sklearn.svm import SVC
from sklearn.grid_search import GridSearchCV
from sklearn.cross_validation import train_test_split

from scoring import print_scores
#from plotting import standard_confusion_matrix,profit_curve,run_profit_curve,plot_profit_model

cleaned_data_csv = 'cleaned_training_data.csv'

target = 'churn'


def load_cleaned_data():
    df = pd.read_csv(cleaned_data_csv)
    X = df.drop(target, axis=1).values
    y = df[target].values

    return df, X, y


def generate_models_and_grids():
    clf_LRC = LogisticRegression()
    clf_KNC = KNeighborsClassifier()
    clf_RFC = RandomForestClassifier()
    clf_ABC = AdaBoostClassifier()
    clf_GBC = GradientBoostingClassifier()
    clf_SVC = SVC()

    grid_LRC = {
        'penalty': ['l1'],
        'class_weight': ['balanced'],
        'C': [0.1],
    }

    grid_KNC = {
        'n_neighbors': [5, 10],
        'weights': ['uniform', 'distance'],
        'p': [1, 2]
    }

    grid_RFC = {
        'criterion': ['gini', 'entropy'],
        'max_features': [None, 'auto', 2, 4, 8],
        'max_depth': [None, 2, 4, 8],
        'n_estimators': [50, 100, 200],
        'class_weight': [None, 'balanced']
    }

    grid_ABC = {
        'n_estimators': [100, 200, 400],
        'learning_rate': [0.1, 0.5, 1, 5],
    }

    grid_GBC = {
        'loss': ['deviance', 'exponential'],
        'n_estimators': [100, 200, 400],
        'max_features': [2, 4, 8],
        'max_depth': [2, 4, 8],
    }

    grid_SVC = [
        {
            'kernel': ['rbf'],
            'gamma': [1e-2, 1e-3, 'auto'],
            'C': [0.1, 1, 10]
        },
        {
            'kernel': ['linear'],
            'C': [0.1, 1, 10]
        },
        {
            'kernel': ['poly'],
            'degree': [2, 3],
            'C': [0.1, 1, 10]
        }
    ]

    # models = [clf_LRC, clf_KNC, clf_RFC, clf_ABC, clf_GBC, clf_SVC]
    # grids = [grid_LRC, grid_KNC, grid_RFC, grid_ABC, grid_GBC, grid_SVC]
    models = [clf_LRC]
    grids = [grid_LRC]


    return models, grids


def find_best_estimators(X_train, y_train, X_test, y_test, models, grids, scoring='f1'):
    result_models = []
    for clf, grid in zip(models, grids):
        grid_obj = GridSearchCV(clf, param_grid=grid, scoring='f1', n_jobs=-1)
        grid_obj.fit(X_train, y_train)
        result_models.append(grid_obj.best_estimator_)

    return result_models


if __name__ == '__main__':

    # load cleaned data
    df, X, y = load_cleaned_data()

    # train, test split
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.04, random_state=42)

    # generate models and parameter grids
    models, grids = generate_models_and_grids()

    # tune parameters for each model
    best_models = find_best_estimators(X_train, y_train, X_test, y_test, models, grids, scoring='f1')

    # inspect each of the best models
    for model in best_models:
        # print out model details
        print model

        #plot_profit_model(model, costbenefit, X_train, X_test, y_train, y_test)

        # print out train and test scores
        y_train_predict = model.predict(X_train)
        y_test_predict = model.predict(X_test)
        print print_scores(y_train, y_train_predict, y_test, y_test_predict)

        # save model to files
        filename = '{}.pkl'.format(model.__class__.__name__)
        output_model = pickle.dumps(model)
        with open(filename, 'w') as handle:
            handle.write(output_model)