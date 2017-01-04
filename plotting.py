import numpy as np
import sklearn.learning_curve as curves
from sklearn.cross_validation import ShuffleSplit
from sklearn.metrics import precision_score, accuracy_score, recall_score, f1_score, roc_auc_score
from sklearn.metrics import precision_recall_curve, roc_curve
from sklearn.metrics import confusion_matrix
from sklearn.linear_model import LogisticRegression
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.cross_validation import train_test_split

costbenefit = np.array([[20, -20], [0, 0]])

def standard_confusion_matrix(y_true, y_predict):
    [[tn, fp], [fn, tp]] = confusion_matrix(y_true, y_predict)
    return np.array([[tp, fp], [fn, tn]])


def profit_curve(cost_benefit_matrix, probabilities, y_true):
    thresholds = sorted(probabilities, reverse=True)
    profits = []
    for threshold in thresholds:
        y_predict = probabilities > threshold
        confusion_mat = standard_confusion_matrix(y_true, y_predict)
        profit = np.sum(confusion_mat * cost_benefit_matrix) / float(len(y_true))
        profits.append(profit)
    return thresholds, profits


def run_profit_curve(model, costbenefit, X_train, X_test, y_train, y_test):
    probabilities = model.predict_proba(X_test)[:, 1]
    thresholds, profits = profit_curve(costbenefit, probabilities, y_test)
    return thresholds, profits


def plot_profit_model(model, costbenefit, X_train, X_test, y_train, y_test):
    percentages = np.linspace(0, 100, len(y_test))
    thresholds, profits = run_profit_curve(model, costbenefit, X_train, X_test, y_train, y_test)
    plt.plot(percentages, profits, label=model.__class__.__name__)
    plt.title("Profit Curve")
    plt.xlabel("Percentage of test instances (decreasing by score)")
    plt.ylabel("Profit")
    plt.legend(loc='best')
    plt.savefig('profit_curve.png')


def find_best_threshold(model, costbenefit, X_train, X_test, y_train, y_test):
    max_threshold = None
    max_profit = None

    thresholds, profits = run_profit_curve(model, costbenefit, X_train, X_test, y_train, y_test)
    max_index = np.argmax(profits)
    if profits[max_index] > max_profit:
        max_threshold = thresholds[max_index]
        max_profit = profits[max_index]
    return max_threshold, max_profit

cleaned_data_csv = 'cleaned_training_data.csv'

target = 'churn'


def load_cleaned_data():
    df = pd.read_csv(cleaned_data_csv)
    X = df.drop(target, axis=1).values
    y = df[target].values

    return df, X, y

if __name__ == '__main__':

    # load cleaned data
    df, X, y = load_cleaned_data()

    # train, test split
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.04, random_state=42)

    lr = LogisticRegression(C=0.1, fit_intercept=True)
    lr.fit(X_train, y_train)

    plot_profit_model(lr, costbenefit, X_train, X_test, y_train, y_test)

