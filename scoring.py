from sklearn.metrics import precision_score, accuracy_score, recall_score, f1_score, roc_auc_score
import pandas as pd


def print_scores(y_train, y_train_predict, y_test, y_test_predict):
    """
    Print precision, recall, accuracy and f1 scores.
    """
    train_precision = precision_score(y_train, y_train_predict)
    train_recall = recall_score(y_train, y_train_predict)
    train_accuracy = accuracy_score(y_train, y_train_predict)
    train_f1 = f1_score(y_train, y_train_predict)

    test_precision = precision_score(y_test, y_test_predict)
    test_recall = recall_score(y_test, y_test_predict)
    test_accuracy = accuracy_score(y_test, y_test_predict)
    test_f1 = f1_score(y_test, y_test_predict)

    df_result = pd.DataFrame(data={"Precision": [train_precision, test_precision],
                                   "Recall": [train_recall, test_recall],
                                   "Accuracy": [train_accuracy, train_f1],
                                   "F1-Score": [train_f1, test_f1]},
                             index=["Training Set", "Testing Set"],
                             columns=["Precision", "Recall", "Accuracy", "F1-Score"]
                             )
    return df_result
