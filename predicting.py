import pickle
import pandas as pd

new_data_csv = 'cleaned_test_data.csv'
target = 'fraud_ind'


def load_cleaned_data():
    df = pd.read_csv(cleaned_data_csv)
    X = df.drop(target, axis=1).values
    y = df[target].values

    return df, X, y


if __name__ == '__main__':

    # load cleaned data
    df, X, y = load_cleaned_data()

    # load pre-trained model
    filename = 'LogisticRegression.pkl'
    with open(filename, 'r') as handle:
        input_model = handle.read()
    model = pickle.loads(input_model)

    # make prediction for each data point
    for i, data_point in enumerate(X):
        print "For the {} th new rider,".format(i), \
            "Predicted value:", model.predict(data_point.reshape(1, X.shape[1]))[0], ",", \
            "Actual value", y[i]
