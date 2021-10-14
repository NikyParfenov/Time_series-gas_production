import numpy as np
import pandas as pd

# The final approximation function is here
from Approximation_func import time_series_approximation

import warnings
warnings.filterwarnings("ignore")


# Name of dataset columns
date_col = 'Дата'
target_col = 'Добыча'
field_col = 'Месторождение'
days_col = 'Дни'
additional_col = 'Собственные нужды'

# error interval precision and prediction period
alpha = 0.05
prediction = 365 * 3

# Check if trend list file exists else make it empty Data Frame (! it's needed for script)
try:
    own_trend_list = pd.read_excel('./Trends_list.xlsx', header=0)
except FileNotFoundError:
    own_trend_list = pd.DataFrame(columns=[date_col, target_col, field_col])

# Read the data with production
data = pd.read_excel('./Production_UGSS.xlsx', header=0, sheet_name=None)
df = pd.concat([data[key] for key in data.keys()], axis=0, ignore_index=True)
df[days_col] = (df[date_col] - df[date_col].iloc[0]).dt.days

# Fields list
fields = df['Месторождение'].unique()

# EXCEPTIONS block for approximation problem fields
exceptions = {
    # Improve the history matching by manually adding change points
    'change_points': {
        'Заполярное': [2735, 3450, 3877],
        'Ямсовейское': [3200],
        'Вынгапуровское': [1900],
        'Еты-Пуровское': [2480],
        'Комсомольское': [2500],
        'ЗАО Пургаз (СП)': [2100],
        'Южно-Русское НГКМ': [3660],
        'Одесское': [2640, 2870],
        'Западно-Салымское': [3775],
        'Бейсугское ГМ': [2130],
        'Платформа ПА-А': [1300, 2700],
        'Ямбургское': [2000, 2760],
        'Медвежье (сеном)': [3540],
        'Вынгаяхинское': [2490],
        'Мирное': [3410],
        'УКПГ-22 (Ачимовка)': [1780, 3850],
        'УКПГ-31 (Ачимгаз)': [1050],
        "ОАО МегионНГ": [3750],
        "ОАО МегионНГГеология": [2785],
        "Патроновское ГКМ": [3200, 3790],
        "Зап.-Красноармейское ГМ": [3845],
        "Казанское": [570],
        "ОАО НГК Славнефть (Ачим.)": [2900],
        "Граничное ГКМ": [2530],

        # Correction of not fitted fields
        'Харасавэйское': [3820],
        'Урмано-Арчинская гр.': [3800],
        'Гречаное ГМ': [2300, 3945],
        'Ю.Черноерковск.НГКМ': [3545],
        "Чаяндинское": [4010],
        "Тазовское (МНГ)": [4159],
        "Рыбальное": [3445, 4160],
        "Фонтановское": [3950],
        "Киринское ГКМ": [1830],
        "Северо-Булганакское": [3290],
    },
    # Use simple approximation for series with lack of data
    'simple_approximation': {
        "Тазовское (МНГ)": np.median,
        "Рыбальное": np.median,
        "Ен.-Яхинское (ГПН-Зап)": np.median,
        "УКПГ-51 (АчимДевелопмент)": np.median,
        "Чаяндинское": np.median,
        "УКПГ-НТС": np.median,
        "Фонтановское": np.median,
        "Киринское ГКМ": np.mean,
        "ГПД Шельф": np.median,
        "Зап.-Красноармейское ГМ": np.median,
    },
    # Own trend file with analytical production forecast
    'own_trend': own_trend_list,
}


def run_calculation(df: pd.DataFrame,
                    fields_list: list,
                    file_name: str,
                    date_col: str,
                    field_col: str,
                    target_col: str):
    """
    Function for calculations with required parameters

    :param df:          Initial Data Frame
    :param fields_list: List of fields under calculation
    :param file_name:   File name for result saving
    :param date_col:    Date column
    :param field_col:   Group (field) column
    :param target_col:  Target column

    :return:            Data Frame with results and write it to file
    """
    # Collect approximations in 'result' Data Frame
    result = pd.DataFrame()
    for field in fields_list:
        _result = time_series_approximation(df, field, fourier_order=8, noise_alpha=alpha, noise_window=5,
                                            picture=False, prediction=prediction, exceptions=exceptions)
        _result[field_col] = field
        result = pd.concat((result, _result), axis=0)

    full_result = pd.merge(result, df[[date_col, field_col, target_col]], how='left', on=[field_col, date_col])
    full_result['Residuals'] = full_result['Approximation'] - full_result[target_col]
    full_result['Series_Var'] = full_result['Series_STD'] ** 2

    # Save approximation result
    full_result.to_csv(file_name)

    return full_result


# Run calculations for production and consumption data
run_calculation(df, fields, "approximation_result.csv", date_col, field_col, target_col)
run_calculation(df, fields, "consumption_result.csv", date_col, field_col, additional_col)
