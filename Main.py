import datetime

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
company_col = 'Предприятие'
additional_col = 'Собственные нужды'

# Check if trend list file exists (file with trends is not necessary)
try:
    own_trend_list = pd.read_excel('./Trends_list.xlsx', header=0)
except FileNotFoundError:
    own_trend_list = None
    print('There is no trend files')

# Read the data with production
data = pd.read_excel('./Production_UGSS.xlsx', header=0, sheet_name=None)
df = pd.concat([data[key] for key in data.keys()], axis=0, ignore_index=True)

# Calculating of grouped data for dashboard
# Summary data
summary_data = df.groupby(date_col).agg(np.sum).reset_index()
summary_data[company_col] = 'ПАО Газпром'
summary_data[field_col] = 'Суммарная добыча'

# Gazprom production
gazprom_data = df.loc[~np.isin(df[field_col],
                               ['Независимые производители', 'Нефтяные компании',
                                'АО Арктикгаз (ГП 50%)', 'ЗАО Нортгаз (ГП 50%)',
                                'ОАО НГК Славнефть (ГП 50%)', 'ЗАО УралНГП (ГП 37.7%)',
                                'ОАО Томскнефть (ГП 50%)']), :].groupby(date_col).agg(np.sum).reset_index()
gazprom_data[company_col] = 'ПАО Газпром'
gazprom_data[field_col] = 'ПАО Газпром'

# Gazprom + JV part production
gazprom_jv_data_temp = df.loc[~np.isin(df[field_col], ['Независимые производители', 'Нефтяные компании']), :]
gazprom_jv_data_temp.loc[np.isin(gazprom_jv_data_temp[field_col],
                                 ['АО Арктикгаз (ГП 50%)', 'ЗАО Нортгаз (ГП 50%)',
                                  'ОАО НГК Славнефть (ГП 50%)', 'ОАО Томскнефть (ГП 50%)']),
                         [target_col, additional_col]] *= 0.5
gazprom_jv_data_temp.loc[gazprom_jv_data_temp[field_col] == 'ЗАО УралНГП (ГП 37.7%)',
                         [target_col, additional_col]] *= 0.377
gazprom_jv_data = gazprom_jv_data_temp.groupby(date_col).agg(np.sum).reset_index()
gazprom_jv_data[company_col] = 'ПАО Газпром'
gazprom_jv_data[field_col] = 'ПАО Газпром + СП'

# JV total production
jv_data = df.loc[np.isin(df[field_col],
                         ['АО Арктикгаз (ГП 50%)', 'ЗАО Нортгаз (ГП 50%)',
                          'ОАО НГК Славнефть (ГП 50%)', 'ЗАО УралНГП (ГП 37.7%)',
                          'ОАО Томскнефть (ГП 50%)']), :].groupby(date_col).agg(np.sum).reset_index()
jv_data[company_col] = 'Совм. предпр.'
jv_data[field_col] = 'Совм. предпр.'

# Independent companies and sources
independent_data = df.loc[np.isin(df[field_col],
                                  ['Независимые производители', 'Нефтяные компании']), :]\
    .groupby(date_col).agg(np.sum).reset_index()
independent_data[company_col] = 'Внешние источники'
independent_data[field_col] = 'НП + НК'

# Independent companies + JV part production
independent_jv_data = df.loc[np.isin(df[field_col],
                                     ['Независимые производители', 'Нефтяные компании',
                                      'АО Арктикгаз (ГП 50%)', 'ЗАО Нортгаз (ГП 50%)', 'ЗАО УралНГП (ГП 37.7%)',
                                      'ОАО НГК Славнефть (ГП 50%)', 'ОАО Томскнефть (ГП 50%)']), :]
independent_jv_data.loc[np.isin(independent_jv_data[field_col],
                                ['АО Арктикгаз (ГП 50%)', 'ЗАО Нортгаз (ГП 50%)',
                                'ОАО НГК Славнефть (ГП 50%)', 'ОАО Томскнефть (ГП 50%)']),
                        [target_col, additional_col]] *= 0.5
independent_jv_data.loc[independent_jv_data[field_col] == 'ЗАО УралНГП (ГП 37.7%)',
                        [target_col, additional_col]] *= (1 - 0.377)
independent_jv_data = independent_jv_data.groupby(date_col).agg(np.sum).reset_index()
independent_jv_data[company_col] = 'Внешние источники'
independent_jv_data[field_col] = 'НП + НК + СП'

# Collect all the data into one data frame
combined_df = pd.concat([summary_data, gazprom_data, gazprom_jv_data, jv_data,
                         independent_data, independent_jv_data, df], axis=0)

# Fields list
fields = combined_df[field_col].unique()

# EXCEPTIONS block for approximation problem fields
exceptions_production = {
    'change_points': {# Improve the history matching by adding new change points
                        'Заполярное': [2735],
                        'Ямсовейское': [3200],
                        'Губкинское (сеном)': [2100],
                        "АО Арктикгаз (ГП 50%)": [2000],
                        'Вынгапуровское': [1900],
                        'Еты-Пуровское': [2480],
                        'Комсомольское': [2470, 3500],
                        'Южно-Русское НГКМ': [3660],
                        'Бейсугское ГМ': [2130],
                        'Ямбургское': [2000, 2760],
                        'Медвежье (сеном)': [3540],
                        'Вынгаяхинское': [2490],
                        'Мирное': [3410],
                        'УКПГ-НТС': [2000, 2850],
                        'УКПГ-22 (Ачимовка)': [1780, 3850],
                        'УКПГ-31 (Ачимгаз)': [1050],
                        "Патроновское ГКМ": [3200, 3790],
                        "Зап.-Красноармейское ГМ": [3845],
                        "Казанское": [570],
                        "Граничное ГКМ": [2530],
                        "ОАО Томскнефть (ГП 50%)": [3500],
                        "Нефтяные компании": [200],
                        "Независимые производители": [550, 2750],
                        "ЗАО ПечораНГП": [1850, 2350],
                        "Ен.-Яхинское (В.)": [2350],
                        "Сев.-Прибрежное НГКМ": [2220],
                        "НП + НК": [550, 2450, 3500],
                        "НП + НК + СП": [550, 2450, 3500],
                        "Совм. предпр.": [2100],
                        "ПАО Газпром": [2500],
                        "ПАО Газпром + СП": [2500],

                        # Correction of not fitted fields
                        'Харасавэйское': [3820],
                        'Гречаное ГМ': [2300, 3945],
                        'Ю.Черноерковск.НГКМ': [3545],
                        "Рыбальное": [3445, 4160],
                        "Киринское ГКМ": [1830],
                        "Северо-Булганакское": [3290],
                       },
    'simple_approximation': {# Use simple approximation for series with lack of data
                           "Рыбальное": np.median,
                           "Ен.-Яхинское (ГПН-Зап)": np.median,
                           "УКПГ-51 (АчимДевелопмент)": np.median,
                           "Пинджинское": np.median,
                           "Зап.-Красноармейское ГМ": np.median,
                          },
     }

# # Still testing!
# def normalize_on_trend(field, field_result, own_trend_list):
#     """
#     Function normalizes predicted profile on analytical trend
#     :return:
#     """
#     field_result['Combine_prod'] = field_result[target_col].fillna(df['Approximation'])
#     field_result['Year'] = field_result[date_col].dt.year
#
#     trend_df = own_trend_list.loc[own_trend_list[field_col] == field, :]
#     trend_df[date_col] = (trend_df[date_col] - 1970).astype('datetime64[Y]')
#     trend_df_downscaled = pd.DataFrame({date_col: trend_df[date_col] + datetime.timedelta(days=182),
#                                         'ЦКР': trend_df[target_col] / 365 * 1e6})
#     trend_df_downscaled['Year'] = trend_df_downscaled[date_col].dt.year
#     trend_df_downscaled = pd.merge(trend_df_downscaled, field_result.groupby('Year')[['Combine_prod']].agg(np.mean),
#                                    how='left', on='Year')
#     trend_df_downscaled['relation'] = trend_df_downscaled['ЦКР'] / trend_df_downscaled['Combined_prod']
#     trend_df_downscaled['relation'].ffill(inplace=True)
#     start = trend_df_downscaled['Дата'].dt.year.min()
#     end = trend_df_downscaled['Дата'].dt.year.max()
#     trend_df_downscaled = pd.merge(pd.DataFrame(data=pd.date_range(f'01-01-{start}', f'31-12-{end}', freq='D').date,
#                                                 columns=['Дата'],
#                                                 dtype='datetime64[ns]'),
#                                    trend_df_downscaled,
#                                    how='left', on='Дата')
#     trend_df_downscaled['Дни'] = (trend_df_downscaled['Дата'] - field_result['Дата'].iloc[0]).dt.days
#     trend_df_downscaled['relation'] = trend_df_downscaled['relation'].interpolate(interpolate='polynomial', order=5)
#     field_result = pd.merge(field_result, trend_df_downscaled[['Дата', 'relation']], how='left', on='Дата')
#
#     return field_result['relation'].fillna(1.)


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
        _result = time_series_approximation(df, field, fourier_order=8, approx_alpha=0.001, noise_window=5,
                                            picture=False, prediction=365*3,
                                            exceptions=exceptions_production)
        _result[field_col] = field
        _result[company_col] = df.loc[df[field_col] == field, company_col].unique()[0]

        # # Take into account trend if acceptable
        # if not relation:
        #     if np.isin(field, own_trend_list[field_col]):
        #         _result['relation'] = normalize_on_trend(field, _result, own_trend_list)
        #     else:
        #         _result['relation'] = pd.Series([1. for _ in range(_result.shape[0])], name='relation')
        # else:
        #     _result['relation'] = relation.loc[df[field_col] == field, 'relation']
        #
        # _result['Combine_prod'] = _result[target_col].fillna(_result['Approximation'] * _result['relation'])
        # result = pd.concat((result, _result), axis=0)

    full_result = pd.merge(result, df[[date_col, field_col, target_col]], how='left', on=[field_col, date_col])
    full_result['Residuals'] = full_result['Approximation'] - full_result[target_col]
    full_result['Series_Var'] = full_result['Series_STD'] ** 2

    # Save approximation result
    full_result.to_csv(file_name)

    # if not relation:
    #     return full_result[[date_col, field_col, 'relation']]
    return full_result


# Run calculations for production and consumption data
run_calculation(combined_df, fields, "approximation_result.csv", date_col, field_col, target_col)
run_calculation(combined_df, fields, "consumption_result.csv", date_col, field_col, additional_col)
