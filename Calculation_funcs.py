import numpy as np
import pandas as pd

from Supporting_inform_funcs import change_point_graph
from Basic_funcs import func_lin, func_log, func_hyp, func_general_fourier, func_general_fourier_k
from scipy.stats import t
from scipy.ndimage import uniform_filter1d, gaussian_filter1d
from scipy.optimize import curve_fit
from sklearn.metrics import r2_score
import matplotlib.pyplot as plt


# Main calculation functions

def own_trend_converter(df: pd.DataFrame,
                        date_col: str = 'Дата',
                        field_col: str = 'Месторождение',
                        days_col: str = 'Дни') -> pd.DataFrame:
    """
    Function converts yearly trend data (analytical production forecast) to the daily one with linear approximation
    between values (function used in case of trend exceptions!)

    :param df:        Data Frame with 'int' year "YYYY" (or middle of year "YYYY-07-DD"), trend values and field name
    :param date_col:  Column name with date
    :param field_col: Column name for group (field)
    :param days_col:  Column name with days

    :return:          Data Frame with own trend data prepared for trend_chooser function
    """

    # Check for date column input
    if df[date_col].dtype != 'datetime64[ns]':
        # Converting yearly 'int' data to datetime
        df[date_col] = (df[date_col] - 1970).astype('datetime64[Y]')

    start_year = df[date_col].dt.year.iloc[0]
    end_year = df[date_col].dt.year.iloc[-1]

    # Creating table with converted dates
    own_trends = pd.DataFrame(data=pd.date_range(f'01-01-{start_year}',
                                                 f'31-12-{end_year}',
                                                 freq='D').date,
                              columns=[date_col], dtype='datetime64[ns]')

    own_trends = pd.merge(own_trends, df, how='left', on=date_col)
    own_trends = own_trends.interpolate(method='ffill', limit_direction='forward')
    own_trends[field_col].ffill(inplace=True)
    own_trends[days_col] = (own_trends[date_col] - own_trends[date_col].iloc[0]).dt.days

    return own_trends


def trend_chooser(x_input: np.ndarray,
                  y_input: np.ndarray,
                  x0: int = 0,
                  exclude_outs: bool = True,
                  return_y: bool = False) -> (np.ndarray, callable, np.ndarray, np.ndarray):
    """
    Method select the trend of time series from: linear, hyperbolic, logarithmic.

    :param x_input:      Days data of time series
    :param y_input:      Values of time series
    :param x0:           Days bias
    :param exclude_outs: Correction for outliers (restricts 1% from high and low values)
    :param return_y:     Whether return y values with trend parameters or return only trend parameters

    :return:             Approximated values (if allowed), trend function, trend coefficients
    """

    # Outliers have a high impaction on trend, so, it's recommended to restrict y values
    if exclude_outs:
        df = pd.DataFrame({'x': x_input, 'y': y_input})
        df = df.drop(df.loc[(df['y'] < df['y'].median() * 0.1) |
                            (df['y'] > df['y'].quantile(0.99)), :].index)
        x = df['x'].values
        y = df['y'].values
    else:
        x = x_input.copy()
        y = y_input.copy()

    popt_lin, pcov_lin = curve_fit(func_lin, x[x0:] - x0, y[x0:], (y[0], 1), maxfev=10 ** 6)
    popt_log, pcov_log = curve_fit(func_log, x[x0:] - x0, y[x0:], (y[0], 1, 1), maxfev=10 ** 6)
    popt_hyp, pcov_hyp = curve_fit(func_hyp, x[x0:] - x0, y[x0:], (y[0], 1, 1), maxfev=10 ** 6)

    r2_lin = r2_score(y[x0:], func_lin(x[x0:] - x0, *popt_lin))
    r2_log = r2_score(y[x0:], func_log(x[x0:] - x0, *popt_log))
    r2_hyp = r2_score(y[x0:], func_hyp(x[x0:] - x0, *popt_hyp))

    r2_max = np.max([r2_lin, r2_log, r2_hyp])

    # Approximate values are needed in some cases, but not always
    if r2_hyp == r2_max:
        return (func_hyp(x_input - x0, *popt_hyp), func_hyp, popt_hyp) if return_y else (func_hyp, popt_hyp)
    elif r2_log == r2_max:
        return (func_log(x_input - x0, *popt_log), func_log, popt_log) if return_y else (func_log, popt_log)
    else:
        return (func_lin(x_input - x0, *popt_lin), func_lin, popt_lin) if return_y else (func_lin, popt_lin)


def approx_ts_by_fourier(x_input: np.ndarray,
                         x0: int,
                         y_input: np.ndarray,
                         trend_func: callable,
                         fourier_order: int,
                         fix_trend: bool,
                         *trend_popt: list,
                         __k: int = 0) -> (np.ndarray, dict):
    """
    Recursive Fourier approximation (k-order) of time series periodic part

    :param x_input:       Days data of time series
    :param x0:            Days bias
    :param y_input:       Values of time series
    :param trend_func:    Function of trend type (lin, log, hyp)
    :param fourier_order: Fourier order of periodical part approximation
    :param fix_trend:     Fix trend with trend_chooser parameters or correct it with 1st Fourier order fitting
    :param trend_popt:    Coefficients of trend function approximation

    :param __k:           Private! Counter of Fourier order, don't change it. It needs for transmitting current Fourier
                          order to the next recursion stage, until restriction (fourier_order) hasn't been reached

    :return:              Approximated values and dictionary with all approximation coefficients
    """

    # Stop recursion when fourier_order is reached
    if __k == fourier_order:
        return trend_func(x_input - x0, *trend_popt), {'Trend_params': trend_popt, 'Trend_func': trend_func}
    else:
        __k += 1

        # if correction of trend coefficients is allowed then correct it with 1st Fourier order
        if __k == 1 and not fix_trend:
            popt, _ = curve_fit(lambda x, a, theta, *args:
                                func_general_fourier(x, a, theta, trend_func, *args),
                                x_input[x0:] - x0,
                                y_input[x0:], (1, -0.1, *trend_popt), maxfev=10 ** 6)
            popt, trend_popt = popt[:2], popt[2:]
            new_y = y_input - func_general_fourier(x_input - x0, *popt, trend_func, *trend_popt)

        # General Fourier fitting for high approximation order or fixed trend
        else:
            popt, _ = curve_fit(lambda x, a, theta:
                                func_general_fourier_k(x, a, theta, __k, trend_func, *trend_popt),
                                x_input[x0:] - x0, y_input[x0:], (1, -0.1), maxfev=10 ** 6)

            new_y = y_input - func_general_fourier_k(x_input - x0, *popt, __k, trend_func, *trend_popt)

        # return approximation and dictionary with approximation coefficients
        params = {f'Fourier-{__k}': popt}
        _approx, _params = approx_ts_by_fourier(x_input, x0, new_y, trend_func, fourier_order, fix_trend, *trend_popt,
                                                __k=__k)

        params.update(_params)
        approx = func_general_fourier_k(x_input - x0, *popt, __k, trend_func, *trend_popt) + _approx

        return approx, params


def change_point_finder(x_input: np.ndarray,
                        y_input: np.ndarray,
                        field: str,
                        picture: bool = True,
                        padding: int = 584,
                        threshold: float = 0.965,
                        exceptions: dict = {}) -> np.ndarray:
    """
    Function of change points search in time series. Realization relies on sum minimization of squared errors in
    splitted intervals: U(t) = n_i * Var(:t) + n_j * Var(t:) -> Min. Calculation conducts on both initial series
    and series with excluded trend, then the minimum Var is taking into account.

    :param x_input:    Days data of time series
    :param y_input:    Values of time series
    :param field:      Name of field
    :param picture:    Option to plot graphs with approximation
    :param padding:    Minimum interval of series separation
    :param threshold:  Cut-off for further series splitting (value U(t).min/U(t).mean and U(t).min/U(t).median)

    :param exceptions: Some additional change points can be force added. Has the next structure for change_points:
                       exceptions = {'change_points': {field: [...], ...}, ...}

    :return:           Array of found change points within the series
    """

    x0 = (y_input != 0).argmax(axis=0)
    y_trend, trend_func, trend_popt = trend_chooser(x_input, y_input, x0, return_y=True)
    y_trend[:x0] = 0
    y_trend[np.where(y_trend < 0)] = 0
    y_delta = y_input - y_trend

    # Data Frame with variance result
    var_data = pd.DataFrame(columns=['Point', 'Variance', 'Variance_trend'])

    # Calculation of squared error sum taking into account min size restriction - padding
    for i in range(x0 + padding, len(x_input) - padding, 1):
        current_var = np.var(y_input[x0:i]) * len(y_input[x0:i]) + \
                      np.var(y_input[i:]) * len(y_input[i:])
        current_var_trend = np.var(y_delta[x0:i]) * len(y_delta[x0:i]) + \
                            np.var(y_delta[i:]) * len(y_delta[i:])
        var_data.loc[len(var_data)] = [i, current_var, current_var_trend]

    # Choose the minimum variance between initial series and series with trend exclusion
    variance = var_data['Variance'] if var_data['Variance'].min() < var_data['Variance_trend'].min() else var_data[
        'Variance_trend']

    var_min = variance.min()
    var_mean = variance.mean()
    var_median = variance.median()

    # Compare values U(t).min/U(t).mean and U(t).min/U(t) of possible change point (t) with threshold
    if (var_min / var_mean < threshold) & (var_min / var_median < threshold):
        change_point = int(var_data.loc[variance == var_min, 'Point'].values[0])
    else:
        change_point = None

    # Check if possible change point exists and not equals borders then concatenate it with others.
    if change_point and change_point != padding:
        if picture:
            change_point_graph(x_input, y_input, y_trend, change_point, var_data, threshold)

        # Recursion for change point [current point, func(left interval), func(right interval)]
        change_points_list = np.concatenate(
            (change_point + x_input[0],
             x_input[0] + change_point_finder(x_input[x0:change_point] - x_input[0],
                                              y_input[x0:change_point],
                                              field, picture, padding, threshold),
             change_point + change_point_finder(x_input[change_point:] - x_input[change_point],
                                                y_input[change_point:],
                                                field, picture, padding, threshold),
             ), axis=None)

        # EXCEPTIONS - additional change_points
        if ('change_points' in exceptions.keys()) and (field in exceptions['change_points'].keys()):
            change_points_list = np.sort(np.concatenate((change_points_list,
                                                         exceptions['change_points'][field],
                                                         ), axis=None))

        # Returns change points list, with excluded NaN
        return np.sort(change_points_list[np.where(~np.isnan(change_points_list))].astype(int))

    # EXCEPTIONS - additional change_points (return only exceptions if didn't find other change points)
    elif ('change_points' in exceptions.keys()) and (field in exceptions['change_points'].keys()):
        return np.array(exceptions['change_points'][field])

    else:
        # Returns none if there are no change points
        return np.array([np.nan])


def variation(result: pd.DataFrame,
              residuals: pd.DataFrame,
              alpha: float = 0.05,
              approx_variation: bool = False,
              monthly_variation: bool = True,
              var_param_top: int = 2,
              var_param_low: int = 3) -> pd.DataFrame:
    """
    Function added self series noise and approximation error variations according to distribution of residuals

    :param result:            Data Frame with month column and approximation values
    :param alpha:             Allowed error in statistical accuracy (default=0.05)
    :param residuals:         Residuals for confident interval estimation: assumed (Y_true - Y_approx) for bootstrap
                              and assumed (Y_true - Y_moving_average) for self noise
    :param approx_variation:  If "True" - use quantiles for approximation error estimation relying on the last series
                              interval [last_change_point: end], if "False" - use t-statistics for self noise on
                              whole series [x0:end].
    :param monthly_variation: Allows to split approximation errors into month groups (if the monthly data is enough)
    :param var_param_top:     Gaussian smoothing window for upper boundary of approximation error estimation
    :param var_param_low:     Moving average window for lower boundary of approximation error estimation

    :return:                  Input Data Frame with added statistics (t-statistics, STD of errors and bootstrap
                              percentiles) and random noise realization
    """

    # Bootstrap error variation block (quantiles of potentially deviated distributions)
    if approx_variation:
        calc_single_noise = False

        high_col_name = f'Approx_error_quantile_{1 - alpha / 2}'
        low_col_name = f'Approx_error_quantile_{alpha / 2}'

        # In the case of monthly lack of data, use whole series for error
        if (len(residuals.groupby('Month').quantile(alpha / 2)) < 12) or (not monthly_variation):
            result[high_col_name] = residuals['Residuals'].quantile(1 - alpha / 2)
            result[low_col_name] = residuals['Residuals'].quantile(alpha / 2)

        # Otherwise, use monthly distribution of approximation errors with some moving average (uniform_filter1d) for
        # bottom interval and more strong gaussian smoothing for top interval
        else:
            quantile_low = uniform_filter1d(residuals.groupby('Month')['Residuals'].quantile(alpha / 2),
                                            size=var_param_low, mode='wrap')
            quantile_high = gaussian_filter1d(residuals.groupby('Month')['Residuals'].quantile(1 - alpha / 2),
                                              sigma=var_param_top, mode='wrap')
            quantile_df = pd.DataFrame(data={'Month': [i+1 for i in range(12)],
                                             low_col_name: quantile_low.flatten(),
                                             high_col_name: quantile_high.flatten(),
                                             }
                                       )

            result = pd.merge(result, quantile_df, how='left')

    # Self series noise (t-statistic)
    else:
        calc_single_noise = True
        delta = t.ppf(1 - alpha / 2, df=len(residuals))
        result['Series_STD'] = residuals['Residuals'].values.std()
        result[f'Series_T-stat_{1-alpha}'] = delta

    # Calculation of random variation relying on self time series noise
    if calc_single_noise:
        # Try to restrict residuals relying on chosen alpha excluding outliers (for better noise visualization)
        try:
            restricted_residuals = residuals['Residuals'].values[
                np.where((residuals['Residuals'].values > np.quantile(residuals['Residuals'].values, alpha / 2)) &
                         (residuals['Residuals'].values < np.quantile(residuals['Residuals'].values, 1 - alpha / 2)))]
        except ValueError:
            restricted_residuals = residuals['Residuals'].values

        hist, bin_edges = np.histogram(restricted_residuals, bins=100, density=True)

        # In some cases (lack of data) noise can't be estimated, so try/except constructions were included
        try:
            noise = np.array([np.random.choice((bin_edges[1:] - np.diff(bin_edges) / 2), p=(hist * np.diff(bin_edges)))
                              for _ in range(len(result))])
        except ValueError:
            noise = np.array([0 for _ in range(len(result))])

        result['Single_Noise'] = noise

    return result


def build_final_approx(x_input: np.ndarray,
                       x0: int,
                       y_input: np.ndarray,
                       field: str,
                       date_col: str = 'Дата',
                       field_col: str = 'Месторождение',
                       target_col: str = 'Добыча',
                       days_col: str = 'Дни',
                       fourier_order: int = 5,
                       fix_trend: bool = False,
                       change_points: np.ndarray = np.nan,
                       prediction: int = 0,
                       exceptions: dict = {}) -> (np.ndarray, np.ndarray, np.ndarray, np.ndarray):
    """
    Function puts together time series trend and Fourier approximation taking into account change_points

    :param x_input:       Days data of time series
    :param x0:            Days bias
    :param y_input:       Values of time series
    :param field:         Name of current field

    :param date_col:      Column name with date
    :param field_col:     Column name for group (field)
    :param target_col:    Column with targets
    :param days_col:      Column name with days

    :param fourier_order: Fourier order of periodical part approximation
    :param fix_trend:     Fix trend with trend_chooser parameters or correct it with 1st Fourier order fitting
    :param change_points: Array of change points for approximation separation (there are no change points if NaN)
    :param prediction:    Extrapolation period in days (prediction)

    :param exceptions:    Can simplify the approximation by mean/median (if there is no enough data for complete
                          approximation) or fix trend by existing values. Has the next structure:
                          exceptions = {'simple_approximations': {field: np.mean/np.median, ...},
                                        'own_trend': {field: <data frame name with trend points>, ...},
                                       }

    :return:              Return values for trend and approximation, and values for prediction period (if available)
    """

    # EXCEPTIONS - if it has it's own trend use it forecast for trend approximation
    if ('own_trend' in exceptions.keys()) and \
            (len(exceptions['own_trend'].loc[exceptions['own_trend'][field_col] == field, :])):

        own_trend = own_trend_converter(exceptions['own_trend'].loc[
                                        exceptions['own_trend'][field_col] == field, :],
                                        date_col, field_col, days_col)

        trend_df = own_trend.loc[(own_trend[days_col] > x_input[-1]) &
                                 (own_trend[days_col] <= x_input[-1] + prediction),
                                 [target_col, days_col]]

        # Trend X and Y expanded to prediction period
        x_tr = np.append(x_input, trend_df[days_col].values)
        y_tr = np.append(y_input, trend_df[target_col].values)

    else:
        # If there is no trend analytical forecast
        x_tr = x_input
        y_tr = y_input

    # Check for change points existence
    if np.all(~np.isnan(change_points)):

        # Array of intervals creation. Approximation will be fitted for each one
        cp = np.sort(np.concatenate((change_points, [0, len(x_input)]), axis=None))

        # Trend intervals is longer to 'delta' and use for trend approximation at the last part of time series
        cp_tr = np.sort(np.concatenate((change_points, [0, len(x_tr)]), axis=None))

        # Arrays of results for trend and summary approximations
        y_trend = np.array([])
        y_approx = np.array([])

        # Calculate approximation for each interval [i:i+1] between change point
        for i, cpi in enumerate(cp[:-1]):
            x0 = (y_input[cpi:cp[i + 1]] != 0).argmax(axis=0)

            # EXCEPTIONS - if the data amount is low then set approximation as constant (e.g. median)
            if ('simple_approximation' in exceptions.keys()) and \
               (field in exceptions['simple_approximation'].keys()) and \
               (i == len(cp) - 2):
                simple_approx = exceptions['simple_approximation'][field](
                    y_input[cpi:cp[i + 1]][np.where(y_input[cpi:cp[i + 1]] > 0)])

                _y_trend = np.array([simple_approx for _ in range(len(x_input[cpi:cp[i + 1]]))])
                _y_approx = np.array([simple_approx for _ in range(len(x_input[cpi:cp[i + 1]]))])

            else:
                _y_trend, trend_func, trend_popt = trend_chooser(x_tr[cpi:cp_tr[i + 1]],
                                                                 y_tr[cpi:cp_tr[i + 1]],
                                                                 x0, return_y=True)

                # EXCEPTIONS - fix last trend of time series according to it's own trend forecast
                if ('own_trend' in exceptions.keys()) and \
                   (len(exceptions['own_trend'].loc[exceptions['own_trend'][field_col] == field, :])) and \
                   (i == len(cp) - 2):
                    fix_trend = True
                    _y_trend = _y_trend[:len(_y_trend) - prediction]

                _y_approx, params = approx_ts_by_fourier(x_input[cpi:cp[i + 1]], x0,
                                                         y_input[cpi:cp[i + 1]], trend_func,
                                                         fourier_order, fix_trend, *trend_popt)

                # trend after correction in approx_ts_by_fourier function
                _y_trend = params['Trend_func'](x_input[cpi:cp[i + 1]] - x0, *params['Trend_params'])

            y_trend = np.concatenate((y_trend, _y_trend), axis=None)
            y_approx = np.concatenate((y_approx, _y_approx), axis=None)

    else:

        # EXCEPTIONS - if the data amount is low then set approximation as constant (e.g. median)
        if ('simple_approximation' in exceptions.keys()) and (field in exceptions['simple_approximation'].keys()):
            simple_approx = exceptions['simple_approximation'][field](
                y_input[x0:][np.where(y_input[x0:] > 0)])

            y_trend = np.array([simple_approx for _ in range(len(x_input))])
            y_approx = np.array([simple_approx for _ in range(len(x_input))])

        else:
            y_trend, trend_func, trend_popt, = trend_chooser(x_tr, y_tr, x0, return_y=True)

            # EXCEPTIONS - fix last trend of time series according to it's own trend forecast
            if ('own_trend' in exceptions.keys()) and \
                    (len(exceptions['own_trend'].loc[exceptions['own_trend'][field_col] == field, :])):
                fix_trend = True

            y_approx, params = approx_ts_by_fourier(x_input, x0, y_input, trend_func,
                                                    fourier_order, fix_trend, *trend_popt)

    # If prediction period more than 0, then make forecast with fitted parameters
    if prediction:
        x_predict = np.arange(x_input[-1], x_input[-1] + prediction, 1)

        # EXCEPTIONS - if the data amount is low then set approximation as constant (e.g. median)
        if ('simple_approximation' in exceptions.keys()) and (field in exceptions['simple_approximation'].keys()):
            y_trend_predict = np.array([simple_approx for _ in range(len(x_predict))])
            y_predict = np.array([simple_approx for _ in range(len(x_predict))])

        else:
            y_trend_predict = params['Trend_func'](x_predict - x0, *params['Trend_params'])
            y_predict = params['Trend_func'](x_predict - x0, *params['Trend_params']) + \
                        sum([func_general_fourier_k(x_predict - x0,
                                                    *params[f'Fourier-{k}'], k, params['Trend_func'],
                                                    *params['Trend_params']) for k in range(1, fourier_order + 1, 1)])
    else:
        y_predict = np.nan
        y_trend_predict = np.nan

    return y_trend, y_approx, y_predict, y_trend_predict
