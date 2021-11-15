import numpy as np
import pandas as pd
from datetime import timedelta
from scipy.ndimage import uniform_filter1d
from Calculation_funcs import change_point_finder, build_final_approx, variation, own_trend_converter
from Supporting_inform_funcs import ts_graphic, metrics_calc


# Aggregate function of time series approximation calculation. This function should be called in "main file".

def time_series_approximation(df_input: pd.DataFrame,
                              field: str,
                              prediction: int = 0,
                              fourier_order: int = 8,
                              fix_trend: bool = False,
                              changepoint_cutoff: float = 0.965,
                              changepoint_padding: int = 584,
                              changepoint_graph: bool = False,
                              add_noise: bool = True,
                              approx_alpha: float = 0.01,
                              noise_alpha: float = 0.05,
                              noise_window: int = 7,
                              monthly_variation: bool = True,
                              var_param_top: int = 2,
                              var_param_low: int = 3,
                              zeroing: bool = True,
                              picture: bool = True,
                              silent: bool = False,
                              date_col: str = 'Дата',
                              target_col: str = 'Добыча',
                              field_col: str = 'Месторождение',
                              exceptions: dict = {}) -> (np.ndarray, np.ndarray):
    """
    Function approximate time series relying on fitting the trend function (linear, logarithmic, hyperbolic) and Fourier
    approximation of choosen N-order. The input data should contains date, group column (field) and target (production).
    The output is Data Frame with date, approximated values of target and statistics for calculation approximation
    error, like t-statistic and STD of self-noise (standart distribution) and chosen quantiles of bootstrap errors.

        **Main inputs**
    :param df_input:            Data Frame with data (Date/Field/Production)
    :param field:               Name of current field
    :param prediction:          Number of forecast steps (in days)

        **Fourier approximation parameters**
    :param fourier_order:       The order of Fourier approximation
    :param fix_trend:           False - the trend built on all points can be corrected during 1st order Fourier fitting,
                                otherwise trend coefficients stay fixed after the initial trend fitting (default=False)

        **Change points choose parameters**
    :param changepoint_cutoff:  Cut-off for change_points selection, the higher cut-off the more points (default=0.965)
    :param changepoint_padding: The minimum interval for time series split by change points (default=365 * 1.6)
    :param changepoint_graph:   Whether plot graphs for each intervals (useful for tuning, default=False)

        **Noise options**
    :param approx_alpha:        The error for approx error statistical confident interval estimation (default=0.01)
    :param noise_alpha:         The error for noise statistical confident interval estimation (default=0.05)
    :param noise_window:        Moving average window for data self noise estimation
    :param monthly_variation:   Allows to split approximation errors into month groups (if the monthly data is enough)
    :param var_param_top:       Gaussian smoothing window for upper boundary of approximation error estimation
    :param var_param_low:       Moving average window for lower boundary of approximation error estimation

        **Additional parameters (default is recommended)**
    :param zeroing:             Zeroing values of time series less than 0, and values earlier then in time x0 (bias)
    :param picture:             Whether plot graphics with TS and resulting approximation or do not
    :param silent:              Show information during approximation (field name, metrics, general information, etc)
    :param date_col:            Column name with date
    :param target_col:          Column name with target (production)
    :param field_col:           Column name for group (field)
    :param exceptions:          Some parameters can be force added to any field e.g. additional change points (see
                                function 'change_point_finder'), simplify approximation mode by mean/median if
                                there is not enough data (see function 'build_final_approx') or use analytical trend
                                points forecast during trend fitting (see function 'build_final_approx').
                                Has the next fixed structure:
                                exceptions = {'change_points': {field: [...], ...},
                                              'simple_approximations': {field: np.mean/np.median, ...},
                                              }

    :return:                    Output Data Frame contains: date, month, approximation, standart deviations relying on
                                self noise (residuals: y_true - y_moving_aver.), random noise realization, low and high
                                bootstrap quantiles relying on approximation error (residuals: y_true - y_approximate),
                                trend values and group column (field)
    """

    # Print current field under work
    if not silent:
        print('*' * 120)
        print(f'The field "{field}"')

    # Select data group by field
    df = df_input.loc[df_input[field_col] == field, :]
    df.reset_index(drop=True, inplace=True)

    # Create days col from date
    days_col = 'Дни'
    df[days_col] = (df[date_col] - df[date_col].iloc[0]).dt.days

    # Estimate "zero point" - bias for X
    x0 = df.loc[df[[target_col]].ne(0).idxmax(), days_col].values[0]

    # Separate days (Х) and production (Y)
    x = df[days_col].values
    y = df[target_col].values

    # Search change points if exists
    change_points = change_point_finder(x, y, field, picture=changepoint_graph, padding=changepoint_padding,
                                        threshold=changepoint_cutoff, exceptions=exceptions)
    if not silent:
        print(f'Change points: {np.around(change_points / 365 + 2010, 1)}')

    # Sometime approximation is failed, so, input approximation function inside the try/except block
    approximation_success = False
    try:
        # Build approximation for each interval between change points
        y_trend, y_approx, y_predict, y_trend_prediction = build_final_approx(x, x0, y, field,
                                                                              date_col, field_col,
                                                                              target_col, days_col,
                                                                              fourier_order,
                                                                              fix_trend=fix_trend,
                                                                              change_points=change_points,
                                                                              prediction=prediction,
                                                                              exceptions=exceptions)
        if not silent:
            print(f'Fourier function of {fourier_order}-th order')

            # EXCEPTIONS - approximation informing
            if ('simple_approximation' in exceptions.keys()) and (field in exceptions['simple_approximation'].keys()):
                print(f'Use simple approximation {exceptions["simple_approximation"][field].__name__}')

        approximation_success = True

    except RuntimeError as approx_error:
        # If Fourier approximation with flexible trend has been failed, then trying to build it with fixed trend
        # reducing freedom degrees of the model (left only Fourier coefficients for matching)
        if not fix_trend:

            if not silent:
                print(f"Can't approximate with flexible trend, trying to approximate with fixed trend")
            try:
                y_trend, y_approx, y_predict, y_trend_prediction = build_final_approx(x, x0, y, field,
                                                                                      date_col, field_col,
                                                                                      target_col, days_col,
                                                                                      fourier_order,
                                                                                      fix_trend=True,
                                                                                      change_points=change_points,
                                                                                      prediction=prediction,
                                                                                      exceptions=exceptions)
                if not silent:
                    print(f'Fourier function of {fourier_order}-th order')

                    # EXCEPTIONS - approximation informing
                    if ('simple_approximation' in exceptions.keys()) and \
                       (field in exceptions['simple_approximation'].keys()):
                        print(f'Use simple approximation {exceptions["simple_approximation"][field].__name__}')

                approximation_success = True

            except RuntimeError as approx_error:
                if not silent:
                    print(f'Approximation has been failed!')

        else:
            if not silent:
                print(f'Approximation has been failed!')

    # If approximation has been succeed then work further: plot line and make forecast
    if approximation_success:

        # Result of approximation and other treatments
        result = pd.DataFrame()
        result[date_col] = pd.date_range(df.iloc[0][date_col], df.iloc[-1][date_col] + timedelta(days=prediction))

        result['Month'] = result[date_col].dt.month
        result['Approximation'] = np.concatenate((y_approx, y_predict), axis=None)
        result['Trend'] = np.concatenate((y_trend, y_trend_prediction), axis=None)

        # Build residuals that reflects "noise" (by t-stats)
        resid_noise = pd.DataFrame(data={'Month': df[date_col][x0:].dt.month,
                                         'Residuals': y[x0:] - uniform_filter1d(y[x0:], size=noise_window)})

        # Build approx error only on the last approximated interval
        if np.all(~np.isnan(change_points)):
            resid_approx = pd.DataFrame(data={'Month': df[date_col][change_points[-1]:].dt.month,
                                              'Residuals': y[change_points[-1]:] - y_approx[change_points[-1]:]})
        else:
            # Restriction for approximate error estimation (last 5 years)
            margin = 0 if len(df[date_col][x0:]) < 5 * 365 else int(np.round(len(df[date_col][x0:]) - 5 * 365))
            resid_approx = pd.DataFrame(data={'Month': df[date_col][x0 + margin:].dt.month,
                                             'Residuals': y[x0 + margin:] - y_approx[x0 + margin:]})

        # Build variations relying on residuals above
        result = variation(result, resid_noise,
                           approx_alpha=approx_alpha,
                           noise_alpha=noise_alpha,
                           approx_variation=False)

        result = variation(result, resid_approx,
                           approx_alpha=approx_alpha,
                           noise_alpha=noise_alpha,
                           approx_variation=True,
                           monthly_variation=monthly_variation,
                           var_param_top=var_param_top,
                           var_param_low=var_param_low)

        if trend_list:
            pass

        # Zeroing y < 0 and periods before start point 'x0'
        if zeroing:
            result['Single_Noise'].iloc[:x0] = 0
            result['Approximation'].iloc[:x0] = 0
            result['Trend'].iloc[:x0] = 0
            result.loc[result['Approximation'] < 0, 'Approximation'] = 0
            result.loc[result['Trend'] < 0, 'Trend'] = 0

        if not silent:
            # Show metrics
            metrics_calc(y[x0:], result['Approximation'].iloc[x0:len(result)-prediction].values)

        # Plot graph with approximation
        if picture:
            ts_graphic(y, result, field, date_col, target_col, days_col, prediction, approx_alpha, noise_alpha)

        return result
