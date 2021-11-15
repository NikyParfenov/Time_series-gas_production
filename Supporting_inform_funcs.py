import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error


# Information functions (prints and plots)

def change_point_graph(x_input: np.ndarray,
                       y_input: np.ndarray,
                       y_trend: np.ndarray,
                       change_point: int,
                       field: str,
                       var_data: pd.DataFrame,
                       threshold: float) -> None:
    """
    Function of output change point information and plot variance graphics

    :param x_input:      Days data of time series
    :param y_input:      Values of time series
    :param y_trend:      Trend values
    :param change_point: Change point of time series
    :param field:        The name of field
    :param var_data:     DataFrame with squared errors
    :param threshold:    Cut-off for variance (min/mean and min/median)

    :return:             Print metrics and plot graphs of variance and the place of change point on time series
    """

    variance = var_data['Variance'] if var_data['Variance'].min() < var_data['Variance_trend'].min() else var_data[
        'Variance_trend']
    var_min = variance.min()
    var_mean = variance.mean()
    var_median = variance.median()

    print(f'Change point = {change_point + x_input[0]}, '
          f'Min Disp = {var_min}, '
          f'Min/Mean = {var_min / var_mean:.3}, '
          f'Min/Median = {var_min / var_median:.3}, '
          f'Threshold = {threshold}')

    fig = plt.figure(figsize=(16, 4))
    plt.suptitle(field, fontsize=14)
    plt.subplots_adjust(wspace=0.4)
    ax1 = fig.add_subplot(121)
    ax1.scatter(var_data['Point'], var_data['Variance'], s=1, c=var_data['Variance'], cmap='coolwarm_r')
    ax1.set_ylabel('Variance', color='darkred')
    ax1_1 = ax1.twinx()
    ax1_1.scatter(var_data['Point'], var_data['Variance_trend'], s=1, c=var_data['Variance_trend'], cmap='BrBG_r')
    ax1_1.set_ylabel('Variance trend', color='darkgreen')
    ax1.set_title('Variance')

    ax2 = fig.add_subplot(122)
    ax2.plot(x_input, y_trend, linewidth=1, linestyle='dashed', color='green')
    ax2.scatter(x_input, y_input, s=1)
    if change_point:
        ax2.vlines(x=change_point + x_input[0], ymin=y_input.min(), ymax=y_input.max(), color='red', linestyle='dashed')
    ax2.set_ylim(bottom=0, top=y_input[np.where(y_input < np.quantile(y_input, 0.99))].max() * 1.5)
    ax2.set_title('Time Series')
    plt.show()


def ts_graphic(y_true: np.ndarray,
               result: pd.DataFrame,
               field: str,
               date_col: str,
               target_col: str,
               days_col: str,
               prediction: int = 0,
               approx_alpha: float = 0.01,
               noise_alpha: float = 0.05) -> None:
    """
    Function for plotting time series approximation with trends

    :param y_true:       Values of time series
    :param result:       Data Frame with approximations
    :param field:        The name of field
    :param prediction:   Forecast period
    :param approx_alpha: Allowed approx error in statistical accuracy (default=0.01)
    :param noise_alpha: Allowed noise in statistical accuracy (default=0.05)
    :param date_col:     Column name with date
    :param target_col:   Column name with target (production)
    :param days_col:     Column name with days

    :return:             Plot with time series approximation
    """
    x_ts = result[date_col].iloc[:len(result)-prediction].values
    y_approx = (result['Approximation'] + result['Single_Noise']).iloc[:len(result)-prediction].values
    y_trend = result['Trend'].iloc[:len(result)-prediction].values
    y_trend_pred = result['Trend'].iloc[-prediction:].values

    x_predict = result[date_col].iloc[-prediction:].values
    y_predict = result['Approximation'].iloc[-prediction:].values
    conf_int = (result[f'Series_T-stat_{1 - noise_alpha}'] * result['Series_STD']).iloc[-prediction:].values
    graph_top = y_predict + result[f'Approx_error_quantile_{1 - approx_alpha / 2}'].iloc[-prediction:].values

    plt.figure(figsize=(14, 6))
    plt.scatter(x_ts, y_true, edgecolors='lightsteelblue', alpha=0.5, label='Time Series')
    plt.plot(x_ts, y_approx, color='orangered', linewidth=1, label='Approximation')
    plt.plot(x_ts, y_trend, color='limegreen', linestyle='dashed', linewidth=1, label='Approximated trend')
    plt.ylim(bottom=0, top=np.max(y_approx) * 1.35)

    # Plot prediction with variations (if available)
    if prediction:
        plt.fill_between(x_predict,
                         y_predict + result[f'Approx_error_quantile_{approx_alpha / 2}'].iloc[-prediction:].values,
                         y_predict + result[f'Approx_error_quantile_{1 - approx_alpha / 2}'].iloc[-prediction:].values,
                         color='lightgrey', label=f'Approx error {int(100 * (1 - approx_alpha))}% interval', alpha=0.5)
        plt.fill_between(x_predict, y_predict + conf_int, y_predict - conf_int,
                         color='orange', label=f'Confident {int(100 * (1 - noise_alpha))}% interval', alpha=0.5)
        plt.plot(x_predict, y_predict,
                 color='red', linewidth=1, linestyle='dashed',
                 label=f'Prediction on {prediction} days', alpha=0.7)
        plt.plot(x_predict, y_trend_pred, color='green', linestyle='dotted', linewidth=1, label='Predicted trend')
        plt.ylim(bottom=0, top=np.concatenate((y_approx, graph_top[np.where(~np.isnan(graph_top))])).max() * 1.35)

    plt.legend(loc=2, prop={'size': 10})
    plt.title(field, fontsize=14)
    plt.xlabel(f'{days_col}, сут')
    plt.ylabel(f'{target_col}, тыс. м3')
    plt.show()


def metrics_calc(y_true: np.array,
                 y_approx: np.array,
                 silent: bool = False) -> (float, float, float):
    """
    Function of calculating ang printing quality metrics

    :param y_true:   Time series values
    :param y_approx: Approximated values
    :param silent:   Print metrics (False) or don't (True)

    :return:         Metrics: R2, MAE, RMSE
    """

    r2 = r2_score(y_true, y_approx)
    mae = mean_absolute_error(y_true, y_approx)
    rmse = np.sqrt(mean_squared_error(y_true, y_approx))

    if not silent:
        print(f'R2={r2:.3}; '
              f'MAE={np.around(mae, 2)} (mean_y={np.around(y_true.mean(), 2)}); '
              f'RMSE={np.around(rmse, 2)}')

    return r2, mae, rmse
