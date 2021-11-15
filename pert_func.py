from pert import PERT


def pert_function(a, c, b=None, mu=None, lamb=4, amount=100000):

    # If Mean is input instead of Mode
    if mu and not b:
        b = (mu * (lamb + 2) - a - c) / lamb

    # If input is a history data without variation
    if a == c:
        a = b - 0.001 * b
        c = b + 0.001 * b

    # If there is a high skewness appeared
    while b > c:
        lamb += 1
        b = (mu * (lamb + 2) - a - c) / lamb

    while b < a:
        lamb -= 1
        b = (mu * (lamb + 2) - a - c) / lamb

    try:
        pert = PERT(a, b, c, lamb).rvs(int(amount))
    except:
        pert = None
        print('Error in b/mu parameter')
    return pert
