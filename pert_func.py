from pert import PERT


def pert_function(a, c, b=None, mu=None, lamb=4, amount=100000):
    if b:
        pert = PERT(a, b, c, lamb).rvs(int(amount))
    elif mu:
        b = (mu * (lamb + 2) - a - c) / lamb
        pert = PERT(a, b, c, lamb).rvs(int(amount))
    else:
        pert = None
        print('Error in b/mu parameter')
    return pert
