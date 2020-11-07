import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os
from typing import Dict, List, Tuple


def get_old_cutoffs():
    """Get the old cutoff values."""
    return {"IgA": 1.1, "IgG": 1.1, "R": 1.0}


def get_new_cutoffs(base_path):
    """Get the new cutoff values from file."""
    # cutoff keys -> data keys
    key_map = {"iga": "IgA", "igg": "IgG", "roche": "R"}

    df = pd.read_csv(
        os.path.join(base_path, "AlgorithmResults", "cutoffs.csv"),
        index_col=0)
    cutoffs = {}
    for key in ["iga", "igg", "roche"]:
        cutoffs[key_map[key]] = df.loc[key, "Median"]
    return cutoffs


def get_data_arr(df: dict, data_key: str) -> np.ndarray:
    """Get data as numpy array."""
    return np.array(df[data_key + '_quant'])


def get_var_id_stuff(
        df: pd.DataFrame, var: str
) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Get the variables as array, unique, as a matrix, and sizes."""
    # all variable values
    var_ids = np.array(df[var])
    # unique ids
    var_ids_uq = np.array(df[var].unique())
    # as a boolean matrix, shape (n_ids_uq, nrow)
    var_id_matrix = np.array([var_ids == var_id for var_id in var_ids_uq])
    # number of corresponding entries for each unique var
    var_sizes = np.array([sum(var_ids == var_id) for var_id in var_ids_uq])
    return var_ids, var_ids_uq, var_id_matrix, var_sizes


def statistic_mean_pd(df, data_key, var, var_ids_uq):
    """Mean of means clustered by var, using pandas."""
    means = [np.mean(df[df[var] == var_id][data_key + '_quant'])
             for var_id in var_ids_uq]
    return np.mean(means)


def statistic_var_pd(df, data_key, var, var_ids_uq):
    """Mean of variances clustered by var, using pandas."""
    vars = [np.var(df[df[var] == var_id][data_key + '_quant'])
            for var_id in var_ids_uq]
    return np.mean(vars)


def statistic_mean_np_iter(data_arr, var_ids, var_ids_uq):
    """Mean of means clustered by var, using numpy."""
    means = [np.mean(data_arr[var_ids == var_id])
             for var_id in var_ids_uq]
    return np.mean(means)


def statistic_var_np_iter(data_arr, var_ids, var_ids_uq):
    """Mean of variances clustered by var, using numpy."""
    vars = [np.var(data_arr[var_ids == var_id]) for var_id in var_ids_uq]
    return np.mean(vars)


def create_data_matrix(data_arr, var_id_matrix) -> np.ndarray:
    """Create data matrix, non-zero where the variable is active."""
    return data_arr * var_id_matrix


def statistic_mean(data_matrix, var_sizes):
    """Mean of means clustered by var, using vectorized numpy."""
    # data_matrix: n_var x n_individual
    #  Entry is non-zero if individual i in var j, and then corresponds to
    #  the data.
    return np.mean(np.sum(data_matrix, axis=1) / var_sizes)


def statistic_var(data_matrix, var_sizes, var_id_matrix):
    """Mean of variances clustered by var, using vectorized numpy."""
    means = np.sum(data_matrix, axis=1) / var_sizes
    means = means.reshape((-1,  1)) * var_id_matrix  # vectorize
    vars = np.sum((data_matrix - means)**2, axis=1) / var_sizes
    return np.mean(vars)


def get_data_arr_perms(data_arr, n_perm) -> List[np.ndarray]:
    """Permute the array n_perm times."""
    return [data_arr[np.random.permutation(len(data_arr))]
            for _ in range(n_perm)]


def permute_ixs_fix_hhs(ndata, ixs_by_hh_size) -> np.ndarray:
    """Permute only households of the same size."""
    # for the index permutations
    perm_ix_arr = np.empty(ndata, dtype=int)
    for size, hhs in ixs_by_hh_size.items():
        # household permutations of size size
        perm = np.random.permutation(len(hhs))
        for old_hh_ixs, perm_ix in zip(hhs, perm):
            # fill permuted indices into old locations
            perm_ix_arr[old_hh_ixs] = hhs[perm_ix]
    return perm_ix_arr


def get_data_arr_perms_fix_hhs(
        data_arr, n_perm, ixs_by_hh_size) -> List[np.ndarray]:
    """Permutation of households of the same size for all data keys."""
    return [data_arr[permute_ixs_fix_hhs(len(data_arr), ixs_by_hh_size)]
            for _ in range(n_perm)]


def save_data(id_, perm_path, **kwargs):
    """Save data to file."""
    for obj_key in kwargs:
        arr = kwargs[obj_key]
        # use numpy functions for convenience
        if not isinstance(arr, np.ndarray):
            arr = np.array([arr])
        np.savetxt(f"{perm_path}/data/{obj_key}_{id_}.csv", arr, delimiter=',')


def load_data(obj_keys, id_, perm_path):
    """Load data from file."""
    ret = []
    for obj_key in obj_keys:
        arr = np.loadtxt(
            f"{perm_path}/data/{obj_key}_{id_}.csv", delimiter=',')
        if arr.size == 1:
            arr = float(arr)
        ret.append(arr)
    return ret


def plot_kde(samples, obj_key, real_sample, data_key, id_, suptitle,
             perm_path):
    """Plot kernel density estimates."""
    fig, ax = plt.subplots(figsize=(3, 3))
    sns.kdeplot(samples, ax=ax, color='C0')
    ax.axvline(real_sample, color='C1')
    ax.set_xlabel(data_key)
    ax.set_ylabel("Density")
    fig.suptitle(suptitle)
    fig.tight_layout(rect=[0, 0.03, 1, 0.95])
    plt.savefig(f"{perm_path}/img/{obj_key}_kde_{id_}.png")


def plot_hist(samples, obj_key, real_sample, data_key, id_, suptitle,
              perm_path, nbins=None):
    """Plot histograms."""
    if nbins is None:
        nbins = int(len(samples) / 100)
    fig, ax = plt.subplots(figsize=(3, 3))
    ax.hist(samples, color='C0', bins=nbins)
    ax.axvline(real_sample, color='C1')
    ax.set_xlabel(data_key)
    ax.set_ylabel("Frequency")
    fig.suptitle(suptitle)
    fig.tight_layout(rect=[0, 0.03, 1, 0.95])
    plt.savefig(f"{perm_path}/img/{obj_key}_hist_{id_}.png")


def read_blab(base_path: str):
    """Read the baseline laboratory file."""
    return pd.read_csv(
        os.path.join(base_path, "KoCo19_Datasets", "Analysis Data Sets",
                     "Koco_baseline.csv"),
        encoding="iso-8859-1")


def read_geo(base_path: str):
    """Read the geo file containing address ids."""
    return pd.read_csv(
        os.path.join(
            base_path, "KoCo19_Datasets", "Geocodes",
            "KoCo19_Haushalte4Modeler_wRRstartConstituency_20200910.csv"))


def compute_clusters(base_path: str, threshold: float, hh_ids_uq: List):
    """Compute location clusters of households within the threshold."""
    # read in file
    distances = pd.read_csv(
        os.path.join(base_path, "KoCo19_Datasets", "Geocodes",
                     "KoCo19_Haushalte_DistanzMatrix_20201005.csv"))
    
    # remove all rows containing not considered households
    distances = distances[
        distances.household1.isin(hh_ids_uq)
        & distances.household2.isin(hh_ids_uq)]
    
    # we sort the distances to increase chances of capturing larger clusters
    distances = distances.sort_values(by="geodesic_km")

    hhs1 = np.array(distances["household1"])
    hhs2 = np.array(distances["household2"])
    geodesic = np.array(distances["geodesic_km"])

    # iterate until all households are in some cluster
    clusters = []
    while True:
        if geodesic[0] >= threshold:
            # no more distances smaller than threshold -> 1-clusters
            hhs = set(hhs1) | set(hhs2)
            for hh in hhs:
                clusters.append(np.array([hh]))
            break
        else:
            # reference household
            hh = hhs1[0]
            # all indices that have the same hh and a smaller distance
            close_hh = (geodesic < threshold) & ((hhs1 == hh) | (hhs2 == hh))
            # to array
            hhs = np.array(list(set(hhs1[close_hh]) | set(hhs2[close_hh])))
            # append to clusters
            clusters.append(hhs)
            # row indices containing only hhs outside the current list
            other_hh = np.invert(np.isin(hhs1, hhs) | np.isin(hhs2, hhs))
            # remove from arrays
            hhs1, hhs2, geodesic = hhs1[other_hh], hhs2[other_hh], geodesic[
                other_hh]
            # some popcorn cinema output
            print(len(hhs1), end=" ")
            if len(hhs1) == 0:
                # no more rows left
                break

    # also add households that have not been assigned yet
    all_hhs = set(distances["household1"]) | set(distances["household2"])
    assigned_hhs = set(hh for hhs in clusters for hh in hhs)
    for hh in all_hhs - assigned_hhs:
        print(hh)
        clusters.append(np.array([hh]))

    return clusters
