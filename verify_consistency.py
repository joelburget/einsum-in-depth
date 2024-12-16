import itertools
import numpy as np
import string
import unittest


def generate_random_einsum_expr(num_tensors, max_dim=5):
    # Pick a random set of indices for each tensor.
    # We'll choose dimension names from 'a'...'z' randomly.
    # For simplicity, we'll ensure no more than max_dim dims per tensor.
    letters = list(string.ascii_lowercase)
    # Randomly decide how many dims each tensor has
    dims_per_tensor = [np.random.randint(1, max_dim + 1) for _ in range(num_tensors)]

    # Assign dimension letters
    used_letters = []
    tensor_dims = []
    for d in dims_per_tensor:
        # Randomly choose d letters (allow repeats with some probability)
        # We'll allow repeats inside a single tensor to test diagonal extraction logic.
        # About 30% chance a letter repeats:
        chosen = []
        for _ in range(d):
            if np.random.rand() < 0.3 and len(chosen) > 0:
                # Repeat a previously chosen letter
                chosen.append(np.random.choice(chosen))
            else:
                # Pick a new letter
                # Just pick from global set; no need to ensure uniqueness across tensors.
                chosen.append(np.random.choice(letters))
        tensor_dims.append(chosen)
        used_letters.extend(chosen)

    # Decide the output indices:
    # We'll say about half of all unique letters appear in output.
    unique_letters = list(set(used_letters))
    np.random.shuffle(unique_letters)
    out_letters = unique_letters[: len(unique_letters) // 2]

    # Build the final expression
    input_exprs = ["".join(td) for td in tensor_dims]
    output_expr = "".join(out_letters)
    expr = ",".join(input_exprs) + "->" + output_expr
    return expr, tensor_dims


def generate_tensors_for_expr(tensor_dims):
    # Assign a size to each letter
    all_letters = set(itertools.chain.from_iterable(tensor_dims))
    letter_sizes = {l: np.random.randint(2, 5) for l in all_letters}  # small sizes

    # Create tensors
    tensors = []
    for dims in tensor_dims:
        shape = [letter_sizes[d] for d in dims]
        t = np.random.randn(*shape)
        tensors.append(t)
    return tensors


def direct_summation(expr, tensors):
    # Parse the expression
    lhs, rhs = expr.split("->")
    inputs = lhs.split(",")
    out_indices = list(rhs)

    # Identify all indices
    all_in_indices = [list(i) for i in inputs]
    all_indices = list(set(itertools.chain.from_iterable(all_in_indices)))
    # Classify indices
    summation_indices = [i for i in all_indices if i not in out_indices]

    # Map indices to dimension sizes
    # Already have them from tensors
    # We can get a dictionary mapping index to size by looking at the first tensor that has it.
    idx_sizes = {}
    for inp, T in zip(all_in_indices, tensors):
        for ix, dim_name in enumerate(inp):
            if dim_name not in idx_sizes:
                idx_sizes[dim_name] = T.shape[ix]
            else:
                # Consistency check
                assert idx_sizes[dim_name] == T.shape[ix]

    # Prepare output
    out_shape = [idx_sizes[i] for i in out_indices]
    out_array = np.zeros(out_shape, dtype=tensors[0].dtype)

    # Nested loops
    # We'll iterate over output indices first
    # Then sum over summation indices.
    # For efficiency, we can use itertools.product, but here we keep it explicit for clarity.
    out_ranges = [range(idx_sizes[i]) for i in out_indices]
    sum_ranges = [range(idx_sizes[i]) for i in summation_indices]

    for out_pos in itertools.product(*out_ranges):
        # Create a mapping from index name -> coordinate
        idx_map = {idx: coord for idx, coord in zip(out_indices, out_pos)}
        val = 0.0
        for sum_pos in itertools.product(*sum_ranges):
            for s_idx, s_i in enumerate(summation_indices):
                idx_map[s_i] = sum_pos[s_idx]

            # Multiply corresponding elements
            prod = 1.0
            for inp, T in zip(all_in_indices, tensors):
                # Extract coords
                coords = tuple(idx_map[i] for i in inp)
                prod = prod * T[coords]
            val += prod
        out_array[out_pos] = val
    return out_array


def extract_diagonals(
    tensor: np.ndarray, inds: list[str]
) -> tuple[np.ndarray, list[str]]:
    """
    Extract diagonals for repeated indices within the same input tensor
    For example, if a tensor has indices [i,i,j], we take diagonal along i.
    """
    counts = {}
    for x in inds:
        counts[x] = counts.get(x, 0) + 1

    # For each set of repeated indices, we take diagonals
    # If an index appears multiple times, we apply np.diagonal repeatedly.
    T = tensor
    new_inds = inds[:]

    # Process each index that appears more than once
    for idx, cnt in counts.items():
        if cnt > 1:
            # Extract diagonals for this index
            # We need to find all axes where this index occurs
            occurrences = [ax for ax, val in enumerate(new_inds) if val == idx]
            # We'll repeatedly diagonalize along the first pair of those occurrences.

            # While we have more than one occurrence of idx, diagonalize
            while len(occurrences) > 1:
                ax1, ax2 = occurrences.pop(), occurrences.pop()
                # Reorder so ax1 < ax2 for consistency
                if ax1 > ax2:
                    ax1, ax2 = ax2, ax1
                T = np.diagonal(T, axis1=ax1, axis2=ax2)
                new_inds.pop(ax2)
                new_inds.pop(ax1)
                occurrences += [len(new_inds)]
                new_inds += [idx]

    ordered_new_inds = sorted(new_inds, key=lambda x: inds.index(x))

    T = np.einsum(f"{''.join(new_inds)} -> {''.join(ordered_new_inds)}", T)
    return T, ordered_new_inds


# Bring each tensor into a consistent order of axes:
def reorder_axes(T: np.ndarray, inds: list[str], order: list[str]) -> np.ndarray:
    # inds is a list of indices for T
    # order is the full order
    axis_map = [inds.index(i) for i in order if i in inds]
    # If some indices in 'order' are not in 'inds', it means we need to broadcast
    # We can add those dimensions with np.reshape if needed.
    # Actually, since T is already broadcasted, let's just expand dims:

    # Insert missing dims:
    missing_indices = [i for i in order if i not in inds]
    # For each missing index, we add a dimension of size idx_sizes[i]
    for m in missing_indices:
        T = np.expand_dims(T, axis=-1)  # temporarily add at the end
        inds.append(m)

    # Now reorder to match 'order'
    # After adding missing, inds now has all indices but in some order
    axis_map = [inds.index(i) for i in order]
    return np.transpose(T, axis_map)


def make_common_shape(extracted_tensors, extracted_inds):
    """
    Broadcast all tensors to a common shape for pointwise multiplication.
    """
    full_index_order = sorted(list(set(itertools.chain.from_iterable(extracted_inds))))

    # To do that, gather a mapping index->size and ensure consistency.
    idx_sizes = {}
    for inds, T in zip(extracted_inds, extracted_tensors):
        for i, idx in enumerate(inds):
            size = T.shape[i]
            if idx not in idx_sizes:
                idx_sizes[idx] = size
            else:
                assert idx_sizes[idx] == size or size == 1 or idx_sizes[idx] == 1
                idx_sizes[idx] = max(idx_sizes[idx], size)

    # Broadcast
    broadcasted_tensors = []
    for inds, T in zip(extracted_inds, extracted_tensors):
        shape = [idx_sizes[i] for i in inds]
        T_broadcasted = np.broadcast_to(T, shape)
        broadcasted_tensors.append(T_broadcasted)

    reordered_tensors = []
    for T, inds in zip(broadcasted_tensors, extracted_inds):
        T = reorder_axes(T, inds, full_index_order)
        T = np.broadcast_to(T, [idx_sizes[i] for i in full_index_order])
        reordered_tensors.append(T)

    return reordered_tensors, idx_sizes, full_index_order


def second_interpretation(expr, tensors):
    """
    Medium-level interpretation:
    1. Extract diagonals for repeated indices within each input tensor.
    2. Broadcast all to a common shape.
    3. Pointwise multiply.
    4. Sum over summation indices.
    """

    lhs, rhs = expr.split("->")
    inputs = lhs.split(",")
    all_in_indices = [list(i) for i in inputs]

    # 1. Extract diagonals for repeated indices within the same input tensor
    extracted_tensors = []
    extracted_inds = []
    for T, inds in zip(tensors, all_in_indices):
        T2, inds2 = extract_diagonals(T, inds)
        extracted_tensors.append(T2)
        extracted_inds.append(inds2)

    # Now each tensor has unique indices.
    # 2. Broadcast all to a common shape for pointwise multiplication.
    reordered_tensors, idx_sizes, common_shape = make_common_shape(
        extracted_tensors, extracted_inds
    )

    # 3. Pointwise multiply them all
    prod = 1.0
    for T in reordered_tensors:
        prod = prod * T

    # 4. Sum over summation indices
    return np.einsum(f"{''.join(common_shape)} -> {rhs}", prod)


def verify_consistency(num_trials=50):
    for _ in range(num_trials):
        num_tensors = np.random.randint(2, 5)
        expr, tensor_dims = generate_random_einsum_expr(num_tensors)
        tensors = generate_tensors_for_expr(tensor_dims)

        # Method 1: direct summation
        res1 = direct_summation(expr, tensors)

        # Method 2: second interpretation
        res2 = second_interpretation(expr, tensors)

        # Method 3: np.einsum
        res3 = np.einsum(expr, *tensors)

        # Compare
        if not (np.allclose(res1, res2) and np.allclose(res2, res3)):
            print("Mismatch found!")
            print("Expression:", expr)
            print("res1:", res1)
            print("res2:", res2)
            print("res3:", res3)
            return
    print("All tests passed; all interpretations agree.")


class Tests(unittest.TestCase):
    def test_extract_diagonals(self):
        # Test diagonal extraction
        T = np.random.randn(3, 3, 2)
        T2, inds2 = extract_diagonals(T, ["i", "i", "j"])
        assert T2.shape == (3, 2)
        assert inds2 == ["i", "j"]
        np.testing.assert_allclose(T2, np.einsum("iij->ij", T))

        T = np.random.randn(3, 2, 3)
        T2, inds2 = extract_diagonals(T, ["i", "j", "i"])
        assert T2.shape == (3, 2)
        assert inds2 == ["i", "j"]
        np.testing.assert_allclose(T2, np.einsum("iji->ij", T))

        T = np.random.randn(3, 2, 2)
        T2, inds2 = extract_diagonals(T, ["i", "j", "j"])
        assert T2.shape == (3, 2)
        assert inds2 == ["i", "j"]
        np.testing.assert_allclose(T2, np.einsum("ijj->ij", T))

    def test_reorder_axes(self):
        T = np.random.randn(3, 4, 5)
        T2 = reorder_axes(T, ["i", "j", "k"], ["k", "i", "j"])
        assert T2.shape == (5, 3, 4)
        np.testing.assert_equal(T2, np.transpose(T, (2, 0, 1)))

        T = np.random.randn(3, 5)
        T2 = reorder_axes(T, ["i", "k"], ["k", "i", "j"])
        assert T2.shape == (5, 3, 1)
        np.testing.assert_equal(T2, np.expand_dims(T, axis=1).transpose((2, 0, 1)))

        T = np.random.randn(3)
        T2 = reorder_axes(T, ["i"], ["k", "i", "j"])
        assert T2.shape == (1, 3, 1)
        np.testing.assert_equal(
            T2, np.expand_dims(np.expand_dims(T, axis=1), axis=2).transpose((2, 0, 1))
        )

    def test_make_common_shape(self):
        T1 = np.random.randn(3, 4)
        T2 = np.random.randn(4, 5)
        reordered_tensors, idx_sizes, full_index_order = make_common_shape(
            [T1, T2], [["i", "j"], ["j", "k"]]
        )
        assert len(reordered_tensors) == 2
        assert full_index_order == ["i", "j", "k"]
        assert idx_sizes == {"i": 3, "j": 4, "k": 5}
        assert reordered_tensors[0].shape == (3, 4, 5)
        assert reordered_tensors[1].shape == (3, 4, 5)
        np.testing.assert_equal(
            reordered_tensors[0], np.broadcast_to(np.expand_dims(T1, axis=2), (3, 4, 5))
        )
        np.testing.assert_equal(
            reordered_tensors[1], np.broadcast_to(np.expand_dims(T2, axis=0), (3, 4, 5))
        )

    def test_consistency(self):
        verify_consistency()


if __name__ == "__main__":
    unittest.main()
