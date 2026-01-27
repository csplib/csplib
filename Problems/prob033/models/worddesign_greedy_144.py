"""
Greedy construction for CSPLib problem 033.

Produces a DNA word set of size 144 for words of length 8,
satisfying:
- GC content exactly 4
- Hamming distance ≥ 4
- reverse-complement distance ≥ 4

The heuristic ordering was inspired by automated heuristic evolution methods (Liu et al., 2024, arXiv:2401.02051).

Run:
    python worddesign_greedy_144.py
"""

from time import time
import time
import numpy as np

INT_TO_BASE = np.array(list("ACGT"), dtype="<U1")

#== Scoring Function ===
# heuristic discovered using Evolution of Heuristics (Liu et al., 2024, arXiv:2401.02051)

def score(word, p0=1.0, p1=1.0):
    """
    Structure score:
      score = p0 * I(non-palindromic enough) + p1 * (sym_score - asym_score)

    Interpretable components:
    - pal_matches: number of mirror matches (0..8)
    - pos_diff_rev: number of mirror mismatches (0..8)
    - half_jump_penalty: counts positions where first-half vs second-half differ "a lot" in numeric encoding
    - half_equal: number of positions where first half equals second half (0..4)
    """
    w = np.asarray(word)
    n = len(w)
    half = n // 2

    # Mirror symmetry vs reverse (palindrome)
    pal_matches = np.sum(w == w[::-1])
    pos_diff_rev = n - pal_matches

    # "Symmetry-ish" term 
    half_jump_penalty = np.sum(np.abs(w[:half] - w[half:]) > 1)
    sym_score = pal_matches - half_jump_penalty

    # "Asymmetry between halves" term 
    half_equal = np.count_nonzero(w[:half] == w[half:])
    asym_score = 2 * (half - half_equal)

    # Combination
    return (pos_diff_rev >= half) * p0 + (sym_score - asym_score) * p1

# Generation of all DNA words of given length
def generate_all_dna_words(word_length):
    total = 4 ** word_length
    numbers = np.arange(total, dtype=np.int64)
    words = np.empty((total, word_length), dtype=np.int8)
    for i in range(word_length):
        words[:, word_length - i - 1] = numbers % 4
        numbers //= 4
    return words

# Application of greedy heuristic to construct word set
def greedy_worddesign(word_length=8, p0=1.0, p1=1.0):
    n = word_length
    d = n // 2

    vectors = generate_all_dna_words(n)

    # GC mask: exactly d positions are 1 or 2
    gc_mask = (np.sum((vectors == 1) | (vectors == 2), axis=1) == d)

    # Reverse-complements of all words 
    complement_mapping = np.array([3, 2, 1, 0], dtype=np.int8)
    vectors_complement = complement_mapping[vectors[:, ::-1]]

    priorities = np.array([score(v, p0=p0, p1=p1) for v in vectors], dtype=float)

    # enforce GC up-front 
    priorities[~gc_mask] = -np.inf

    wordset = []
    while np.any(priorities != -np.inf):
        max_index = int(np.argmax(priorities))
        selected = vectors[max_index]

        differences = np.sum(vectors != selected, axis=1)
        mask_complementary = (np.sum(vectors_complement != selected, axis=1) >= d)

        mask_invalid = (differences < d) | (~mask_complementary) 
        priorities[mask_invalid] = -np.inf
        priorities[max_index] = -np.inf

        wordset.append(selected)

    return np.array(wordset, dtype=np.int8)

# utility to print words in base letters
def word_to_acgt(word_vec):
    return "".join(INT_TO_BASE[word_vec].tolist())

def print_wordset_acgt(subset_array, row_size=8, blocks_per_line=1):
    n = len(subset_array)
    i = 0
    while i < n:
        parts = ["|"]
        for _ in range(blocks_per_line):
            for _ in range(row_size):
                if i >= n:
                    break
                parts.append(f" {word_to_acgt(subset_array[i])} |")
                i += 1
            if i >= n:
                break
        # parts.append("")
        print("".join(parts))

def hamming(a, b):
    return int(np.sum(a != b))

def revcomp(v):
    comp = np.array([3, 2, 1, 0], dtype=np.int8)  # A<->T, C<->G under 0/1/2/3
    return comp[v[::-1]]

def final_check(wordset, d=4, verbose=True):
    """
    Final verification for prob033 (length 8, d=4):
      - GC content exactly 4 for every word
      - Hamming distance >= d for every distinct pair
      - reverse-complement distance >= d for every distinct pair

    Prints the first few counterexamples if any constraint is violated.
    Returns True iff all checks pass.
    """
    wordset = np.asarray(wordset)
    n_words, n = wordset.shape

    ok = True

    # 1) Per-word GC content
    gc = np.sum((wordset == 1) | (wordset == 2), axis=1)  # C or G under 0/1/2/3
    bad_gc_idx = np.where(gc != 4)[0]
    if len(bad_gc_idx) > 0:
        ok = False
        if verbose:
            print(f"[FAIL] GC content != 4 for {len(bad_gc_idx)} words (showing up to 5):")
            for idx in bad_gc_idx[:5]:
                print(f"  idx={idx} word={word_to_acgt(wordset[idx])} GC={int(gc[idx])}")

    # 2) Pairwise distances
    bad_hamm = []
    bad_rc = []
    for i in range(n_words):
        wi = wordset[i]
        for j in range(i + 1, n_words):
            wj = wordset[j]
            hd = hamming(wi, wj)
            if hd < d:
                ok = False
                if len(bad_hamm) < 5:
                    bad_hamm.append((i, j, hd))

            rc_hd = hamming(wi, revcomp(wj))
            if rc_hd < d:
                ok = False
                if len(bad_rc) < 5:
                    bad_rc.append((i, j, rc_hd))

    if verbose:
        if bad_hamm:
            print(f"[FAIL] Found Hamming distance < {d} for some pairs (showing up to 5):")
            for i, j, hd in bad_hamm:
                print(f"  ({i},{j}) hd={hd}  {word_to_acgt(wordset[i])}  {word_to_acgt(wordset[j])}")
        if bad_rc:
            print(f"[FAIL] Found reverse-complement distance < {d} for some pairs (showing up to 5):")
            for i, j, hd in bad_rc:
                print(f"  ({i},{j}) rc_hd={hd}  {word_to_acgt(wordset[i])}  {word_to_acgt(wordset[j])}")
        if ok:
            print(f"[OK] Final check passed: |S|={n_words}, length={n}, GC=4, d={d}")

    return ok

if __name__ == "__main__":

    # construct word set and time it
    t0 = time.perf_counter()
    subset = greedy_worddesign(word_length=8, p0=1.0, p1=1.0)
    t1 = time.perf_counter()

    print(f"Constructed |S|={len(subset)} in {t1 - t0:.3f} seconds")
    # final check: does the set match constraints
    assert final_check(subset, d=4, verbose=True)

    # show solution in ACGT format 
    print_wordset_acgt(subset, row_size=8, blocks_per_line=1)

