import matplotlib.pyplot as plt
import numpy as np
import algoxtools as axt

INDEX, META, SOLUTION_COUNT, SOLUTION, VALUE = 0, -1, 0, 1, -1
dt = np.int16


def random_rgb(seed):
    np.random.seed(seed)
    return np.random.random(3)


pieces = [
    # First one
    np.array([[1, 0],
              [1, 1],
              [1, 0],
              [1, 0]]),
    np.array([[1, 1, 1, 1],
              [0, 0, 1, 0]]),
    np.array([[0, 1],
              [0, 1],
              [1, 1],
              [0, 1]]),
    np.array([[0, 1, 0, 0],
              [1, 1, 1, 1]]),
    # Second one
    np.array([[1, 0],
              [1, 0],
              [1, 1],
              [1, 0]]),
    np.array([[1, 1, 1, 1],
              [0, 1, 0, 0]]),
    np.array([[0, 1],
              [1, 1],
              [0, 1],
              [0, 1]]),
    np.array([[0, 0, 1, 0],
              [1, 1, 1, 1]]),
]

if __name__ == '__main__':
    display_solutions = True

    h, w = 15, 15
    n = len(pieces)

    n_set = ((h - 3) * (w - 3) + (h - 3) + (w - 3)) * n
    n_element = h * w

    # Initialize array
    array = axt.init(n_set, n_element)

    # Assign nodes. Rows and cols start from 1!
    k_set = 0
    for p in pieces:
        hp, wp = p.shape

        for r in range(h - hp + 1):
            for c in range(w - wp + 1):
                k_set += 1

                R = (r + np.arange(hp)).reshape(hp, 1)
                C = (c + np.arange(wp)).reshape(1, wp)

                d = ((w * R + C + 1) * p).flatten()
                s = d[d > 0].astype(dt)

                axt.annex_row(array, k_set, s)

    print('Number of sets:', k_set, ' ; expected:', n_set)

    # Get result
    ii = array[INDEX, INDEX]
    while axt.exact_cover(array):
        solution = array[META, SOLUTION: ii[VALUE], VALUE]
        if display_solutions:
            k_gray = 0
            img = np.zeros(shape=(h, w, 3))
            for k_piece in solution:
                piece = array[k_piece, 1:, -1].reshape(h, w, 1)
                img += piece * random_rgb(k_piece)
                k_gray += 1
            plt.imshow(img)
            plt.show()

    print("Number of solution:", array[META, SOLUTION_COUNT, VALUE])
