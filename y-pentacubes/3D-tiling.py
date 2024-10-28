import numpy as np
import algoxtools as axt
from matplotlib import pyplot as plt

INDEX, META, SOLUTION_COUNT, SOLUTION, VALUE = 0, -1, 0, 1, -1
dt = np.int16


def random_rgba(seed, alpha):
    np.random.seed(seed)
    rgba = np.full(4, alpha)
    rgba[:3] = np.random.random(3)
    return rgba


def generate_3d_pieces():
    def along_dimension(dim4):
        # The y-pentacube is along the dim4-th dimension
        acc_pieces = []

        for dim2 in range(3):
            if dim2 != dim4:
                shape = [4 if k == dim4 else
                         (2 if k == dim2 else 1)
                         for k in range(3)]

                for offset in (0, 1):
                    index_offset = tuple(offset if k == dim2 else slice(None)
                                         for k in range(3))

                    for solo in (1, 2):
                        index_solo = tuple(1 - offset if k == dim2 else
                                           (solo if k == dim4 else slice(None))
                                           for k in range(3))

                        piece = np.zeros(shape=tuple(shape))
                        piece[index_offset] = 1
                        piece[index_solo] = 1

                        acc_pieces.append(piece)

        return acc_pieces

    return along_dimension(0) + along_dimension(1) + along_dimension(2)


if __name__ == '__main__':
    display_solutions = True
    start_at_solution = 0

    w, h, d = 5, 5, 5

    pieces = generate_3d_pieces()
    n = len(pieces)

    n_set = int(((w - 3) * (h - 3) * (d - 3) +
                (5 / 3) * ((w - 3) * (h - 3) + (w - 3) * (d - 3) + (h - 3) * (d - 3)) +
                2 * ((w - 3) + (h - 3) + (d - 3))) * n)
    n_element = w * h * d

    # Initialize array
    array = axt.init(n_set, n_element)

    # Assign nodes. Rows and cols start from 1!
    k_set = 0
    for p in pieces:
        wp, hp, dp = p.shape

        for x in range(w - wp + 1):
            for y in range(h - hp + 1):
                for z in range(d - dp + 1):
                    k_set += 1

                    X = (x + np.arange(wp)).reshape(wp, 1, 1)
                    Y = (y + np.arange(hp)).reshape(1, hp, 1)
                    Z = (z + np.arange(dp)).reshape(1, 1, dp)

                    f = (((h * d) * X + d * Y + Z + 1) * p).flatten()
                    s = f[f > 0].astype(dt)

                    axt.annex_row(array, k_set, s)

    print('Number of sets:', k_set, ' ; expected:', n_set)

    # Get results
    k_solution = 0
    ii = array[INDEX, INDEX]
    while axt.exact_cover(array):
        solution = array[META, SOLUTION: ii[VALUE], VALUE]
        k_solution += 1
        print('{} solutions found so far!'.format(k_solution))

        if display_solutions and k_solution > start_at_solution:
            for z in range(d):
                # Draw pentacubes up to z
                data = np.zeros(shape=(w, h, d), dtype=bool)
                colors = np.zeros(shape=(w, h, d, 4), dtype=float)
                for k_set in solution:
                    p = array[k_set, 1:, -1].reshape(w, h, d, 1)
                    if np.any(p[:, :, :z+1]):
                        data[p[:, :, :, 0] == 1] = True
                        colors += p * random_rgba(k_set, alpha=0.64 if np.any(p[:, :, :z]) else 1.)

                fig = plt.figure("Solution {} up to layer {} over {}".format(k_solution, z+1, d))
                ax = fig.add_subplot(111, projection='3d')
                ax.voxels(data, facecolors=colors, edgecolors='black', linewidth=0.5)

                # Plot axis arrows
                ax.quiver(0, 0, 0, w+1, 0, 0, color='r')
                ax.quiver(0, 0, 0, 0, h+1, 0, color='g')
                ax.quiver(0, 0, 0, 0, 0, d+1, color='b')

                plt.axis('equal')
                plt.axis('off')
                plt.show()

    print("Number of solution:", array[META, SOLUTION_COUNT, VALUE])
