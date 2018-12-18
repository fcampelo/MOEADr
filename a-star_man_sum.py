from heapq import heapify, heappush, heappop;
import random
import math
from datetime import datetime
import time
import numpy
import matplotlib.pyplot as plt

# number of inversions for each
def countInversions(grid, i, j, N, M):
  inversions = 0;
  lastTile = (N * M);
  tileValue = grid[i][j];
  if (tileValue>0):
    for k in range(N):
      for l in range(M):
        if (k*M + l > i*M + j):
          compValue = grid[k][l];
          if (compValue != 0):
            if (tileValue > compValue and tileValue != (lastTile)):
              inversions = inversions+1;

  return inversions;


# running sum of the inversions (for every title)
def sumInversions(grid, N, M):
  inversions = 0;
  for i in range(N):
    for j in range (M):
      inversions = inversions+ countInversions(grid, i, j, N, M)
  return inversions;


# return 1 if RANDOM puzzle is solvable ans 0 otherwise
def isSolvable(grid, N, M):
  inversions = sumInversions(grid, N, M);
  if (M & 1):
    if ((inversions % 2) == 0): return True
    else: return False


# calculate manhanttan function
def manhattan (N, M, grid, target):
    result = 0;
    for i in range (N):
      for j in range (M):
        if (target [i] [j] == 0):
          continue;
        for l in range (N):
          for m in range (M):
            if (target [i] [j] == grid [l] [m]):
              result += (abs (m - j) + abs (l - i));
              break;
    return (result);

# generate next states given the limits of the title
def getNextStates (N, M, current):
    nextStates = [];
    empty = None;
    dim = N*M;
    for i in range (dim):
        try:
            empty = current [i].index (0);
        except Exception as e:
            continue;
        empty = (i, empty);
        break

    # verify if going right is possible and if so append to list of next states
    if (empty [1] < (M - 1)):
        a = [ i.copy () for i in current ];
        a [empty [0]] [empty [1]], a [empty [0]] [empty [1] + 1] = a [empty [0]] [empty [1] + 1], a [empty [0]] [empty [1]];
        nextStates.append (('RIGHT', a, (empty [0], empty [1] + 1)));
    # verify if going left is possible and if so append to list of next states
    if (empty [1] > 0):
        b = [ i.copy () for i in current ];
        b [empty [0]] [empty [1]], b [empty [0]] [empty [1] - 1] = b [empty [0]] [empty [1] - 1], b [empty [0]] [empty [1]];
        nextStates.append (('LEFT', b, (empty [0], empty [1] - 1)));
    # verify if going up is possible and if so append to list of next states
    if (empty [0] > 0):
        c = [ i.copy () for i in current ];
        c [empty [0]] [empty [1]], c [empty [0] - 1] [empty [1]] = c [empty [0] - 1] [empty [1]], c [empty [0]] [empty [1]];
        nextStates.append (('UP', c, (empty [0] - 1, empty [1])));
    # verify if going down is possible and if so append to list of next states
    if (empty [0] < (N - 1)):
        d = [ i.copy () for i in current ];
        d [empty [0]] [empty [1]], d [empty [0] + 1] [empty [1]] = d [empty [0] + 1] [empty [1]], d [empty [0]] [empty [1]];
        nextStates.append (('DOWN', d, (empty [0] + 1, empty [1])));
    return (nextStates);

def a_star (N, M, grid):
    target = [ [j for j in range (i, i + M)] for i in range (0, (M * (N - 1)) + 1, M) ];
    current = (manhattan (N,M, grid, target) + sumInversions(grid, N, M), 0, [], grid)
    stateTree = [current];
    heapify (stateTree);
    while (not current [-1] == target):
        current = heappop (stateTree);
        for state in getNextStates (N, M, current [-1]):
            heappush (stateTree, (manhattan (N,M, state [1], target)  + sumInversions(grid, N, M) + current [1] + 1, current [1] + 1, current [2] + [state [0]], state [1]));

    return (current [1], current [3]);

if (__name__ == '__main__'):
    N = 4;
    M = 3;
    times = list()
    for i in range(30):
      grid = [ [j for j in range (i, i + M)] for i in range (0, (M * (N - 1)) + 1, M) ];
      dim = N*M-1;
      random.seed(datetime.now())
      pos_i = 0
      pos_j = 0
      a=1
      # controls the difficult of the puzzle
      while(a > 0.01):
        xj = pos_i
        yj = pos_j
        if (a > 0.5):
          while (abs(pos_i - xj) > 1 or pos_i == xj):
            xj = random.randint(0,N-1)
        else:
          while (abs(pos_j - yj) > 1 or pos_j == yj):
            yj = random.randint(0,M-1)
        temp = grid[pos_i][pos_j]
        grid[pos_i][pos_j] = grid[xj][yj]
        grid[xj][yj] = temp
        pos_i = xj
        pos_j = yj
        if(not isSolvable(grid, N, M)):
          grid = [ [j for j in range (i, i + M)] for i in range (0, (M * (N - 1)) + 1, M) ];
        else:
          a=random.random()
      # grid = [[1, 4, 3], [10, 7, 2], [8, 0, 9], [6, 11, 5]]
      print("Grid to be solved", grid)
      print(isSolvable(grid, N, M))

      start = time.time()

      print("solving...")
      seqCount, sequence = a_star (N, M, grid);
      print("solved! With", seqCount, "moves. Answer:", sequence)
      end = time.time()
      times.append(end - start)
    times_array = numpy.asarray(times)
    fig = plt.figure(1, figsize=(9, 6))

    # Create an axes instance
    fig, ax = plt.subplots()

    # Create the boxplot
    ax.boxplot(times_array)
    ax.set_ylim(-1,(max(times_array)+1)*2)
    fig.savefig('run.times_man_sum.png', bbox_inches='tight')

