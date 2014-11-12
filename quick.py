'''QuickSort with comparison counting. Used to verify haskell solution.'''


def sortCount(l):
    return sortCountWith(fst, l)


def sortCountWith(f, l):
    length = len(l)
    if length == 0:
        return 0
    lessers, greaters = partition(set_pivot(f, l))
    print lessers, greaters
    return (length - 1) + sortCountWith(f, lessers) + sortCountWith(f, greaters)


def partition(l):
    if len(l) in (0, 1):
        return [], []

    pivot = l[0]
    i = 0
    for j, x in enumerate(l):
        if x <= pivot:
            swap(i, j, l)
            i += 1
    print 'g', l
    less = l[1:i]
    if less:
        less = swap(0, -1, less)
    return less, l[i:]


def set_pivot(f, l):
    i = f(l)
    swap(0, i, l)
    return l


def swap(i, j, l):
    l[i], l[j] = l[j], l[i]
    return l


def fst(l):
    return 0


def lst(l):
    return len(l) - 1


def median(l):
    if len(l) in (0, 1, 2):
        return 0
    middle = (len(l) + 1) / 2 - 1
    end = len(l) - 1
    i = (0, l[0])
    j = (middle, l[middle])
    k = (end, l[end])
    if i[1] <= j[1] <= k[1] or i[1] >= j[1] >= k[1]:
        return j[0]
    if j[1] <= k[1] <= i[1] or j[1] >= k[1] >= i[1]:
        return k[0]
    return i[0]
