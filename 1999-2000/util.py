import operator


def remove_by_limits(hashmap, VALUE_KEY, minv, maxv):
    """
    """
    for k in hashmap.keys():
        try:
            value = float(hashmap[k][VALUE_KEY])
            if not (minv <= value <= maxv):
                hashmap.pop(k)
        except:
            hashmap.pop(k)
    return hashmap


def remove_if_not_in(hashmap, VALUE_KEY, values=[]):
    """
    If the value for a key in the hashmap does not have a VALUE_KEY value,
    remove from the hashmap.
    """

    for k in hashmap.keys():
        person = hashmap[k]
        value = person.get(VALUE_KEY)
        if value is None:
            hashmap.pop(k)
        elif value not in values:
            hashmap.pop(k)
    return hashmap


def remove_gender(hashmap, gender):
    """
    male is "1"; female is "2"
    """
    return remove_if_not_in(hashmap, [str(gender)])


def count(hashmap, VALUES):
    """
    VALUES like {'imq020': ['1'], 'mcq060': ['1']} got HBV and has ADD.
    """
    return len([
        k for k, v in hashmap.iteritems()
        if all((v.get(key) in val) for key, val in VALUES.items())
    ])


def test_kwargs(hashmap, only_test={}, float_limits={}, **kwargs):
    """
    Does simple chi-square p value for contingency table.

    only_test is like {"riadgendr": ["1"]} // only test males
    float_limits is like {"ridageyr": [3, 12]} // test only between 3 and 12
    """
    nhanes = hashmap.copy()
    for k, tup in only_test.items():
        remove_if_not_in(nhanes, k, tup)
    for k, tup in float_limits.items():
        remove_by_limits(nhanes, k, *tup)
    for k, tup in kwargs.items():
        remove_if_not_in(nhanes, k, reduce(operator.add, tup))
 
    l = len(kwargs.values()[0])
    # kwargs like (imq020=("1", "3"), mcq060=("1", "2"))

    # ["imq020", "mcq060"]
    keys = sorted(kwargs.keys())
    contingency_table = [
        [None for j in kwargs[keys[0]]]
        for i in kwargs
    ]
    # 2
    l = len(kwargs[keys[0]]) # all must be the same

    exposure = keys[1]
    print "\t%s in %s\t%s in %s" % (
        exposure, kwargs[exposure][0], exposure, kwargs[exposure][1]
    )
    outcome = keys[0]
    for i in range(l):
        row = "%s in %s\t" % (outcome, kwargs[outcome][i])
        for j in range(l):
            exposurek = keys[0]
            outcomek = keys[1]
            exposurev = kwargs[exposurek][i]
            outcomev = kwargs[outcomek][j]
            pairs = (exposurek, exposurev), (outcomek, outcomev)
            c = count(nhanes, dict(pairs))
            row += "%s\t" % c
            contingency_table[i][j] = c
        print row
    print

    p = computeContingencyTablePValue(*contingency_table)
    print p
    return contingency_table, p, nhanes


def computeContingencyTablePValue(*observedTuples):
    if len(observedTuples) == 0: return None

    rowSums = []

    for row in observedTuples:
        rowSums.append(float(sum(row)))

    columnSums = []
    for i in range(len(observedTuples[0])):
        columnSum = 0.0

        for row in observedTuples:
            columnSum += row[i]

        columnSums.append(float(columnSum))

    grandTotal = float(sum(rowSums))
    observedTestStatistic = 0.0

    for i in range(len(observedTuples)):
        for j in range(len(row)):
            expectedValue = (rowSums[i]/grandTotal)*(columnSums[j]/grandTotal)*grandTotal
            observedValue = float(observedTuples[i][j])
            observedTestStatistic += ((observedValue - expectedValue)**2) / expectedValue

    degreesFreedom = (len(columnSums) - 1) * (len(rowSums) - 1)

    from scipy.stats import chisqprob
    return chisqprob(observedTestStatistic, degreesFreedom)


