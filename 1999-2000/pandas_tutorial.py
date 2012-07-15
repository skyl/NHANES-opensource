import pandas

from functools import partial

loadcsv = partial(pandas.DataFrame.from_csv,
                  index_col="seqn", parse_dates=False)


demoq = loadcsv("csv/DEMO.csv")
rxq = loadcsv("csv/quest/RXQ_RX.csv")




