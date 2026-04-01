from tqdm.auto import tqdm
from datetime import timedelta
import pandas as pd
import time
import mosqlient

api_key = "EzequielEBS:c74e2486-70d0-454a-983b-a12d55376324"
dengue_rj = mosqlient.get_infodengue(
  api_key = api_key, 
  disease =  "dengue", 
  start_date = "2014-12-01", 
  end_date = "2026-02-28", 
  uf = "RJ",
  geocode = 3304557
)

dengue_joinville = mosqlient.get_infodengue(
  api_key = api_key, 
  disease =  "dengue", 
  start_date = "2014-12-01", 
  end_date = "2026-02-28", 
  uf = "SC",
  geocode = 4209102
)

dengue_sc = mosqlient.get_infodengue(
  api_key = api_key, 
  disease =  "dengue", 
  start_date = "2014-12-01", 
  end_date = "2026-02-28", 
  uf = "SC"
)

climate_rj = mosqlient.get_climate(
    api_key = api_key,
    start_date = "2014-12-01", 
  end_date = "2026-02-28", 
    uf = "RJ",
    geocode = 3304557
)

climate_joinville = mosqlient.get_climate(
    api_key = api_key,
    start_date = "2014-12-01",
    end_date = "2026-02-28",
    uf = "SC",
    geocode = 4209102
)

# climate_sc = mosqlient.get_climate(
#     api_key = api_key,
#     start_date = "2014-12-01",
#     end_date = "2026-02-28",
#     uf = "SC"
# )

# saving the dataframes as csv files
dengue_rj.to_csv("data/rio_de_janeiro/dengue_rj.csv", index=False)
dengue_joinville.to_csv("data/joinville/dengue_joinville.csv", index=False)
dengue_sc.to_csv("data/joinville/dengue_sc.csv", index=False)
climate_rj.to_csv("data/rio_de_janeiro/climate_rj.csv", index=False)
climate_joinville.to_csv("data/joinville/climate_joinville.csv", index=False)

def fetch_climate_split(api_key, start_date, end_date, uf, chunk_days=30, max_retries=3, backoff=2, show_progress=True):
    start = pd.to_datetime(start_date)
    end = pd.to_datetime(end_date)

    ranges = []
    cur = start
    while cur <= end:
        chunk_end = min(cur + pd.Timedelta(days=chunk_days - 1), end)
        ranges.append((cur.date().isoformat(), chunk_end.date().isoformat()))
        cur = chunk_end + pd.Timedelta(days=1)

    parts = []
    total = len(ranges)
    iterator = range(total)
    pbar = tqdm(iterator, desc=f"fetch_climate {uf}", unit="chunk") if show_progress else iterator

    try:
        for i in pbar:
            s, e = ranges[i]
            for attempt in range(1, max_retries + 1):
                try:
                    df = mosqlient.get_climate(api_key=api_key, start_date=s, end_date=e, uf=uf)
                    parts.append(df)
                    break
                except Exception as exc:
                    if attempt == max_retries:
                        # ensure progress bar closed before raising
                        if show_progress:
                            pbar.close()
                        raise
                    # show retry message without breaking the bar
                    if show_progress:
                        tqdm.write(f"Retry {attempt}/{max_retries} for {s} to {e}: {exc}")
                    time.sleep(backoff * attempt)
    finally:
        if show_progress:
            pbar.close()

    if parts:
        climate_sc = pd.concat(parts, ignore_index=True)
    else:
        climate_sc = pd.DataFrame()

    return climate_sc

climate_sc = fetch_climate_split(api_key, "2014-12-01", "2026-02-28", "SC")
climate_sc.to_csv("data/joinville/climate_sc.csv", index=False)
