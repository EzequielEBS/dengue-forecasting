import mosqlient

api_key = "EzequielEBS:c74e2486-70d0-454a-983b-a12d55376324"
dengue_rj = mosqlient.get_infodengue(
  api_key = api_key, 
  disease =  "dengue", 
  start_date = "2022-12-01", 
  end_date = "2026-02-19", 
  uf = "RJ",
  geocode = 3304557
)

dengue_joinville = mosqlient.get_infodengue(
  api_key = api_key, 
  disease =  "dengue", 
  start_date = "2019-12-01", 
  end_date = "2026-02-28", 
  uf = "SC",
  geocode = 4209102
)

climate_rj = mosqlient.get_climate(
    api_key = api_key,
    start_date = "2022-12-01",
    end_date = "2026-02-19", 
    uf = "RJ",
    geocode = 3304557
)

climate_joinville = mosqlient.get_climate(
    api_key = api_key,
    start_date = "2019-12-01",
    end_date = "2026-02-28",
    uf = "SC",
    geocode = 4209102
)

# saving the dataframes as csv files
dengue_rj.to_csv("data/rio_de_janeiro/dengue_rj.csv", index=False)
dengue_joinville.to_csv("data/joinville/dengue_joinville.csv", index=False)
climate_rj.to_csv("data/rio_de_janeiro/climate_rj.csv", index=False)
climate_joinville.to_csv("data/joinville/climate_joinville.csv", index=False)
