import mosqlient

api_key = "EzequielEBS:c74e2486-70d0-454a-983b-a12d55376324"
start_date = "2022-12-01"
end_date = "2026-02-19"
uf = "RJ"
geocode = 3304557
dengue_rj = mosqlient.get_infodengue(
  api_key = api_key, 
  disease =  "dengue", 
  start_date = start_date, 
  end_date = end_date, 
  uf = uf,
  geocode = geocode
)

climate_rj = mosqlient.get_climate(
    api_key = api_key,
    start_date = start_date,
    end_date = end_date, 
    uf = uf,
    geocode = geocode
)

# saving the dataframes as csv files
dengue_rj.to_csv("data/rio_de_janeiro/dengue_rj.csv", index=False)
climate_rj.to_csv("data/rio_de_janeiro/climate_rj.csv", index=False)
