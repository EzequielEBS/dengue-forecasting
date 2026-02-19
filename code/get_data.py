import mosqlient

api_key = "EzequielEBS:c74e2486-70d0-454a-983b-a12d55376324"
start_date = "2024-12-29"
end_date = "2026-02-19"
uf = "RJ"
geocode = 3304557
dengue_rj_from2025 = mosqlient.get_infodengue(
  api_key = api_key, 
  disease =  "dengue", 
  start_date = start_date, 
  end_date = end_date, 
  uf = uf,
  geocode = geocode
)

climate_rj_from2025 = mosqlient.get_climate(
    api_key = api_key,
    start_date = start_date,
    end_date = end_date, 
    uf = uf,
    geocode = geocode
)

# saving the dataframes as csv files
dengue_rj_from2025.to_csv("data/dengue_rj_from2025.csv", index=False)
climate_rj_from2025.to_csv("data/climate_rj_from2025.csv", index=False)
