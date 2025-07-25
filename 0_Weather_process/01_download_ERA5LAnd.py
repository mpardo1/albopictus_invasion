import cdsapi

c = cdsapi.Client()

months = ["01", "02", "03", "04", "05", "06", "07", "08", "09","10", "11", "12"]
years =  ["2024"]#["2004","2005","2006","2007","2008","2009","2010","2011","2012","2013",
          #"2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"]

for year in years:
    for month in months:
        print("Download year/month:", str(year) + "/" + str(month))
        dataset = "reanalysis-era5-land"
        request = {
            "variable": ["total_precipitation"],
            "year": year,
            "month": month,
            "day": [
                "01", "02", "03",
                "04", "05", "06",
                "07", "08", "09",
                "10", "11", "12",
                "13", "14", "15",
                "16", "17", "18",
                "19", "20", "21",
                "22", "23", "24",
                "25", "26", "27",
                "28", "29", "30",
                "31"
            ],
            "time": [
                "00:00", "01:00", "02:00",
                "03:00", "04:00", "05:00",
                "06:00", "07:00", "08:00",
                "09:00", "10:00", "11:00",
                "12:00", "13:00", "14:00",
                "15:00", "16:00", "17:00",
                "18:00", "19:00", "20:00",
                "21:00", "22:00", "23:00"
            ],
            "data_format": "grib",
            "download_format": "unarchived",
            "area": [80, -30, 30, 50]
        }

        target = "ERA5_EU_hourly_prec" + month +  year + ".grib"
        c.retrieve(dataset, request, target)

