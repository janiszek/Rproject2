library(AzureML)

ws <- workspace(id = "19fa0face9694074a2dfbf665cc69942", auth = "qrFxReOHoRhJOduqBbs/xQzaqneaiZt6ntFJjG2J37LeeXTe1isYvUv1S16FaXNR+Ef0WClTmcf9WFUnsIRv7g==")

#wyswietla wszystkie datasety dostepne w workspace
datasets(ws, filter = c("all", "my datasets", "samples"))

#jesli chce zaladowac do AzureML z R
upload.dataset(Pizza, ws, "Pizza")

#upload.dataset(airquality, ws, "airquality")

#head(datasets(ws))

#delete.datasets(ws, "airquality")