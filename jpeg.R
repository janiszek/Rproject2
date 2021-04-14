library(jpeg)
library(FactoMineR)

photo <- readJPEG("dane/zz.jpg")

#scalamy 3D do jednej macierzy
photo <- photo[,,1]+photo[,,2]+photo[,,3]

#usredniamy
photo <- photo/max(photo)
#wyliczamy do 100 wymiarów
w<-PCA(photo, scale.unit = TRUE, ncp = 100, graph = FALSE)
p <- reconst(w)
pw<-writeJPEG(p)
image(p)

#95% zmiennosci calkowitej jest wyjasnione przez 10 komponentow, 
#ale to pogarsza jakosc obrazu
View(w$eig)

