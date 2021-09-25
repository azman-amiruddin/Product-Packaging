library(arules)
transaksi_tabular <- read.transactions(file="https://storage.googleapis.com/dqlab-dataset/transaksi_dqlab_retail.tsv", 
                                       format="single", sep="\t", cols=c(1,2), skip=1)
#Transaksi TOP 10
#mengecek frekuensi setiap item transaksi
data_item <- itemFrequency(transaksi_tabular, type = "absolute")
#mengurutkan data dari tertinggi ke terendah
data_item <- sort(data_item, decreasing = TRUE)
#mengambil 10 teransaksi teratas
data_item <- data_item[1:10]
#konversi data ke data frame
data_item <- data.frame("Nama.Produk" = names(data_item), 
                        "Jumlah" = data_item, row.names = NULL)
#menyimpan hasil dalam bentuk csv
write.csv(data_item, file = "top10_item_retail.txt")
print(data_item)

#Transaksi Bottom 10
#mengecek frekuensi setiap item transaksi
data_item <- itemFrequency(transaksi_tabular, type = "absolute")
#mengurutkan data dari terendah ke tertinggi
data_item <- sort(data_item, decreasing = FALSE)
#mengambil 10 teransaksi teratas
data_item <- data_item[1:10]
#konversi data ke data frame
data_item <- data.frame("Nama.Produk" = names(data_item), 
                        "Jumlah" = data_item, row.names = NULL)
#menyimpan hasil dalam bentuk csv
write.csv(data_item, file = "bottom10_item_retail.txt")
print(data_item)

#Kombinasi produk dengan korelasi yang kuat
kombinasi_retail <- apriori(transaksi_tabular, parameter=list(supp = 10/length(transaksi_tabular), 
                                                              confidence = 0.5,minlen=2,maxlen=3))
kombinasi_retail <- head(sort(kombinasi_retail, decreasing = TRUE, by='lift'), n=10)
write(kombinasi_retail, file="kombinasi_retail.txt")
print(kombinasi_retail)


#Mencari Paket Produk yang bisa dipasangkan dengan Item Slow-Moving

kombinasi_retail <- apriori(transaksi_tabular, parameter = list(supp=10/length(transaksi_tabular), confidence = 0.1, minlen= 2, maxlen = 3))

f1 <- subset(kombinasi_retail, rhs %in% "Tas Makeup")
f2 <- subset(kombinasi_retail, rhs %in% "Baju Renang Pria Anak-anak")
f1 <- sort(f1, by="lift")[1:3]
f2 <- sort(f2, by="lift") [1:3]
rules <- c(f1,f2)
inspect(rules)

write(rules, file="kombinasi_retail_slow_moving.txt")