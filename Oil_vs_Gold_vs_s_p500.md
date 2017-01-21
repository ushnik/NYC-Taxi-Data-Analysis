Oil vs Gold vs S&P500
================
Ushnik
November 18, 2016

Data
====

Loading data & exploration

``` r
gold <- read.csv("C:/Users/Ushnik/Downloads/Gold (2).csv", header=TRUE, stringsAsFactors=FALSE)
View(gold)
names(gold)
```

    ## [1] "Date"  "Value"

``` r
str(gold)
```

    ## 'data.frame':    322 obs. of  2 variables:
    ##  $ Date : chr  "1/1/1990" "2/1/1990" "3/1/1990" "4/1/1990" ...
    ##  $ Value: num  410 417 393 374 369 ...

``` r
head(gold)
```

    ##       Date Value
    ## 1 1/1/1990 410.1
    ## 2 2/1/1990 416.8
    ## 3 3/1/1990 393.1
    ## 4 4/1/1990 374.2
    ## 5 5/1/1990 369.1
    ## 6 6/1/1990 352.3

``` r
summary(gold)
```

    ##      Date               Value       
    ##  Length:322         Min.   : 256.1  
    ##  Class :character   1st Qu.: 344.3  
    ##  Mode  :character   Median : 392.6  
    ##                     Mean   : 673.8  
    ##                     3rd Qu.:1096.9  
    ##                     Max.   :1771.9

``` r
Date<-as.Date(gold$Date, "%m/%d/%Y")
gold$Date<-Date
str(gold)
```

    ## 'data.frame':    322 obs. of  2 variables:
    ##  $ Date : Date, format: "1990-01-01" "1990-02-01" ...
    ##  $ Value: num  410 417 393 374 369 ...

``` r
View(gold)

oil <- read.csv("C:/Users/Ushnik/Downloads/Oil (2).csv", header=TRUE, stringsAsFactors=FALSE)
View(oil)
names(oil)
```

    ## [1] "DATE"  "VALUE"

``` r
str(oil)
```

    ## 'data.frame':    322 obs. of  2 variables:
    ##  $ DATE : chr  "1/1/1990" "2/1/1990" "3/1/1990" "4/1/1990" ...
    ##  $ VALUE: num  22.9 22.1 20.4 18.4 18.2 ...

``` r
head(oil)
```

    ##       DATE VALUE
    ## 1 1/1/1990 22.86
    ## 2 2/1/1990 22.11
    ## 3 3/1/1990 20.39
    ## 4 4/1/1990 18.43
    ## 5 5/1/1990 18.20
    ## 6 6/1/1990 16.70

``` r
summary(oil)
```

    ##      DATE               VALUE       
    ##  Length:322         Min.   : 11.35  
    ##  Class :character   1st Qu.: 20.40  
    ##  Mode  :character   Median : 32.23  
    ##                     Mean   : 46.63  
    ##                     3rd Qu.: 71.02  
    ##                     Max.   :133.88

``` r
DATE<-as.Date(oil$DATE, "%m/%d/%Y")
oil$DATE<-DATE
str(oil)
```

    ## 'data.frame':    322 obs. of  2 variables:
    ##  $ DATE : Date, format: "1990-01-01" "1990-02-01" ...
    ##  $ VALUE: num  22.9 22.1 20.4 18.4 18.2 ...

``` r
View(oil)
```

The two data set has 322 observations with columns representing Date and Monthly Values of Gold and Oil in USD respectively.

Time Series Objects
===================

We then store both the data sets in two time series objects in R. To store the data in a time series object, we use the `ts()` function in R.

``` r
ts.gold<-ts(gold$Value,  start=c(1990,1), frequency = 12) 
ts.gold
```

    ##         Jan    Feb    Mar    Apr    May    Jun    Jul    Aug    Sep    Oct
    ## 1990  410.1  416.8  393.1  374.2  369.1  352.3  362.5  394.7  389.3  380.7
    ## 1991  383.6  363.8  363.3  358.4  357.0  366.7  367.7  356.3  348.7  358.7
    ## 1992  354.5  353.9  344.3  338.6  337.2  340.8  352.7  343.1  345.4  344.4
    ## 1993  329.0  329.3  330.1  342.2  367.2  371.9  392.2  378.8  355.3  364.2
    ## 1994  386.9  381.9  384.1  377.3  381.4  385.6  385.5  380.4  391.6  389.8
    ## 1995  378.6  376.6  382.1  391.0  385.2  387.6  386.2  383.7  383.1  383.1
    ## 1996  399.5  404.8  396.2  392.9  391.9  385.3  383.5  387.3  383.2  381.1
    ## 1997  355.1  346.6  351.8  344.5  343.8  340.8  324.1  324.0  322.8  324.9
    ## 1998  289.1  297.5  295.9  308.3  299.1  292.3  292.9  284.1  289.0  295.9
    ## 1999  287.1  287.3  286.0  282.6  276.4  261.3  256.1  256.7  264.7  310.7
    ## 2000  284.3  299.9  286.4  279.7  275.2  285.7  281.6  274.5  273.7  270.0
    ## 2001  265.5  261.9  263.0  260.5  272.4  270.2  267.5  272.4  283.4  283.1
    ## 2002  281.5  295.5  294.1  302.7  314.5  321.2  313.3  310.3  319.1  316.6
    ## 2003  356.9  359.0  340.6  328.2  355.7  356.4  351.0  359.8  379.0  378.9
    ## 2004  413.8  404.9  406.7  403.3  383.8  392.4  398.1  400.5  405.3  420.5
    ## 2005  424.0  423.4  434.3  429.2  421.9  430.7  424.5  437.9  456.1  469.9
    ## 2006  549.9  555.0  557.1  610.7  675.4  596.2  633.7  632.6  598.2  585.8
    ## 2007  631.2  664.7  654.9  679.4  666.9  655.5  665.3  665.4  712.7  754.6
    ## 2008  889.6  922.3  968.4  909.7  888.7  889.5  939.8  839.0  829.9  806.6
    ## 2009  858.7  943.2  924.3  890.2  928.6  945.7  934.2  949.4  996.6 1043.2
    ## 2010 1118.0 1095.4 1113.3 1148.7 1205.4 1232.9 1193.0 1215.8 1271.0 1342.0
    ## 2011 1356.4 1372.7 1424.0 1473.8 1510.4 1528.7 1572.8 1755.8 1771.9 1665.2
    ## 2012 1656.1 1742.6 1673.8 1650.1 1585.5 1596.7 1593.9 1626.0 1744.5 1747.0
    ## 2013 1671.0 1627.6 1592.9 1485.1 1413.5 1342.4 1286.7 1347.1 1348.8 1316.2
    ## 2014 1244.8 1301.0 1336.1 1299.0 1287.5 1279.1 1311.0 1296.0 1238.8 1222.5
    ## 2015 1251.9 1227.2 1178.6 1197.9 1199.1 1181.5 1130.0 1117.5 1124.5 1159.3
    ## 2016 1097.4 1199.9 1246.3 1242.3 1259.4 1276.4 1337.3 1341.1 1326.0 1266.6
    ##         Nov    Dec
    ## 1990  381.7  377.0
    ## 1991  360.2  361.7
    ## 1992  335.0  334.8
    ## 1993  373.8  383.3
    ## 1994  384.4  379.3
    ## 1995  385.3  387.4
    ## 1996  377.9  369.0
    ## 1997  306.0  288.7
    ## 1998  294.1  291.7
    ## 1999  293.2  283.1
    ## 2000  266.0  271.5
    ## 2001  276.2  275.9
    ## 2002  319.1  331.9
    ## 2003  389.9  407.0
    ## 2004  439.4  442.1
    ## 2005  476.7  510.1
    ## 2006  627.8  629.8
    ## 2007  806.3  803.2
    ## 2008  760.9  816.1
    ## 2009 1127.0 1134.7
    ## 2010 1369.9 1390.6
    ## 2011 1739.0 1652.3
    ## 2012 1721.1 1688.5
    ## 2013 1275.8 1225.4
    ## 2014 1176.3 1202.3
    ## 2015 1085.7 1068.3
    ## 2016

``` r
ts.oil<-ts(oil$VALUE, start=c(1990,1), frequency = 12) 
ts.oil
```

    ##         Jan    Feb    Mar    Apr    May    Jun    Jul    Aug    Sep    Oct
    ## 1990  22.86  22.11  20.39  18.43  18.20  16.70  18.45  27.31  33.51  36.04
    ## 1991  25.23  20.48  19.90  20.83  21.23  20.19  21.40  21.69  21.89  23.23
    ## 1992  18.79  19.01  18.92  20.23  20.98  22.38  21.78  21.34  21.88  21.69
    ## 1993  19.03  20.09  20.32  20.25  19.95  19.09  17.89  18.01  17.50  18.15
    ## 1994  15.03  14.78  14.68  16.42  17.89  19.06  19.65  18.38  17.45  17.72
    ## 1995  18.04  18.57  18.54  19.90  19.74  18.45  17.33  18.02  18.23  17.43
    ## 1996  18.85  19.09  21.33  23.50  21.17  20.42  21.30  21.90  23.97  24.88
    ## 1997  25.13  22.18  20.97  19.70  20.82  19.26  19.66  19.95  19.80  21.33
    ## 1998  16.72  16.06  15.12  15.35  14.91  13.72  14.17  13.47  15.03  14.46
    ## 1999  12.51  12.01  14.68  17.31  17.72  17.92  20.10  21.28  23.80  22.69
    ## 2000  27.26  29.37  29.84  25.72  28.79  31.82  29.70  31.26  33.88  33.11
    ## 2001  29.59  29.61  27.24  27.49  28.63  27.60  26.42  27.37  26.20  22.17
    ## 2002  19.71  20.72  24.53  26.18  27.04  25.52  26.97  28.39  29.66  28.84
    ## 2003  32.95  35.83  33.51  28.17  28.11  30.66  30.75  31.57  28.31  30.34
    ## 2004  34.31  34.68  36.74  36.75  40.28  38.03  40.78  44.90  45.94  53.28
    ## 2005  46.84  48.15  54.19  52.98  49.83  56.35  59.00  64.99  65.59  62.26
    ## 2006  65.49  61.63  62.69  69.44  70.84  70.95  74.41  73.04  63.80  58.89
    ## 2007  54.51  59.28  60.44  63.98  63.45  67.49  74.12  72.36  79.91  85.80
    ## 2008  92.97  95.39 105.45 112.58 125.40 133.88 133.37 116.67 104.11  76.61
    ## 2009  41.71  39.09  47.94  49.65  59.03  69.64  64.15  71.04  69.41  75.72
    ## 2010  78.33  76.39  81.20  84.29  73.74  75.34  76.32  76.60  75.24  81.89
    ## 2011  89.17  88.58 102.86 109.53 100.90  96.26  97.30  86.33  85.52  86.32
    ## 2012 100.27 102.20 106.16 103.32  94.65  82.30  87.90  94.13  94.51  89.49
    ## 2013  94.76  95.31  92.94  92.02  94.51  95.77 104.67 106.57 106.29 100.54
    ## 2014  94.62 100.82 100.80 102.07 102.18 105.79 103.59  96.54  93.21  84.40
    ## 2015  47.22  50.58  47.82  54.45  59.27  59.82  50.90  42.87  45.48  46.22
    ## 2016  31.68  30.32  37.55  40.76  46.71  48.76  44.65  44.72  45.18  49.78
    ##         Nov    Dec
    ## 1990  32.33  27.28
    ## 1991  22.46  19.50
    ## 1992  20.34  19.41
    ## 1993  16.61  14.51
    ## 1994  18.07  17.16
    ## 1995  17.99  19.03
    ## 1996  23.71  25.23
    ## 1997  20.19  18.33
    ## 1998  13.00  11.35
    ## 1999  25.00  26.10
    ## 2000  34.42  28.44
    ## 2001  19.64  19.39
    ## 2002  26.35  29.46
    ## 2003  31.11  32.13
    ## 2004  48.47  43.15
    ## 2005  58.32  59.41
    ## 2006  59.08  61.96
    ## 2007  94.77  91.69
    ## 2008  57.31  41.12
    ## 2009  77.99  74.47
    ## 2010  84.25  89.15
    ## 2011  97.16  98.56
    ## 2012  86.53  87.86
    ## 2013  93.86  97.63
    ## 2014  75.79  59.29
    ## 2015  42.44  37.19
    ## 2016

Time Series Plot
================

We then plot the price changes of Gold and Oil from 1990 until 2016

``` r
plot.ts(ts.gold, ylab= "Values of Gold")
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
plot.ts(ts.oil, ylab= "Values of Oil")
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-3-2.png)

Correlation between Gold and Oil
================================

``` r
par(mar=c(2,8,4,8)+0.1)
plot(ts.gold, col="blue", ylim=c(0,2000), axes=F, ylab="")
box()
axis(2, col="blue")
mtext("Value of Gold", side=2, line=3)
par(new=T)
plot(ts.oil, col="red", ylim=c(0,250), axes=F, ylab="")
axis(4, col="red")
mtext("Value of Oil", side=4, line=3)
axis(1, xlim=c(1990,2016))
mtext("Gold vs Oil values from 1990 to 2016",  line=3)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
cor(ts.gold, ts.oil)
```

    ## [1] 0.8219019

Therefore from the comparison above, we can see that there is a general upward trend for both, but Gold has a steeper rise than that of Oil. There is an overall positive correlation but there seems to be an inverse relationship of the value trends at times. For instance, in 2002 and 2009 Gold seems to have a decreasing value whereas Oil has an increasing value.

S&P500 data
===========

Loading and exploring

``` r
sp <- read.csv("C:/Users/Ushnik/Downloads/S&P500.csv", header=TRUE, stringsAsFactors=FALSE)
View(sp)
names(sp)
```

    ## [1] "Date"      "Open"      "High"      "Low"       "Close"     "Volume"   
    ## [7] "Adj.Close"

``` r
str(sp)
```

    ## 'data.frame':    322 obs. of  7 variables:
    ##  $ Date     : chr  "1/2/1990" "2/1/1990" "3/1/1990" "4/2/1990" ...
    ##  $ Open     : num  353 329 332 340 331 ...
    ##  $ High     : num  361 336 344 347 362 ...
    ##  $ Low      : num  320 322 331 328 331 ...
    ##  $ Close    : num  329 332 340 331 361 ...
    ##  $ Volume   : num  1.81e+08 1.66e+08 1.56e+08 1.46e+08 1.71e+08 ...
    ##  $ Adj.Close: num  329 332 340 331 361 ...

``` r
head(sp)
```

    ##       Date   Open   High    Low  Close    Volume Adj.Close
    ## 1 1/2/1990 353.40 360.59 319.83 329.08 181041300    329.08
    ## 2 2/1/1990 329.08 336.09 322.10 331.89 165598400    331.89
    ## 3 3/1/1990 331.89 344.49 331.08 339.94 155573600    339.94
    ## 4 4/2/1990 339.94 347.30 327.76 330.80 146198500    330.80
    ## 5 5/1/1990 330.80 362.26 330.80 361.23 171016800    361.23
    ## 6 6/1/1990 361.26 368.78 351.23 358.02 160561400    358.02

``` r
summary(sp)
```

    ##      Date                Open             High             Low        
    ##  Length:322         Min.   : 304.0   Min.   : 319.7   Min.   : 294.5  
    ##  Class :character   1st Qu.: 674.8   1st Qu.: 696.7   1st Qu.: 660.8  
    ##  Mode  :character   Median :1132.4   Median :1170.3   Median :1095.3  
    ##                     Mean   :1098.0   Mean   :1133.4   Mean   :1060.2  
    ##                     3rd Qu.:1378.4   3rd Qu.:1420.0   3rd Qu.:1325.3  
    ##                     Max.   :2173.1   Max.   :2193.8   Max.   :2147.6  
    ##      Close            Volume            Adj.Close     
    ##  Min.   : 304.0   Min.   :1.462e+08   Min.   : 304.0  
    ##  1st Qu.: 691.8   1st Qu.:4.440e+08   1st Qu.: 691.8  
    ##  Median :1133.7   Median :1.491e+09   Median :1133.7  
    ##  Mean   :1103.4   Mean   :2.128e+09   Mean   :1103.4  
    ##  3rd Qu.:1379.1   3rd Qu.:3.666e+09   3rd Qu.:1379.1  
    ##  Max.   :2173.6   Max.   :7.633e+09   Max.   :2173.6

``` r
Date<-as.Date(sp$Date, "%m/%d/%Y")
sp$Date<-Date
str(sp)
```

    ## 'data.frame':    322 obs. of  7 variables:
    ##  $ Date     : Date, format: "1990-01-02" "1990-02-01" ...
    ##  $ Open     : num  353 329 332 340 331 ...
    ##  $ High     : num  361 336 344 347 362 ...
    ##  $ Low      : num  320 322 331 328 331 ...
    ##  $ Close    : num  329 332 340 331 361 ...
    ##  $ Volume   : num  1.81e+08 1.66e+08 1.56e+08 1.46e+08 1.71e+08 ...
    ##  $ Adj.Close: num  329 332 340 331 361 ...

``` r
View(sp)
```

Time Series Objects
===================

We then store the S&P500 data set in a time series object in R. To store the data in a time series object, we use the `ts()` function in R.

``` r
ts.sp<-ts(sp$Open,  start=c(1990,1), frequency = 12) 
ts.sp
```

    ##          Jan     Feb     Mar     Apr     May     Jun     Jul     Aug
    ## 1990  353.40  329.08  331.89  339.94  330.80  361.26  358.02  356.15
    ## 1991  330.20  343.91  367.07  375.22  375.35  389.81  371.18  387.81
    ## 1992  417.03  408.79  412.68  403.67  414.95  415.35  408.20  424.19
    ## 1993  435.70  438.78  443.38  451.67  440.19  450.23  450.54  448.13
    ## 1994  466.51  481.60  467.19  445.66  450.91  456.50  444.27  458.28
    ## 1995  459.21  470.42  487.39  500.70  514.76  533.40  544.75  562.06
    ## 1996  615.93  636.02  640.43  645.50  654.17  669.12  670.63  639.95
    ## 1997  740.74  786.16  790.82  757.12  801.34  848.28  885.14  954.29
    ## 1998  970.43  980.28 1049.34 1101.75 1111.75 1090.82 1133.84 1120.67
    ## 1999 1229.23 1279.64 1238.33 1286.37 1335.18 1301.84 1372.71 1328.72
    ## 2000 1469.25 1394.46 1366.42 1498.58 1452.43 1420.60 1454.60 1430.83
    ## 2001 1320.28 1366.01 1239.94 1160.33 1249.46 1255.82 1224.42 1211.23
    ## 2002 1148.08 1130.20 1106.73 1147.39 1076.92 1067.14  989.82  911.62
    ## 2003  879.82  855.70  841.15  848.18  916.92  963.59  974.50  990.31
    ## 2004 1111.92 1131.13 1144.94 1126.21 1107.30 1120.68 1140.84 1101.72
    ## 2005 1211.92 1181.27 1203.60 1180.59 1156.85 1191.50 1191.33 1234.18
    ## 2006 1248.29 1280.08 1280.66 1302.88 1310.61 1270.05 1270.06 1278.53
    ## 2007 1418.03 1437.90 1406.80 1420.83 1482.37 1530.62 1504.66 1455.18
    ## 2008 1467.97 1378.60 1330.45 1326.41 1385.97 1399.62 1276.69 1269.42
    ## 2009  902.99  823.09  729.57  793.59  872.74  923.26  920.82  990.22
    ## 2010 1116.56 1073.89 1105.36 1171.23 1188.58 1087.30 1031.10 1107.53
    ## 2011 1257.62 1289.14 1328.64 1329.48 1365.21 1345.20 1320.64 1292.59
    ## 2012 1258.86 1312.45 1365.90 1408.47 1397.86 1309.87 1362.33 1379.32
    ## 2013 1426.19 1498.11 1514.68 1569.18 1597.55 1631.71 1609.78 1689.42
    ## 2014 1845.86 1782.68 1857.68 1873.96 1884.39 1923.87 1962.29 1929.80
    ## 2015 2058.90 1996.67 2105.23 2067.63 2087.38 2108.64 2067.00 2104.49
    ## 2016 2038.20 1936.94 1937.09 2056.62 2067.17 2093.94 2099.34 2173.15
    ##          Sep     Oct     Nov     Dec
    ## 1990  322.56  306.10  303.99  322.23
    ## 1991  395.43  387.86  392.46  375.11
    ## 1992  414.03  417.80  418.66  431.35
    ## 1993  463.55  458.93  467.83  461.93
    ## 1994  475.49  462.69  472.26  453.55
    ## 1995  561.88  584.41  581.50  605.37
    ## 1996  651.99  687.31  705.27  757.02
    ## 1997  899.47  947.28  914.62  955.40
    ## 1998  957.28 1017.01 1098.67 1163.63
    ## 1999 1320.41 1282.71 1362.93 1388.91
    ## 2000 1517.68 1436.52 1429.40 1314.95
    ## 2001 1133.58 1040.94 1059.78 1139.45
    ## 2002  916.07  815.28  885.76  936.31
    ## 2003 1008.01  995.97 1050.71 1058.20
    ## 2004 1104.24 1114.58 1130.20 1173.78
    ## 2005 1220.33 1228.81 1207.01 1249.48
    ## 2006 1303.80 1335.82 1377.76 1400.63
    ## 2007 1473.96 1527.29 1545.79 1479.63
    ## 2008 1287.83 1164.17  968.67  888.61
    ## 2009 1019.52 1054.91 1036.18 1098.89
    ## 2010 1049.72 1143.49 1185.71 1186.60
    ## 2011 1219.12 1131.21 1251.00 1246.91
    ## 2012 1406.54 1440.90 1412.20 1416.34
    ## 2013 1635.95 1682.41 1758.70 1806.55
    ## 2014 2004.07 1971.44 2018.21 2065.78
    ## 2015 1970.09 1919.65 2080.76 2082.93
    ## 2016 2171.33 2164.33

Time Series Plot
================

We then plot the S&P500 values from 1990 until 2016

``` r
plot.ts(ts.sp, ylab= "S&P500 Values")
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-7-1.png)

Decomposing, Holt Smoothening and forcasting for Gold, Oil and S&P500
=====================================================================

``` r
library(forecast)
```

    ## Warning: package 'forecast' was built under R version 3.3.2

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 3.3.2

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: timeDate

    ## Warning: package 'timeDate' was built under R version 3.3.2

    ## This is forecast 7.3

    ## 
    ## Attaching package: 'forecast'

    ## The following object is masked _by_ '.GlobalEnv':
    ## 
    ##     gold

``` r
ts.gold.d <- decompose(ts.gold)
plot(ts.gold.d)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ts.gold.holt <- HoltWinters(ts.gold, gamma=TRUE)
plot(ts.gold.holt, ylab="Gold Values", xlab="Year")
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-8-2.png)

``` r
ts.gold.forecasts <- forecast.HoltWinters(ts.gold.holt, h=14)  
ts.gold.forecasts
```

    ##          Point Forecast    Lo 80    Hi 80     Lo 95    Hi 95
    ## Nov 2016       1268.054 1225.857 1310.251 1203.5196 1332.589
    ## Dec 2016       1265.386 1206.135 1324.637 1174.7690 1356.003
    ## Jan 2017       1274.440 1200.985 1347.895 1162.1002 1386.780
    ## Feb 2017       1279.063 1192.796 1365.330 1147.1293 1410.997
    ## Mar 2017       1260.586 1162.319 1358.853 1110.2997 1410.872
    ## Apr 2017       1246.917 1137.165 1356.669 1079.0658 1414.768
    ## May 2017       1248.752 1127.856 1369.648 1063.8575 1433.647
    ## Jun 2017       1248.922 1117.111 1380.733 1047.3345 1450.510
    ## Jul 2017       1250.799 1108.226 1393.372 1032.7523 1468.846
    ## Aug 2017       1261.130 1107.894 1414.365 1026.7758 1495.483
    ## Sep 2017       1268.686 1104.847 1432.524 1018.1161 1519.255
    ## Oct 2017       1264.817 1090.405 1439.229  998.0770 1531.557
    ## Nov 2017       1266.271 1080.472 1452.071  982.1152 1550.427
    ## Dec 2017       1263.603 1067.270 1459.936  963.3374 1563.869

We would therefore want to invest in gold in April 2017 when the point forecast is $1246.917 and sell gold in Sep 2017 at $1268.686. The forecast above lies within a 80% and 95% confidence level intervals as indicated above.

We plot the forecast of gold by usinn the `plot.forecast` function:

``` r
plot.forecast(ts.gold.forecasts, ylab="Gold Values", xlab="Year")
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
ts.oil.d <- decompose(ts.oil)
plot(ts.oil.d)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
ts.oil.holt <- HoltWinters(ts.oil, gamma=TRUE)
plot(ts.oil.holt, ylab="Oil Values", xlab="Year")
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-10-2.png)

``` r
ts.oil.forecasts <- forecast.HoltWinters(ts.oil.holt, h=14)  
ts.oil.forecasts
```

    ##          Point Forecast    Lo 80    Hi 80     Lo 95    Hi 95
    ## Nov 2016       45.90922 38.50265 53.31578 34.581855 57.23658
    ## Dec 2016       45.25659 35.78074 54.73244 30.764534 59.74865
    ## Jan 2017       46.78734 35.61926 57.95543 29.707230 63.86746
    ## Feb 2017       52.54851 39.91282 65.18420 33.223888 71.87312
    ## Mar 2017       57.32470 43.37497 71.27444 35.990424 78.65898
    ## Apr 2017       60.26042 45.11018 75.41065 37.090134 83.43070
    ## May 2017       59.75044 43.48809 76.01279 34.879321 84.62156
    ## Jun 2017       56.79770 39.49457 74.10084 30.334838 83.26057
    ## Jul 2017       53.43384 35.14907 71.71862 25.469694 81.39799
    ## Aug 2017       51.03212 31.81579 70.24845 21.643275 80.42097
    ## Sep 2017       49.98313 29.87836 70.08791 19.235533 80.73073
    ## Oct 2017       47.50486 26.54928 68.46044 15.456063 79.55366
    ## Nov 2017       43.63408 21.40811 65.86005  9.642395 77.62576
    ## Dec 2017       42.98145 19.98301 65.97989  7.808377 78.15453

We would therefore want to invest in oil in Jan 2017 when the point forecast is $46.78 and sell oil in Apr 2017 at $60.26. The forecast above lies within a 80% and 95% confidence level intervals as indicated above.

We plot the forecast of gold by usinn the `plot.forecast` function:

``` r
plot.forecast(ts.oil.forecasts, ylab="Oil Values", xlab="Year")
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
ts.sp.d <- decompose(ts.sp)
plot(ts.sp.d)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
ts.sp.holt <- HoltWinters(ts.sp, gamma=TRUE)
plot(ts.sp.holt, ylab="S&P500 Values", xlab="Year")
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-12-2.png)

``` r
ts.sp.forecasts <- forecast.HoltWinters(ts.sp.holt, h=14) 
ts.sp.forecasts
```

    ##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## Nov 2016       2173.259 2111.134 2235.385 2078.247 2268.272
    ## Dec 2016       2171.768 2085.641 2257.895 2040.048 2303.488
    ## Jan 2017       2181.598 2076.831 2286.365 2021.370 2341.826
    ## Feb 2017       2178.730 2058.171 2299.289 1994.351 2363.109
    ## Mar 2017       2202.175 2067.666 2336.685 1996.461 2407.890
    ## Apr 2017       2223.679 2076.536 2370.822 1998.643 2448.715
    ## May 2017       2231.856 2073.081 2390.631 1989.031 2474.681
    ## Jun 2017       2229.472 2059.862 2399.083 1970.075 2488.869
    ## Jul 2017       2227.815 2048.021 2407.610 1952.843 2502.787
    ## Aug 2017       2243.395 2053.963 2432.826 1953.684 2533.105
    ## Sep 2017       2220.808 2022.206 2419.409 1917.072 2524.543
    ## Oct 2017       2205.367 1998.001 2412.734 1888.227 2522.507
    ## Nov 2017       2214.296 1997.824 2430.769 1883.230 2545.363
    ## Dec 2017       2212.805 1988.264 2437.347 1869.399 2556.212

We would therefore want to invest in S&P500 stocks in Jan 2017 when the point forecast is $2181.59 and sell the stocks in Sep 2017 at $2220.8. The forecast above lies within a 80% and 95% confidence level intervals as indicated above.

We plot the forecast of gold by usinn the `plot.forecast` function:

``` r
plot.forecast(ts.sp.forecasts, ylab="S&P500 Values", xlab="Year")
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-13-1.png)

Arima model for Gold
====================

We use the Arima model to further consolidate our Forecast Models achieved as above. We use the `auto.arima` function to predict the (p,d,q) variables and use the returned values for an optimum forecast model.

``` r
auto.arima(ts.gold)
```

    ## Series: ts.gold 
    ## ARIMA(0,1,1)                    
    ## 
    ## Coefficients:
    ##          ma1
    ##       0.2071
    ## s.e.  0.0576
    ## 
    ## sigma^2 estimated as 944.4:  log likelihood=-1554.51
    ## AIC=3113.02   AICc=3113.05   BIC=3120.56

``` r
gold.arima<-arima(ts.gold, c(0,1,1))   
gold.arima.forecasts <- forecast.Arima(gold.arima, h=14)
gold.arima.forecasts
```

    ##          Point Forecast    Lo 80    Hi 80     Lo 95    Hi 95
    ## Nov 2016       1254.873 1215.551 1294.194 1194.7357 1315.010
    ## Dec 2016       1254.873 1193.236 1316.509 1160.6073 1349.138
    ## Jan 2017       1254.873 1177.078 1332.667 1135.8960 1373.849
    ## Feb 2017       1254.873 1163.741 1346.004 1115.4993 1394.246
    ## Mar 2017       1254.873 1152.121 1357.624 1097.7280 1412.017
    ## Apr 2017       1254.873 1141.688 1368.057 1081.7717 1427.974
    ## May 2017       1254.873 1132.139 1377.607 1067.1670 1442.578
    ## Jun 2017       1254.873 1123.280 1386.465 1053.6193 1456.126
    ## Jul 2017       1254.873 1114.982 1394.764 1040.9278 1468.818
    ## Aug 2017       1254.873 1107.149 1402.597 1028.9481 1480.797
    ## Sep 2017       1254.873 1099.710 1410.035 1017.5724 1492.173
    ## Oct 2017       1254.873 1092.613 1417.132 1006.7177 1503.028
    ## Nov 2017       1254.873 1085.813 1423.932  996.3182 1513.427
    ## Dec 2017       1254.873 1079.276 1430.469  986.3212 1523.424

``` r
plot(gold.arima.forecasts)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-14-1.png)

The ??forecast errors?? are calculated as the observed values minus predicted values, for each time point. We can only calculate the forecast errors for the time period covered by our original time series, which is 1990 to 2016 for the exchange rate data. As mentioned above, one measure of the accuracy of the predictive model is the sum-of-squared-errors (SSE) for the in-sample forecast errors.

The in-sample forecast errors are stored in the named element ??residuals?? of the list variable returned by `forecast.HoltWinters()`. If the predictive model cannot be improved upon, there should be no correlations between forecast errors for successive predictions. In other words, if there are correlations between forecast errors for successive predictions, it is likely that the simple exponential smoothing forecasts could be improved upon by another forecasting technique.

To figure out whether this is the case, we can obtain a correlogram of the in-sample forecast errors for lags 1-20. We can calculate a correlogram of the forecast errors using the `acf()` function in R. To specify the maximum lag that we want to look at, we use the `lag.max` parameter in `acf()`.

For example, to calculate a correlogram of the forecast errors for the values of Gold for lags 1-20, we type:

``` r
acf(gold.arima.forecasts$residuals, lag.max=20)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-15-1.png)

We can see from the correlogram that the autocorrelation at lag 4 and lag 9 are outside the significance bounds and lag 7 is just touching the significance bounds. To test whether there is significant evidence for non-zero correlations at lags 1-20, we can carry out a Ljung-Box test. This can be done in R using the `Box.test()` function. The maximum lag that we want to look at is specified using the ??lag?? parameter in the `Box.test()` function. For example, to test whether there are non-zero autocorrelations at lags 1-20, for the in-sample forecast errors for exchange rate data, we type:

``` r
Box.test(gold.arima.forecasts$residuals, lag=20, type="Ljung-Box")
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  gold.arima.forecasts$residuals
    ## X-squared = 32.274, df = 20, p-value = 0.04046

Here the Ljung-Box test statistic is 32.2, and the p-value is 0.04, so there is little evidence of non-zero autocorrelations in the in-sample forecast errors at lags 1-20.

To be sure that the predictive model cannot be improved upon, it is also a good idea to check whether the forecast errors are normally distributed with mean zero and constant variance. To check whether the forecast errors have constant variance, we can make a time plot of the in-sample forecast errors:

``` r
plot.ts(gold.arima.forecasts$residuals)            # make time plot of forecast errors
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-17-1.png)

The plot shows that the in-sample forecast errors do not seem to have a constant variance over time, although the size of the fluctuations in the start of the time series may be slightly less than that at later dates.

We therefore difference the given forecast once to obtain constant variance over time. This is also supported by the `auto.arima` function predicting a integrated variable "d" as 1.

``` r
gold.diff <- diff(gold.arima.forecasts$residuals, differences = 1)
plot.ts(gold.diff)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-18-1.png)

To check whether the forecast errors are normally distributed with mean zero, we can plot a histogram of the forecast errors, with an overlaid normal curve that has mean zero and the same standard deviation as the distribution of forecast errors. To do this, we can define an R function ??plotForecastErrors()??, below:

``` r
PlotForecastErrors <- function(forecasterrors)
{
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors)-mysd*5
  mymax <- max(forecasterrors)+mysd*5
  mynorm <- rnorm(10000,mean=0,sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2<mymin)
  {
    mymin<-mymin2
  }
  if (mymax2>mymax)
  {
    mymax<-mymax2
  }
  mybins <- seq(mymin,mymax,mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

PlotForecastErrors(gold.arima.forecasts$residuals)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-19-1.png)

The plot shows that the distribution of forecast errors is roughly centred on zero, and is more or less normally distributed and so it is plausible that the forecast errors are normally distributed with mean zero.

The Ljung-Box test showed that there is little evidence of non-zero autocorrelations in the in-sample forecast errors, and the distribution of forecast errors seems to be normally distributed with mean zero. This suggests that the simple exponential smoothing method provides an adequate predictive model for the exchange rates, which probably cannot be improved upon. Furthermore, the assumptions that the 80% and 95% predictions intervals were based upon (that there are no autocorrelations in the forecast errors, and the forecast errors are normally distributed with mean zero and constant variance) are probably valid.

Arima model forecast for Oil
============================

We carry out the same procedure to achieve forecast values using the Arima model and check for forecast errors.

``` r
auto.arima(ts.oil)
```

    ## Series: ts.oil 
    ## ARIMA(1,1,0)(0,0,2)[12]                    
    ## 
    ## Coefficients:
    ##          ar1    sma1     sma2
    ##       0.4332  0.0927  -0.1647
    ## s.e.  0.0514  0.0567   0.0599
    ## 
    ## sigma^2 estimated as 17.16:  log likelihood=-910.71
    ## AIC=1829.41   AICc=1829.54   BIC=1844.5

``` r
oil.arima<-arima(ts.oil, c(1,1,0))   
oil.arima.forecasts <- forecast.Arima(oil.arima, h=14)
oil.arima.forecasts
```

    ##          Point Forecast    Lo 80    Hi 80     Lo 95     Hi 95
    ## Nov 2016       51.61879 46.23785 56.99972 43.389360  59.84822
    ## Dec 2016       52.35382 43.09726 61.61038 38.197139  66.51050
    ## Jan 2017       52.64764 40.15346 65.14182 33.539437  71.75584
    ## Feb 2017       52.76509 37.52003 68.01015 29.449779  76.08040
    ## Mar 2017       52.81204 35.17292 70.45116 25.835337  79.78874
    ## Apr 2017       52.83081 33.06107 72.60054 22.595600  83.06601
    ## May 2017       52.83831 31.13718 74.53944 19.649289  86.02733
    ## Jun 2017       52.84131 29.36381 76.31880 16.935572  88.74704
    ## Jul 2017       52.84251 27.71265 77.97236 14.409712  91.27530
    ## Aug 2017       52.84298 26.16244 79.52353 12.038619  93.64735
    ## Sep 2017       52.84318 24.69707 80.98928  9.797428  95.88892
    ## Oct 2017       52.84325 23.30415 82.38236  7.667086  98.01942
    ## Nov 2017       52.84328 21.97394 83.71263  5.632699 100.05387
    ## Dec 2017       52.84330 20.69871 84.98788  3.682395 102.00420

``` r
plot(oil.arima.forecasts)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
acf(oil.arima.forecasts$residuals, lag.max=20)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-20-2.png)

``` r
Box.test(oil.arima.forecasts$residuals, lag=20, type="Ljung-Box")
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  oil.arima.forecasts$residuals
    ## X-squared = 24.055, df = 20, p-value = 0.24

``` r
plot.ts(oil.arima.forecasts$residuals)            # make time plot of forecast errors
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-20-3.png)

``` r
oil.diff <- diff(oil.arima.forecasts$residuals, differences = 1)
plot.ts(oil.diff)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-20-4.png)

``` r
PlotForecastErrors(oil.arima.forecasts$residuals)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-20-5.png)

``` r
mean(oil.arima.forecasts$residuals)
```

    ## [1] 0.05615918

The plot shows that the distribution of forecast errors is roughly centred on zero, and is more or less normally distributed and so it is plausible that the forecast errors are normally distributed with mean zero.

The Ljung-Box test showed that there is little evidence of non-zero autocorrelations in the in-sample forecast errors, and the distribution of forecast errors seems to be normally distributed with mean zero. This suggests that the simple exponential smoothing method provides an adequate predictive model for the exchange rates, which probably cannot be improved upon. Furthermore, the assumptions that the 80% and 95% predictions intervals were based upon (that there are no autocorrelations in the forecast errors, and the forecast errors are normally distributed with mean zero and constant variance) are probably valid.

Arima model forecast for S&P500
===============================

Similarly, we forecast values for S&P500 and check for forecast errors

``` r
auto.arima(ts.sp)
```

    ## Series: ts.sp 
    ## ARIMA(0,1,0) with drift         
    ## 
    ## Coefficients:
    ##        drift
    ##       5.6415
    ## s.e.  2.6326
    ## 
    ## sigma^2 estimated as 2232:  log likelihood=-1692.51
    ## AIC=3389.02   AICc=3389.06   BIC=3396.57

``` r
sp.arima<-arima(ts.sp, c(0,1,0))   
sp.arima.forecasts <- forecast.Arima(sp.arima, h=14)
sp.arima.forecasts
```

    ##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## Nov 2016        2164.33 2103.453 2225.207 2071.227 2257.433
    ## Dec 2016        2164.33 2078.237 2250.423 2032.662 2295.998
    ## Jan 2017        2164.33 2058.888 2269.772 2003.070 2325.590
    ## Feb 2017        2164.33 2042.576 2286.084 1978.123 2350.537
    ## Mar 2017        2164.33 2028.205 2300.455 1956.145 2372.516
    ## Apr 2017        2164.33 2015.212 2313.448 1936.274 2392.386
    ## May 2017        2164.33 2003.265 2325.396 1918.002 2410.659
    ## Jun 2017        2164.33 1992.144 2336.516 1900.994 2427.666
    ## Jul 2017        2164.33 1981.699 2346.961 1885.020 2443.640
    ## Aug 2017        2164.33 1971.820 2356.840 1869.911 2458.749
    ## Sep 2017        2164.33 1962.424 2366.236 1855.541 2473.119
    ## Oct 2017        2164.33 1953.446 2375.214 1841.810 2486.850
    ## Nov 2017        2164.33 1944.835 2383.825 1828.641 2500.019
    ## Dec 2017        2164.33 1936.549 2392.111 1815.969 2512.691

``` r
plot(sp.arima.forecasts)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
acf(sp.arima.forecasts$residuals, lag.max=20)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-21-2.png)

``` r
Box.test(sp.arima.forecasts$residuals, lag=20, type="Ljung-Box")
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  sp.arima.forecasts$residuals
    ## X-squared = 20.791, df = 20, p-value = 0.4095

``` r
plot.ts(sp.arima.forecasts$residuals)            # make time plot of forecast errors
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-21-3.png)

``` r
sp.diff <- diff(sp.arima.forecasts$residuals, differences = 1)
plot.ts(sp.diff)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-21-4.png)

``` r
PlotForecastErrors(sp.arima.forecasts$residuals)
```

![](Oil_vs_Gold_vs_s_p500_files/figure-markdown_github/unnamed-chunk-21-5.png)

The plot shows that the distribution of forecast errors is roughly centred on zero, and is more or less normally distributed and so it is plausible that the forecast errors are normally distributed with mean zero.

The Ljung-Box test showed that there is little evidence of non-zero autocorrelations in the in-sample forecast errors, and the distribution of forecast errors seems to be normally distributed with mean zero. This suggests that the simple exponential smoothing method provides an adequate predictive model for the exchange rates, which probably cannot be improved upon. Furthermore, the assumptions that the 80% and 95% predictions intervals were based upon (that there are no autocorrelations in the forecast errors, and the forecast errors are normally distributed with mean zero and constant variance) are probably valid.

Interpretations:
================

### 1. Gold

From the forecast plots, we see that investing in Gold in April 2017 ($1246.917) would yield a profit of 1.75% when sold in Sep 2017($1268.686).

These values lie at a confidence level of:

#### Buying price:

`Holt's Forecasting`:

80%: low of $1137.165; high of $1356.669
95%: low of $1079.0658; high of $1414.768

`Arima model forecasting`:

80%: low of $1141.688; high of $1368.057
95%: low of $1081.7717; high of $1427.974

#### Selling Price:

`Holt's Forecasting`:

80%: low of $1104.847; high of $1432.524
95%: low of $1018.1161; high of $1519.255

`Arima model forecasting`:

80%: low of $1085.813; high of $1423.932
95%: low of $996.3182; high of $1513.427

### 2. Oil

Similarly, we see that investing in Oil in January 2017 ($46.78) would yield a profit of 28.8% when sold in Apr 2017($60.26)

These values lie at a confidence level of:

#### Buying price:

`Holt's Forecasting:`

80%: low of $35.61926; high of $57.95543
95%: low of $29.707230; high of $63.86746

`Arima model forecasting:`

80%: low of $40.15346; high of $65.14182
95%: low of $33.539437; high of $71.75584

#### Selling Price:

`Holt's Forecasting`:

80%: low of $45.11018; high of $75.41065
95%: low of $37.090134; high of $83.43070

`Arima model forecasting`:

80%: low of $33.06107; high of $72.60054
95%: low of $22.595600; high of $83.06601

### 3. S&P500

We also see that investing in S&P500 in January 2017 ($2181.59) would yield a profit of 1.8% when sold in Sep 2017($2220.808)

#### Buying Price:

`Holt's Forecasting`:

80%: low of $2076.831; high of $2286.365
95%: low of $2021.370; high of $2341.826

`Arima model forecasting`:

80%: low of $2058.888; high of $2269.772
95%: low of $2003.07; high of $2325.590

#### Selling Price:

`Holt's Forecasting`:

80%: low of $2022.206; high of $2419.409
95%: low of $1917.072; high of $2524.543

`Arima model forecasting`:

80%: low of $1962.424; high of $2366.236
95%: low of $1855.541; high of $2473.119

#### Conclusion:

We use the arima model confidnce bands as a primary variation indicator as it removes the trend and seasonal components and works with the irregular component (noise) to model the forecasts. This, I feel, is more important in investing activities since it is safer to know what the maximum loss might be, incase of irregularities.

Therefore, within 80% and 95% confidence levels as mentioned above we should look to invest in Oil in January 2017 and sell our stocks in April 2017 to earn a profit of 28.8%
