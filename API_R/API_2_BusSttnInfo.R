library(XML)
library(RCurl)

# 국토교통부_버스정류소정보 (공공데이터 포털)
# https://www.data.go.kr/dataset/15000759/openapi.do
# 활용신청 클릭

url_2 <- 'http://openapi.tago.go.kr/openapi/service/BusSttnInfoInqireService'

op_21 <- '/getSttnNoList' # 정류소번호목록조회
op_22 <- '/getCrdntPrxmtSttnList' # 좌표기반근접정류소목록조회
op_23 <- '/getCtyCodeList' # 도시코드 목록 조회

key_2 <- paste0('?ServiceKey=','____') # 밑줄 부분에 서비스 키 입력

# Opearation 1: 정류소번호목록조회
cityCode=paste0('&cityCode=','25')
nodeNm=paste0('&nodeNm=','전통시장')
nodeNo=paste0('&nodeNo=','44810')
reqUrl_21 = paste0(url_2, op_21, key_2, cityCode, nodeNm, nodeNo)
reqUrl_21

pg_21=xmlParse(reqUrl_21)
pg_21
doc_21=xmlToDataFrame(pg_21,nodes = getNodeSet(pg_21, "//body/items/item"))
head(doc_21)


# Opearation 2: 좌표기반근접정류소목록조회
gpsLati=paste0('&gpsLati=','36.3')
gpsLong=paste0('&gpsLong=','127.3')
reqUrl_22 = paste0(url_2, op_22, key_2, gpsLati, gpsLong)
reqUrl_22

pg_22=xmlParse(reqUrl_22)
pg_22
doc_22=xmlToDataFrame(pg_22,nodes = getNodeSet(pg_22, "//body/items/item"))
head(doc_22)


# Opearation 3: 도시코드 목록 조회
reqUrl_23 = paste0(url_2, op_23, key_2)
reqUrl_23

pg_23=xmlParse(reqUrl_23)
doc_23=xmlToDataFrame(pg_23,nodes = getNodeSet(pg_23, "//body/items/item"))
head(doc_23)

