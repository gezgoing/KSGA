# 출처: https://poiopzxcv.tistory.com/17 [잡동사니 상자]

library(XML)
library(RCurl)

# 국토교통부_버스노선정보 (공공데이터 포털)
# https://www.data.go.kr/dataset/15000758/openapi.do
# 활용신청 클릭

# http:// [api url]/오퍼레이션명?서비스키&페이지당 노출 수&페이지 넘버

url_1 <- 'http://openapi.tago.go.kr/openapi/service/BusRouteInfoInqireService'

op_11 <- '/getRouteNoList' # 노선번호목록조회
op_12 <- '/getRouteAcctoThrghSttnList' # 노선별경유정류소목록조회
op_13 <- '/getRouteInfoIem' # 노선정보항목조회
op_14 <- '/getCtyCodeList' # 도시코드 목록 조회

key_1 <- paste0('?ServiceKey=','____') # 밑줄 부분에 서비스 키 입력

# Opearation 1: 노선번호목록조회
cityCode=paste0('&cityCode=','25')
routeNo=paste0('&routeId=','5')
reqUrl_11 = paste0(url_1, op_11, key_1, cityCode, routeNo)
reqUrl_11

pg_11=xmlParse(reqUrl_11)
pg_11 # 구조 파악을 위해 꼭 한번 와야 함.

doc_11=xmlToDataFrame(pg_11,nodes = getNodeSet(pg_11, "//body/items/item"))
head(doc_11)

# Opearation 2: 노선별경유정류소목록조회
numOfRows=paste0('&numOfRows=','10')
pg_No=paste0('&pg_No=','2')
cityCode=paste0('&cityCode=','25')
routeId=paste0('&routeId=','DJB30300004ND')
reqUrl_12 = paste0(url_1, op_12, key_1, numOfRows,pg_No, cityCode, routeId)
reqUrl_12

pg_12=xmlParse(reqUrl_12)
doc_12=xmlToDataFrame(pg_12,nodes = getNodeSet(pg_12, "//body/items/item"))
head(doc_12)

# Opearation 3: 노선정보항목조회
cityCode=paste0('&cityCode=','25')
routeId=paste0('&routeId=','DJB30300004ND')
reqUrl_13 = paste0(url_1, op_13, key_1,cityCode,routeId)
reqUrl_13

pg_13=xmlParse(reqUrl_13)
doc_13=xmlToDataFrame(pg_13,nodes = getNodeSet(pg_13, "//body/items/item"))
head(doc_13)

# Opearation 4: 도시코드 목록 조회
reqUrl_14 = paste0(url_1, op_14, key_1)
reqUrl_14

pg_14=xmlParse(reqUrl_14)
doc_14=xmlToDataFrame(pg_14,nodes = getNodeSet(pg_14, "//body/items/item"))
head(doc_14)
