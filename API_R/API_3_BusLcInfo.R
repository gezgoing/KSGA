library(XML)
library(RCurl)

# 국토교통부_버스위치정보 (공공데이터 포털)
# https://www.data.go.kr/dataset/15000515/openapi.do
# 활용신청 클릭

url_3 <- 'http://openapi.tago.go.kr/openapi/service/BusLcInfoInqireService'

op_31 <- '/getRouteAcctoBusLcList' # 노선별버스위치목록조회
op_32 <- '/getRouteAcctoSpcifySttnAccesBusLcInfo' # 노선별특정정류소접근버스위치정보조회
op_33 <- '/getCtyCodeList' # 도시코드 목록 조회

key_3 <- paste0('?ServiceKey=','____') # 밑줄 부분에 서비스 키 입력

# Opearation 1: 
cityCode=paste0('&cityCode=','25')
routeId=paste0('&routeId=','DJB30300052ND')
reqUrl_31 = paste0(url_3, op_31, key_3, cityCode, routeId)
reqUrl_31

pg_31=xmlParse(reqUrl_31)
pg_31

doc_31=xmlToDataFrame(pg_31,nodes = getNodeSet(pg_31, "//body/items/item"))
head(doc_31)


# Opearation 2: 
routeId=paste0('&routeId=','DJB30300052ND')
nodeId=paste0('&nodeId=','DJB8005621ND')
cityCode=paste0('&cityCode=','25')
reqUrl_32 = paste0(url_3, op_32, key_3, routeId, nodeId,cityCode)
reqUrl_32

pg_32=xmlParse(reqUrl_32)
doc_32=xmlToDataFrame(pg_32,nodes = getNodeSet(pg_32, "//body/items/item"))
head(doc_32)


# Opearation 3: 
reqUrl_33 = paste0(url_3, op_33, key_3)
reqUrl_33

pg_33=xmlParse(reqUrl_33)
doc_33=xmlToDataFrame(pg_33,nodes = getNodeSet(pg_33, "//body/items/item"))
head(doc_33)

