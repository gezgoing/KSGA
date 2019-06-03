library(XML)
library(RCurl)

# OO 서비스 (OO데이터 포털)
# https://
# 활용신청 클릭

url <- 'http://openapi.tago.go.kr/openapi/service/BusLcInfoInqireService'

op_1 <- '/getRouteAcctoBusLcList' # 노선별버스위치목록조회
op_2 <- '/getRouteAcctoSpcifySttnAccesBusLcInfo' # 노선별특정정류소접근버스위치정보조회
op_3 <- '/getCtyCodeList' # 도시코드 목록 조회

key <- paste0('?ServiceKey=','____') # 밑줄 부분에 서비스 키 입력

# Opearation 1: 
cityCode=paste0('&cityCode=','25')
routeId=paste0('&routeId=','DJB30300052ND')
reqUrl_1 = paste0(url, op_1, key, cityCode, routeId)
reqUrl_1

pg_1=xmlParse(reqUrl_1)
pg_1

doc_1=xmlToDataFrame(pg_1,nodes = getNodeSet(pg_1, "//body/items/item")) # path구조 바꾸기
head(doc_1)


# Opearation 2: 
