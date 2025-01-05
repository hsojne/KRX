
qvar_make_plot <- function(val=data, main=cha, col=cols){
  plot(date_qvar, val, type="l", xlab="", ylab="", main=main, lwd=0.7)
  grid(NA, NULL, lty=2)
  
  polygon(c(date_qvar, rev(date_qvar)), c(pmin(val, 0), rep(0, length(val))),
          col = col, border = NA)
  polygon(c(date_qvar, rev(date_qvar)), c(pmax(val, 0), rep(0, length(val))),
          col = col, border = NA)
  
  lines(date_qvar, val, type="l", col=col, lwd=0.1)
  abline(h=0, col="black", lwd=0.1)
}

# 0.95
par(mfrow = c(3,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))
date_qvar = daily.volatility[daily.volatility$date  >= "1997-06-12", 1]

high.set <- as.data.frame(q_95$NET)
qvar_make_plot(high.set$KOSPI, main = "Q = 0.95 KOSPI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(high.set$NIKKEI, main = "Q = 0.95 NIKKEI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(high.set$SP500, main = "Q = 0.95 SP500", col = rgb(153/255, 0, 0, 0.7))

# 0.5
min.set = as.data.frame(q_50$NET)
qvar_make_plot(min.set$KOSPI, main = "Q = 0.5 KOSPI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(min.set$NIKKEI, main = "Q = 0.5 NIKKEI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(min.set$SP500, main = "Q = 0.5 SP500", col = rgb(0, 51/255, 102/255, 0.7))

# 0.05
low.set = as.data.frame(q_05$NET)
qvar_make_plot(low.set$KOSPI, main = "Q = 0.05 KOSPI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(low.set$NIKKEI, main = "Q = 0.05 NIKKEI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(low.set$SP500, main = "Q = 0.05 SP500", col = rgb(51/255, 0, 102/255, 0.7))

par(mfrow = c(3,3), oma = c(0, 0, 0, 0) + 0.5, mar = c(1, 1, 1, 1) + 0.5, 
    mgp = c(1, 0.4, 0))
# w 0.95
qvar_make_plot(w.95$`From KOSPI`/10, main = "Q = 0.95 From KOSPI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(w.95$`From NIKKEI`/10, main = "Q = 0.95 From NIKKEI", col = rgb(51/255, 0, 102/255, 0.7))
qvar_make_plot(w.95$`From KOSPI`/10, main = "Q = 0.95 From SP500", col = rgb(51/255, 0, 102/255, 0.7))

# w 0.5
qvar_make_plot(w.50$`From KOSPI`/10, main = "Q = 0.5 From KOSPI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(w.50$`From NIKKEI`/10, main = "Q = 0.5 From NIKKEI", col = rgb(0, 51/255, 102/255, 0.7))
qvar_make_plot(w.50$`From KOSPI`/10, main = "Q = 0.5 From SP500", col = rgb(0, 51/255, 102/255, 0.7))

# w 0.05
qvar_make_plot(w.05$`From KOSPI`/10, main = "Q = 0.05 FromKOSPI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(w.05$`From NIKKEI`/10, main = "Q = 0.05 FromNIKKEI", col = rgb(153/255, 0, 0, 0.7))
qvar_make_plot(w.05$`From KOSPI`/10, main = "Q = 0.05 From SP500", col = rgb(153/255, 0, 0, 0.7))


par(mfrow=c(4,1))

kot <- data.frame(
  date = index(daily.return), ret = daily.return$KOSPI
)
sto <- data.frame(
  date = index(zoo_total.stock), stock = zoo_total.stock$KOSPI
)

plot(index.new.df$date, index.new.df$new.index, type="l", col= rgb(0, 51/255, 102/255, 0.7),
     main="new index", xlab="")
plot(index.new.df$date, index.new.df$vol, type="l", col = rgb(51/255, 0, 102/255, 0.7),
     main="volatility", xlab="")
plot(index.new.df$date, index.new.df$ret, type="l", col = rgb(0, 51/255, 51/255, 0.7),
     main="retrun", xlab="")
plot(index.new.df$date, index.new.df$stock, type="l", col = rgb(51/255, 51/255, 51/255, 0.7),
     main="stock", xlab="" )




''' 한국 미국 일본 간의 수익률 변동성의 전이효과를 분위수별로 추출함
추출한 데이터를 가지고 통합 전이 지수를 만듦
 - 한국이 한국으로 주는 변동성 전이효과
 - 일본이 한국으로 주는 변동성 전이효과
 - 미국이 한국으로 주는 변동성 전이효과
 
 변동성은 수익률에서 고려한 변동성인데
 우리가 꼽는거는 변동성의 전이효과임. 즉 한국의 변동성이
 "한국 자체", "일본에 의한 변화", "미국에 의한 변화"로 설명하고 나머지 설명하지
 못하는 부분은 잔차(예를 들어 중국에 의한 변화)로 설정
 
 결과를 보면 일반적으로 5%, 50%(저위험, 평균 위험)에서 한국이 한국에서, 미국에서 한국으로
 주는 것으로 확인됨.
 
 하지만 코로나 & 금융위기에서 일시적으로 전이효과가 낮아짐
 반면 동기간 일본이 한국으로 주는 젼이효과는 커짐
 
 이는 해당 기간동안 변동성의 구축요소가 한국&미국에서 일본으로 옮겨진것으로 판단
 
 따라서 한국의 주가 수익률 변동성은 한국 미국만이 아닌 일본에 의한 효과도 반영
 효과 1. 기존의 국내적 영향뿐만 아니라 해외 요인의 영향을 반영하여 시장의 불확실성을
      더 잘 설명할 수 있음
      
      2. 변동성 전이 효과의 동적 특성을 반영하여 위기 시기와 안정 시기를 구분
 
 우리는 이같은 결과를 기반으로 한미일간의 변동성 전파 지수를 새롭게 구축할 예정
 
 우선 구축예정인 지수는 각각의 상관관계가 있음을 가정하고자 한다.
 첫째, 수익률간의 상관은 POSITIVE여야 생각이 든다.
 왜냐하면 전파지수가 커진다는 것은 국내 영향만이 아닌 타국 영향을 고려하고 이로인해
 불확실성은 높아지기 때문이다. 변동성은 증폭되고 high risk, high return이기에 수익률은
 높아져야 한다.
 
 하지만 만일 전파지수가 커지는 구간에서 변동성간의 무상관일 경우?
 혹은 수익률간의 무상관일 경우? 이런 경우에서 어떻게 해석을 해야하지??
 
 먼저 첫번째 해석, 시장의 변동성이 독립적인 상황으로 해석
 일시적으로 시장간 동조성이 감소하였거나, 환율 & 금리 & 정책 등의 특정 요인이 영향을 주었다고
 생각할 수 있음
 
 두번째 해석, 동적 상관계수가 양수인 경우, 시장간 동조성이 높아진 걸로 해석
 그렇다면 상관계수가 음수는 전파성 증가와 변동성 감소로 해석?
 무상관은 시장간 독립인 상태 
 
 양수는 받는게 많아 그래서 변동성이 커져 ㅇㅋ? ㅇㅋ 이해 됨 이건
 **음수는 받는게 많아 근데 변동성이 작아져 <--- 이게 뭐야 ??
 0은 받아도 변동성과 무관해 이 상황은 한미일 시장간 동조성이 떨어진 구간임 
 
 **이거는 대체 뭐지 ? 
 
 아, 전파성 지수를 다시 정의하면 "전파성 지수
 
 아니면 상관계수를 인자로 곱해서 다시 만들어? 
 
 r*w 이잖아 r이 높아 근데 w가 낮아 이건 전파성이 낮아 그치 ? ㅇㅋ
 r이 낮아 근데 w가 높아 이것도 시장간 주고 받는게 큰데
 
 아 총전이지수를 분모로 해서 하면? 총전이수가 해당 퀀타이가 얼마나
 전이성을 주냐 잖아
 
 

'''

