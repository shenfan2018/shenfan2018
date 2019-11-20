# all semi - annual
# return monthly
# flow age size quarterly
# 涉及到portfolio, semi-annual

# 000 stock-holding 
# single named("socialpf.RData") and ("portfolio.RData")
# sum name("IOS.RData") and ("IOF.RData")

# 001 social and fund characterstic 
# named("fundchar.RData") and ("socialchar.RData") 

# 001-holding return (social and fund)
# named("social_ret.RData") and named("fund_ret.RData") [+ ret.f.raw]
# 原版本与现版本差异 1. 原版本多两只已经倒闭的基金 2. 现版本raw return多了，原版本match的问题

# 001-social and fund characterstic 还包括了monthly的因子和vol以及bull bear
# named("monthlyfactor.RData")

## 002-new single holding (将equal weighted and value weighted都包括进去)
# named("")

## 003 all is summary,回归都不能用的

## 005-02 new overlap each to each
# named("overlap.RData")

## 005-04 opposite trade each to each
# named("oppositetrade.RData")

# performance including four_alpha in past 36 months and past 12 months prod return
# 006-01-fund performance
# named("fundper.RData")
# 006-01-social performance
# named("socialper.RData")

# 006-04-attract more flow (company)
# named("company_ret.RData")
# 里面包括sub sample and all sample


