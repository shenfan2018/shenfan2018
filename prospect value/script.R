load("monthlyTK.RData")
SJ <- as.data.table(SJ)

a <- SJ[order(id, year, month)
	][, month_return.h1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, tk.group := ntile(ztk, 5), keyby = .(year, month)
	][, .(month_return.h1 = mean(month_return.h1)), keyby = .(year, month, tk.group)]

t.test(a[tk.group == 5, month_return.h1])
t.test(a[tk.group == 1, month_return.h1])

t.test(a[tk.group == 5, month_return.h1])
t.test(a[tk.group == 1, month_return.h1])
t.test(a[tk.group == 5, month_return.h1], a[tk.group == 1, month_return.h1], paired = FALSE)