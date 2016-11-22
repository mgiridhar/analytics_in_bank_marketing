bank = read.csv('bank.csv')

M=fit(y~., bank, model="svm")
I=Importance(M, bank)
S=sort.int(I$imp, decreasing = TRUE, index.return = TRUE)
N=10
L=list(runs=1, sen=t(I$imp[S$ix[1:N]]))
LEG=names(bank)

mgraph(L, graph='IMP', leg = LEG[S$ix[1:N]], col='gray', Grid=10)

vecplot(I, graph = "VEC", xval = 12, main = "Call Duration Relevance", Grid = 10, TC=2, sort = "decreasing")

vecplot(I, graph = "VEC", xval = 11, main = "Last Call Month", Grid = 10, TC=2, sort = "decreasing")
