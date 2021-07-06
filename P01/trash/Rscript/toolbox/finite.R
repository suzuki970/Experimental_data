# 有限母集団の割合の推定に必要な標本サイズを決める
finite <- function(	n=NULL,			# 標本サイズ
			N=NULL,			# 母集団サイズ
			p=NULL,			# 母比率
			epsilon=NULL,		# 精度
			conf.level=NULL)	# 信頼度（信頼係数）
{
	if (sum(sapply(list(n, N, p, epsilon, conf.level), is.null)) != 1) {
		stop("n, N, p, epsilon, conf.level のうちの，どれか一つだけが NULLでなければならない")
	}
	n.function <- quote(N/((epsilon/qnorm(0.5-conf.level/2, lower.tail=FALSE))^2*((N-1)/(p*(1-p)))+1))
	if (is.null(n)) {
		n <- eval(n.function)
	}
	else if (is.null(epsilon)) {
		epsilon <- uniroot(function(epsilon) eval(n.function)-n, c(1e-7, 0.9999999))$root
	}
	else if (is.null(N)) {
		N <- uniroot(function(N) eval(n.function)-n, c(1, 1e7))$root
	}
	else if (is.null(p)) {
		p <- uniroot(function(p) eval(n.function)-n, c(1e-7, 0.9999999))$root
	}
	else if (is.null(conf.level)) {
		conf.level <- uniroot(function(conf.level) eval(n.function)-n, c(1e-7, 0.9999999))$root
	}
	METHOD <- "有限母集団の割合の推定に必要な標本サイズ"
	structure(list(n=n, N=N, p=p, epsilon=epsilon, conf.level=conf.level, method=METHOD), class="power.htest")
}
