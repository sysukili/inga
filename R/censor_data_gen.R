expit <- function(x) {
    exp(x) / (1 + exp(x))
}

#' @title 数据生成函数：censor_data_gen
#' @description 生成模拟数据用于因果推断分析，支持不同删失比例、协变量重叠程度和模型设置。
#' @param n 样本量，默认为 5000。
#' @param p 协变量数量，默认为 10。
#' @param PH 是否假设比例风险模型，默认为 TRUE。
#' @param censor 删失比例，默认为 30。
#' @param setting 数据生成的设置，默认为 1。
#' @param overlap 协变量之间的重叠程度，可选值为 "strong"、"medium" 和 "weak"，默认为 "strong"。
#' @return 返回一个数据框，包含以下列：
#' \itemize{
#'   \item \code{LP0}, \code{LP1}: 线性预测器。
#'   \item \code{lambda0}, \code{lambda1}: 比例参数。
#'   \item \code{eta_0}, \code{eta_1}: 风险模型参数。
#'   \item \code{p1_category}: 倾向得分类别。
#'   \item \code{x1} 至 \code{x10}: 协变量。
#'   \item \code{W}: 二分类变量，表示分组。
#'   \item \code{C}: 删失时间。
#'   \item \code{Time}: 观察到的生存时间。
#'   \item \code{Event}: 删失指标，1 表示未删失，0 表示删失。
#'   \item \code{T0}, \code{T1}: 潜在生存时间。
#'   \item \code{p}: 倾向得分。
#' }
#' @examples
#' # 生成默认参数的数据
#' data <- censor_data_gen()
#'
#' # 生成弱重叠、删失比例为 20% 的数据
#' data <- censor_data_gen(n = 1000, overlap = "weak", censor = 20)
#'
#' # 使用非比例风险模型
#' data <- censor_data_gen(PH = FALSE)
#' @export
censor_data_gen = function(n=5000, p=10,  PH=TRUE, censor = 30, setting = 1, overlap = "strong") {

    # 生成连续型协变量矩阵 X，服从正态分布，标准差为 1
    X <- matrix(rnorm(p * n, sd = 1), nrow = n, ncol = p)
    x1 <- X[, 1]
    x2 <- X[, 2]
    x3 <- X[, 3]
    x4 <- X[, 4]
    x5 <- X[, 5]

    # 生成二分类协变量，每个变量的值为 0 或 1，概率均等
    x6 <- sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5))
    x7 <- sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5))
    x8 <- sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5))
    x9 <- sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5))
    x10 <- sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5))

    # 根据 overlap 参数的值，计算概率 p1
    if (overlap == "strong") {
        # 强重叠：协变量对 p1 的影响较小
        p1 <- expit(
            -(0.1 * x1 - .9 * x2 - .3 * x3  - .1 * x4 - 0.1 * x6 -0.2*x7 - 0.4 * x8 + .5 * x9)*0.2
        )
    }

    if (overlap == "medium") {
        # 中等重叠：协变量对 p1 的影响中等
        p1 <- expit(
            -1 * 1 * x1 - .9 * 1 * x2 - .3 * 1 * x3  - .1 * 1 * x4 - 0.1 * 1 * x6 -0.3 * 1* x7 - 0.4 * 1 * x8 + .5 * 1 * x9
        )
    }

    if (overlap == "weak") {
        # 弱重叠：协变量对 p1 的影响较大
        p1 <- expit(
            0.3 - 0.1 * 5 * x1 - .9 * 5 * x2 - .3 * 5 * x3  - .1 * 5 * x4 - 0.1 * 5 * x6 -0.3 * 5* x7 - 0.4 * 5 * x8 + .5 * 5 * x9
        )
    }

    # 计算 p2，表示 p1 的补概率
    p2 <- 1 - p1

    # 将 p1 分为 50 个类别，用于后续分析
    p1_category <- cut(p1, breaks=c(quantile(p1, probs = seq(0, 1, by = 0.02))), include.lowest = T, labels = 1:50)

    # 根据 p1 和 p2 生成二分类变量 W
    W = NULL
    for (i in 1:n) {
        W[i] <- sample(c(0, 1),
                       size = 1,
                       replace = TRUE,
                       prob = c(p1[i], p2[i]))
    }
    table(W) # 输出 W 的分布表

    # 根据 PH 参数设置比例风险模型的参数 eta_0 和 eta_1
    if (PH == FALSE) {
        # 非比例风险模型：eta_0 和 eta_1 根据协变量计算
        eta_0 <- exp(0.7 + 1.8 * x3 + 0.8 * x7)
        eta_1 <- exp(0.9 - 0.5 * x1 + 0.5 * x2)
        summary(eta_0) # 输出 eta_0 的统计摘要
    }
    if (PH == TRUE) {
        # 比例风险模型：eta_0 和 eta_1 固定为 2
        eta_0 <- 2
        eta_1 <- 2
    }



    # 根据 setting 参数生成线性预测器 LP0 和 LP1
    if (setting == 1) {
        LP0 <- 0.2 - 0.5 * x1 - 0.8 * x3 - 1.8 * x4 - 0.9 * x6 - 0.1 * x7
        LP1 <- -0.2 + 0.1 / (1 + exp(-x1)) - 0.8 * sin(x3) - 0.1 * x4^2 - 0.3 * x6 - 0.2 * x7
    } else if (setting == 2) {
        LP0 <- -0.1 + 0.1 * x1^2 - 0.2 * sin(x3) + 0.2 / (1 + exp(-x4)) + 0.2 * x6 - 0.3 * x7
        LP1 <- -0.2 + 0.1 / (1 + exp(-x1)) - 0.8 * sin(x3) - 0.1 * x4^2 - 0.3 * x6 - 0.2 * x7
    } else if (setting == 3) {
        LP0 <- -0.1 + 0.1 * x1^2 - 0.2 * sin(x3) + 0.2 / (1 + exp(-x4)) + 0.2 * x6 - 0.3 * x7
        LP1 <- 0.5 - 0.1 / (1 + exp(-x2)) + 0.1 * sin(x3) - 0.1 * x4^2 + 0.2 * x5 - 0.3 * x6
    } else if (setting == 4) {
        LP0 <- -0.2 + 0.5 * sin(pi * x1 * x3) + 0.2 / (1 + exp(-x4)) + 0.2 * x6 - 0.3 * x7
        LP1 <- 0.5 - 0.1 / (1 + exp(-x2)) + 0.1 * sin(x3) - 0.1 * x4^2 + 0.2 * x4 - 0.3 * x6
    }

    # 根据 censor 参数生成删失时间 C
    C <-rpois(n,censor+10*abs(x3))

    # 生成均匀分布的随机数 U，用于计算潜在生存时间
    U = runif(n, 0, 1)

    # 定义比例参数 lambda0 和 lambda1
    lambda0 <- 500
    lambda1 <- 2000

    # 计算潜在生存时间 T1 和 T2
    T0 <- (lambda0 * (-log(U)) / exp(LP0)) ^ (1 / eta_0)
    T1 <- (lambda1 * (-log(U)) / exp(LP1)) ^ (1 / eta_1)

    # 计算观察到的生存时间 Tobs
    T <- cbind(T0, T1)
    TW <- cbind(T, W)
    Tobs <- apply(TW, 1, function(x)
        x[1:2][x[3] + 1]) # 根据治疗分组 W 确定观察到的生存时间

    # 计算观察时间与删失时间的最小值
    Tobs_C <- pmin(Tobs, C)

    # 计算删失指标 delta，1 表示未删失，0 表示删失
    delta <- ifelse(Tobs > C, 0, 1)

    # 返回生成的数据
    return(
        data.frame(
            LP0 = LP0,
            LP1 = LP1,
            lambda0 = lambda0,
            lambda1 = lambda1,
            eta_0 = eta_0,
            eta_1 = eta_1,
            p1_category =  p1_category,
            x1,
            x2,
            x3,
            x4,
            x5,
            x6,
            x7,
            x8,
            x9,
            x10,
            W=W,
            C=C,
            Time=Tobs_C,
            Event=delta,
            T0 = T0,
            T1 = T1,
            p=p1
        )
    )
}
