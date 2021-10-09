Rastrigin <- function(x1, x2)
{
    20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}
# 4.522979 4.522986
Ackley <- function(x1, x2)
{
    -20 * exp(-0.2*sqrt(0.5*(x1^2+x2^2))) - exp(0.5*(cos(2*pi*x1) + cos(2*pi*x2))) + exp(1) + 20
}
# -9.538420  9.542952
Baele <- function(x1, x2)
{
    (1.5-x1+x1*x2)^2+(2.25-x1+x1*x2^2)^2+(2.625-x1+x1*x2^3)^2
}

Eggholder <- function(x1, x2)
{
    -1*(x2+47)*sin(sqrt(abs(x1/2+x2+47))) - x1*sin(sqrt(abs(x1-x2-47)))
}

Holder <- function(x1, x2){
    -1*abs(sin(x1)*cos(x2)*exp(abs(1-sqrt(x1^2+x2^2)/pi)))
}
