#############################################################################
#
#Objective: set up a baseline code to numerically simulates ode's solutions 
#Author: Imelda Trejo
#
#Bibliography: Dynamical Systems for Biological Modeling, An Introduction.
#               By Fred Brauer and Christopher Kribs, 1ra. edici\'on, 2016
#
#Ejemplo 1. predator-pray Lotka-Volterra Equations, page 123
#
#code adapted from 
#https://www.rdocumentation.org/packages/deSolve/versions/1.40/topics/ode
#
#Last update: 10/02/2024
#
# ver este post  https://strimas.com/post/lotka-volterra/
################################################

# Instala el paquete si no lo tienes
#if (!require(deSolve)) {
#  install.packages("deSolve")
#}

library(deSolve)


# Definimos el modelo predator-prey Lotka-Volterra

predator_prey_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Ecuaciones del modelo
    dx <- x*(lambda-b*y)
    dy <- y*(-mu+c*b*x)
    # Retornar las derivadas
    list(c(dx, dy))
  })
}

# Parámetros del modelo
parameters <- c(
  lambda = 1,  # Tasa intrinseca de crecimiento de la presas
  mu = 0.2,      # Tasa natural de mortalidad de los predadores
  b =0.2,        # Tasa de ataque
  c = 0.1        # Tasa de efficiencia 
)

# Condiciones iniciales
x0 <- 1      # Población inicial de la presa
y0 <- 2        # Población inicial de los predadores

state <- c(x = x0, y=y0)

# Tiempo de simulación
times <- seq(0, 160, by = 1)

# Salida o solucion numérica del modelo
output <- ode(y = state, times = times, func = predator_prey_model, parms = parameters)

# Convertimos la salida en un data.frame
output <- as.data.frame(output)

#plot(output)

## User specified plotting
matplot(output[ , 1], output[ , 2:3], type = "l", xlab = "time", ylab = "Población",
        main = "Lotka-Volterra", lwd = 2)
legend("topright", c("prey", "predator"), col = 1:2, lty = 1:2)

#retrato phase

plot(output$x,output$y)
