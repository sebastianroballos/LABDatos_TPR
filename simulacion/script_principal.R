#### Ejercicio Simulación 1: Demanda con preferencias Cobb-Douglas ####
#Para empezar, voy a poner todo el código en este archivo (con las funciones). 
#Una vez que esté más pulido, ahí pondré las funciones en su respectivo archivo

#### Setup ####
#Definimos todas las variables que vamos a usar
n_hogares = 3000
p_1 = 10
p_2 = 5
a_1 = 0.6
a_2 = 0.4
x_1 = 0
x_2 = 0
u_function = (x_1**a_1)*(x_2**a_2)
k = 1

#### Inciso 1: Generación de Ingresos ####

Y = rchisq(n_hogares, 30)
hist(Y)

#Veamos cómo varía la dispersión de ingresos a medida que cambia k: Vamos iterando los grados de 
#libertad y ploteamos los histogramas todos juntos

#Hacemos una grilla de 2 filas y 3 columnas
par(mfrow = c(2,3))

#Iteramos histogramas de la chi cuadrado con un k cada vez mayor
n_sim = 6
deg = 1
means = numeric(n_sim)
sds = numeric(n_sim)
for (i in 1:n_sim){
  Z = rchisq(n_hogares, deg)
  hist(Z, main = paste("df =", deg), xlab = "Z", col = "steelblue", border = "white")
  deg = deg*5
  means[i] = mean(Z)
  sds[i] = sd(Z)
}

#Volvemos al ploteo normal
par(mfrow = c(1, 1))

