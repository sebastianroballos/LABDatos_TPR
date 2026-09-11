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

#Hacemos una grilla de 1 fila y 4 columnas
par(mfrow = c(1,4))

#Iteramos histogramas de la chi cuadrado con un k cada vez mayor
n_sim = 4
deg = 4^(1:n_sim)
means = numeric(n_sim)
sds = numeric(n_sim)
for (i in 1:n_sim){
  Z = rchisq(n_hogares, deg[i])
  hist(Z,
       breaks = 30,
       freq = FALSE,    #Representamos como densidad                              
       col = adjustcolor("steelblue", alpha.f = 0.6), # Color
       border = "white",
       main = bquote(chi^2 ~ "con" ~ df == .(deg[i])), # Título (grados)
       xlab = "Ingreso", ylab = "Densidad")
  
  # Densidad (curva)
  curve(dchisq(x, deg[i]), add = TRUE, col = "firebrick", lwd = 2)
  
  #Guardamos media y sd de cada simulación
  means[i] = mean(Z)
  sds[i] = sd(Z)
}

#Volvemos al ploteo normal
par(mfrow = c(1, 1))

