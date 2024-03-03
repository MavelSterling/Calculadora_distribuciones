#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Instalar y cargar las bibliotecas necesarias
install.packages("shiny")
library(shiny)

# Definir la interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Calculadora de Distribuciones de Probabilidad y Densidad"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Selecciona la distribución:",
                  choices = c("Binomial", "Binomial Negativa", "Normal", "Beta", "Exponencial", 
                              "Gamma", "Poisson", "Hipergeométrica", "Bernoulli", "Uniforme Discreta", "Geométrica")),
      
      sliderInput("x", "Valor(s) de x:",
                  min = 0, max = 100, value = c(0, 10)),
      
      numericInput("n", "Tamaño de la muestra (n):", value = 10),
      numericInput("p", "Probabilidad (p):", value = 0.5),
      numericInput("lambda", "Tasa (λ):", value = 1),
      numericInput("alpha", "Parámetro alfa (α):", value = 1),
      numericInput("beta", "Parámetro beta (β):", value = 1),
      numericInput("mean", "Media:", value = 0),
      numericInput("sd", "Desviación estándar:", value = 1),
      
      hr(), # Insertar una línea horizontal
      
      # Mostrar los parámetros seleccionados
     # verbatimTextOutput("parameters")
    ),
    
    mainPanel(
      verbatimTextOutput("distributionInfo"),  # Mostrar la fórmula y los parámetros de la distribución
      verbatimTextOutput("result"),
      plotOutput("distributionPlot")  # Mostrar el gráfico de la distribución
    )
  )
)

# Definir el servidor
server <- function(input, output) {
  output$parameters <- renderPrint({
    params <- list(
      "Tamaño de la muestra (n)" = input$n,
      "Probabilidad (p)" = input$p,
      "Tasa (λ)" = input$lambda,
      "Parámetro alfa (α)" = input$alpha,
      "Parámetro beta (β)" = input$beta,
      "Media" = input$mean,
      "Desviación estándar" = input$sd
    )
    return(params)
  })
  
  output$distributionInfo <- renderPrint({
    formula <- switch(input$dist,
                      "Binomial" = paste("Binomial(n =", input$n, ", p =", input$p, ")"),
                      "Binomial Negativa" = paste("Binomial Negativa(n =", input$n, ", p =", input$p, ")"),
                      "Normal" = paste("Normal(mean =", input$mean, ", sd =", input$sd, ")"),
                      "Beta" = paste("Beta(alpha =", input$alpha, ", beta =", input$beta, ")"),
                      "Exponencial" = paste("Exponencial(lambda =", input$lambda, ")"),
                      "Gamma" = paste("Gamma(alpha =", input$alpha, ", beta =", input$beta, ")"),
                      "Poisson" = paste("Poisson(lambda =", input$lambda, ")"),
                      "Hipergeométrica" = paste("Hipergeométrica(n =", input$n, ", N =", input$n + input$N, ", m =", input$N, ")"),
                      "Bernoulli" = paste("Bernoulli(p =", input$p, ")"),
                      "Uniforme Discreta" = paste("Uniforme Discreta(min =", input$x[1], ", max =", input$x[2], ")"),
                      "Geométrica" = paste("Geométrica(p =", input$p, ")"))
    return(formula)
  })
  
  output$result <- renderPrint({
    dist <- switch(input$dist,
                   "Binomial" = pbinom(input$x[2], input$n, input$p) - pbinom(input$x[1] - 1, input$n, input$p),
                   "Binomial Negativa" = sum(dnbinom(input$x[1]:input$x[2], input$n, input$p)),
                   "Normal" = pnorm(input$x[2], input$mean, input$sd) - pnorm(input$x[1] - 1, input$mean, input$sd),
                   "Beta" = pbeta(input$x[2], input$alpha, input$beta) - pbeta(input$x[1] - 1, input$alpha, input$beta),
                   "Exponencial" = pexp(input$x[2], input$lambda) - pexp(input$x[1] - 1, input$lambda),
                   "Gamma" = pgamma(input$x[2], input$alpha, input$beta) - pgamma(input$x[1] - 1, input$alpha, input$beta),
                   "Poisson" = ppois(input$x[2], input$lambda) - ppois(input$x[1] - 1, input$lambda),
                   "Hipergeométrica" = sum(dhyper(input$x[1]:input$x[2], input$n, input$N - input$n, input$N)),
                   "Bernoulli" = sum(dbinom(input$x[1]:input$x[2], 1, input$p)),
                   "Uniforme Discreta" = sum(dunif(input$x[1]:input$x[2], input$x[1], input$x[2])),
                   "Geométrica" = sum(dgeom(input$x[1]:input$x[2], input$p))
    )
    
    return(dist)
  })
  
  output$distributionPlot <- renderPlot({
    max_density <- switch(input$dist,
                          "Beta" = max(dbeta(seq(0, 1, length.out = 1000), input$alpha, input$beta)),
                          "Normal" = max(dnorm(seq(-5, 5, length.out = 1000), input$mean, input$sd)),
                          "Exponencial" = max(dexp(seq(0, 10, length.out = 1000), input$lambda)),
                          "Gamma" = max(dgamma(seq(0, 10, length.out = 1000), input$alpha, input$beta)),
                          "Poisson" = max(dpois(seq(0, 10, length.out = 1000), input$lambda)),
                          "Uniforme Discreta" = 1 / (input$x[2] - input$x[1] + 1),
                          "Geométrica" = max(dgeom(seq(0, 10, length.out = 1000), input$p))
    )
    
    dist <- switch(input$dist,
                   "Binomial" = barplot(dbinom(0:input$x[2], input$n, input$p), names.arg = 0:input$x[2], xlab = "Valor de x", ylab = "Probabilidad", ylim = c(0, 1)),
                   "Binomial Negativa" = barplot(dnbinom(0:input$x[2], input$n, input$p), names.arg = 0:input$x[2], xlab = "Valor de x", ylab = "Probabilidad", ylim = c(0, 1)),
                   "Normal" = curve(dnorm(x, input$mean, input$sd), from = input$x[1], to = input$x[2], n = 1000, xlab = "Valor de x", ylab = "Densidad de probabilidad", ylim = c(0, max_density)),
                   "Beta" = curve(dbeta(x, input$alpha, input$beta), from = 0, to = 1, n = 1000, xlab = "Valor de x", ylab = "Densidad de probabilidad", ylim = c(0, max_density)),
                   "Exponencial" = curve(dexp(x, input$lambda), from = input$x[1], to = input$x[2], n = 1000, xlab = "Valor de x", ylab = "Densidad de probabilidad", ylim = c(0, max_density)),
                   "Gamma" = curve(dgamma(x, input$alpha, input$beta), from = input$x[1], to = input$x[2], n = 1000, xlab = "Valor de x", ylab = "Densidad de probabilidad", ylim = c(0, max_density)),
                   "Poisson" = barplot(dpois(0:input$x[2], input$lambda), names.arg = 0:input$x[2], xlab = "Valor de x", ylab = "Probabilidad", ylim = c(0, 1)),
                   "Hipergeométrica" = barplot(dhyper(0:input$x[2], input$n, input$N - input$n, input$N), names.arg = 0:input$x[2], xlab = "Valor de x", ylab = "Probabilidad", ylim = c(0, 1)),
                   "Bernoulli" = barplot(dbinom(0:1, 1, input$p), names.arg = 0:1, xlab = "Valor de x", ylab = "Probabilidad", ylim = c(0, 1)),
                   "Uniforme Discreta" = barplot(dunif(0:input$x[2], input$x[1], input$x[2]), names.arg = 0:input$x[2], xlab = "Valor de x", ylab = "Densidad de probabilidad", ylim = c(0, 1)),
                   "Geométrica" = barplot(dgeom(0:input$x[2], input$p), names.arg = 0:input$x[2], xlab = "Valor de x", ylab = "Probabilidad", ylim = c(0, 1)))
    return(dist)
  })
}


# Correr la aplicación Shiny
shinyApp(ui = ui, server = server)
