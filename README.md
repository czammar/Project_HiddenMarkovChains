# Fundamentos de estadística

6 de octubre de 2019

### Introducción

Este documento pretende ser un bosquejo de los temas que se deben cubrir para la presentación del curso de Fundamentos de Estadística sobre el tema de cadenas de Markov ocultas.

### Consideraciones generales

* **Indicaciones de Nasser**
	* **Formato:** Se debe exponer el tema en un presentación de Beamer (LaTeX) o PowerPoint. *Nota:* LaTex permite escribir ecuaciones matemáticas de manera sencilla; entiendo que Sebastían y yo estamos familiarizados con su uso.
	* La presentación se debe enviar al profesor con algunos días de anticipación (no se especifica cuando; sugiero que sea al menos una semana antes, es decir máximo a 31 de octubre, para lidiar con imprevistos)
	*  **Fecha de exposición:** 7 de Noviembre de 2019

* **Lecturas:**
	* Francois Caron, University of Oxford, Febrero 2019. *Lecture notes: Hidden Markov Models.* Ver: http://www.stats.ox.ac.uk/~caron/teaching/sb1b/lecturehmm.pdf
	* Emilio Frazzoli, Aeronautics and Astronautics (MIT),  Noviembre 2010. *Principes of Autonomy and Decision Making. Lecture 20: Intro to Hidden Markov Models.* Ver: https://ocw.mit.edu/courses/aeronautics-and-astronautics/16-410-principles-of-autonomy-and-decision-making-fall-2010/lecture-notes/MIT16_410F10_lec20.pdf
	
### Bosquejo del proyecto

1. **Motivación de las cadenas de Markov ocultas con un ejemplo de aplicaciones:**
	* Caron muestra unejemplo de clasificar palabras de un texto por *tags* de tipo de función dentro de oraciones (pronombre, articulos, verbos, etc). También introduce ejemplos breves de posicionamiento de un robot y secuenciación genética.
	* Frazzoli se va aun más atrás e inicia hablando de cadenas de markov para modelar el juevo de golpear al topo (whack-to-mole). La introducción a cadenas de Markov ocultas (slide 10) la motiva haciendo alusión a una aplicación con sensores ruidosos de una variable donde el estado es desconocido.
	
2. **Modelos Discretos de Markov ocultos:** (**Nota:** en ambos casos los autores abordan primero los casos discretos y posteriormente hablan de los casos continuos y sus generalizaciones; creo que deberiamos seguir ese enfoque)
	* **Recordatorio del concepto de cadenas de Markov en el caso discreto e introducción de notaciones.** *Nota:* La notación de Caron es más general que la de Frazzoli, que introduce notación específica del caso discreto, pienso que deberíamos seguir la primera:
		* Propiedad de Markov:
		$$P(X_{t+1} =x_{t+1}| X_T=x_t, ..., X_0=x_0)  = P(X_{t+1} =x_{t+1}| X_T=x_t) $$
		* Cadenas homogéneas:
		$$A_{ij} := P(X_{t+1} =i| X_{t} =j )$$
		
		* Distribución conjunta 
		$$p(x_{0:T}) := P(X_0=x_0, ..., X_T=x_T) $$
		
		* Parametrización de la distribución conjunta

		$$p(x_{0:T}) := \mu_{x_0} \prod_{t=1}^T A_{x_{t-1},x_{t}} \mbox{ con }   \mu_{x_0} = P(X_0 = x_0)$$
		
	* **Cadenas de Markov ocultas.** *Nota:* La notación de Caron es más general que la de Frazzoli, que introduce notación específica del caso discreto, pienso que deberíamos seguir la primera:
		* Definicion
		* Independencia condicional para el espacio de estados ocultos.
		* Homogeneidad para cadenas de Markov ocultas y *emission mass probability function*
		* Parametrización de la distribución conjunta de la cadena de Markov oculta
		* Representación gráfica

	* **Inferencia para cadenas de Markov ocultas.** 
		* Problemas de interés para hacer inferencia (definición del problema y forma de modelarlo para su resolución algorítmica):
			* Evaluación:  forward y backward algorithm basado en relaciones de recurrencia.	
			* Explicación: 
				* Filtering (algoritmos de alfa y beta recursion)
				* Prediction
				* Smoothing
				* Likelihood
				* Decoding/Most likely state path/maximum a posteriori y el algoritmo de Viterbi
				* **Nota:** 1) en las dos notas se presentan los algoritmos a manera de pseudo código o indicaciones, 2) para filtering, smooting y Most likely state path/maximum a posteriori las notas de Caron presentan simulaciones numéricas en R para el problema de una rana y sensores en una escalera; 3) las notas de Frazzoli incluyen un ejemplo *muy sencillo* para filtering, smoothing y decoding en el problema del topo (whack-to-mole) comparando además los resultados para estos; 4) deberíamos incluir algo de simulaciones numéricas también en la presentación?  

2. **Modelos de estados continuos para cadenas de Markov ocultas:** (**Nota:** solo se aborda en las notas de Caron)
	* Sistemas lineales gausianos:
		* Repaso de conceptos de distribuciones normales multivariadas y sus condicionales
		* **Modelos dinámicos lineales de espacios de estados gausianos**
		* **Inferencia para modelos dináminos lineales de espacios de estados gausianos**
			* Kallman filters (generalizaciones de los métodos para espacios discretos)
			*  Kallman smoothers (generalizaciones de los métodos para espacios discretos)
			* **Notas:** 1) se presentan ejemplos numéricos en R para kallman filters y smoothers; 2) nosotros también deberíamos de incluirlos?
