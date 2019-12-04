  !!El objetivo de este programa es el de aproximar las raíces de una función
  !!utilizando el método de punto fijo.
  !!--------------------------------------------------------------------------

PROGRAM puntofijo
  
  !Declaramos las variables.
  IMPLICIT NONE
  REAL(8)::x                 !!Variable x de la funcón.
  REAL(8)::g                 !!Función despejada de nuestra función a evaluar.
  REAL(8)::aprox             !!Valor aproximado.
  REAL(8)::tol               !!Tolerancia aceptable de error.
  REAL(8)::error             !!Variable que calcula el error del aproximado.

  !Declaramos nuestro error y la tolerancia de error.
  tol=1e-5
  error=1

  !Declaramos un valor inicial arbitrario para nuestra aproximación.
  aprox=1
  
  !Explicamos el funcionamiento del programa al usuario.
  PRINT*,"Hola, bienvenido al programa de punto fijo, donde aproximaremos la"
  PRINT*,"raíz de una función utilizando el método del punto fijo."

  PRINT*,"En este programa, aproximaremos la raíz a la función:"
  PRINT*,"x**3 + x**2 + 2x -1 = 0 en el intervalo [0,1]."

  !Calculamos la aproximación en un DO condicional; cuando el error sea menor
  !a nuestra tolerancia de error, el DO se detiene.
  DO WHILE(error>tol)
     x = g(aprox)
     aprox=x

     error = abs(aprox - g(aprox))

  END DO

  !Mostramos la aproximación resultante al usuario.
  PRINT*,"La raíz de la función, con un margen de error de 1x10-5, es:"
  PRINT*,aprox
  
END PROGRAM puntofijo

!Establecemos la función a evaluar.
FUNCTION g(x)
  REAL(8),INTENT(IN)::x
  REAL(8)::g

  g = 1/(x**2+x+2)

END FUNCTION g

  
