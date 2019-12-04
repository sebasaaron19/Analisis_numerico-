  !!El objetivo de este programa es el de encontrar las raíces de una función
  !!utilizando el método de Newton-Rhapson modificado.
  !!-------------------------------------------------------------------------

PROGRAM newtonmod

  !Diccionario de variables
  IMPLICIT NONE
  REAL(8)::f                   !!Función a la cual buscamos aproximar sus raíces.
  REAL(8)::d1f                 !!Primera derivada de la función.
  REAL(8)::d2f                 !!Segunda derivada de la función.
  REAL(8)::u                   !!Función del criterio que permite encontrar las raíces.
  REAL(8)::du                  !!Derivada de la función u.
  REAL(8)::x                   !!Argumento de la función.
  REAL(8)::aprox               !!Aproximación de la raíz.
  REAL(8)::tol                 !!Tolerancia de error.
  INTEGER::i                   !!Contador.

  !Explicamos al usuario las funciones del programa
  PRINT*,"Hola, bienvenido al programa del método de Newton-Rhapson modificado,"
  PRINT*,"donde aproximaremos la raíz de la función"
  PRINT*,"x**4 - 6x**3 + 12x**2 + 10x + 3"
  PRINT*,"utilizando el método de Newton-Rhapson modificado."

  !Establecemos nuestra tolerancia de error
  tol = 1e-9
  
  !Establecemos una raíz aproximada  donde evaluaremos la función
  aprox = 2

  !Obtenemos nuestra aproximación inicial u
  u = f(aprox)/d1f(aprox)

  !Utilizamos la derivada de nuestra aproximación u para el paso siguiente
  du =((d1f(aprox)**2)-(f(aprox)*d2f(aprox)))/(d1f(aprox)**2)
  
  !Iniciamos un ciclo DO que aproxime las raíces
  DO i=0,1000
     x = aprox - u/du
     aprox = x
     u = f(aprox)/d1f(aprox)
     du =((d1f(aprox)**2)-(f(aprox)*d2f(aprox)))/(d1f(aprox)**2)
  END DO

  !Mostramos el resultado al usuario
  PRINT*,"Tu función tiene tres raíces múltiples cuyo valor es"
  PRINT*,du

     
END PROGRAM newtonmod

!Establecemos la función a la cual queremos aproximar sus raíces
FUNCTION f(x)
  REAL(8)::f                 !!Función a la cual buscamos aproximar sus raíces.
  REAL(8),INTENT(IN)::x      !!Argumento de la función.

  f = x**4 - 6*x**3 + 12*x**2 - 10*x +3

END FUNCTION f

!Establecemos la primera derivada de la función
FUNCTION d1f(x)
  REAL(8)::d1f                 !!Primera derivada de la función.
  REAL(8),INTENT(IN)::x      !!Argumento de la función.

  d1f = 4*x**3 - 18*x**2 + 24*x - 10

END FUNCTION d1f

!Establecemos la segunda derivada de la función
FUNCTION d2f(x)
  REAL(8)::d2f                 !!Segunda derivada de la función.
  REAL(8),INTENT(IN)::x        !!Argumento de la función.

  d2f = 12*x**2 - 36*x + 24

END FUNCTION d2f


