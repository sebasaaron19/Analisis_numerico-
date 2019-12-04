  !!El objetivo de este programa es el de aproximar las raíces de una función
  !!utilizando el método de Diferencias Divididas Centradas (DDC).
  !!-------------------------------------------------------------------------

PROGRAM ddc 

  IMPLICIT NONE
  !Diccionario de variables
  REAL(8)::x                     !!Valor a evaluar en la función.
  REAL(8)::f                     !!Función a evaluar.
  REAL(8)::df                    !!Valor exacto de la derivada de la función a evaluar.
  REAL(8)::h                     !!Ancho de paso.
  REAL(8)::ddcf                  !!Diferencia dividida centrada.
  REAL(8),DIMENSION(0:10)::deriv !!Módulo que almacena los valores de las derivadas.
  REAL(8)::et                    !!Error de truncamiento.
  REAL(8)::er                    !!Error de redondeo.
  REAL(8)::error                 !!Error del aproximado.
  INTEGER::i                     !!Contador.!!Declaracion de variables


!Establecemos nuestro ancho de paso h inicial y el valor en x a evaluar.
h = 1
x = 0.434

!Establecemos el valor real de nuestra derivada
df = 1.144879869

!Calculamos la DDC con un ancho de paso h cada vez más pequeño
DO i=1,10
   h=h/(10*i)
   
   deriv(i)=ddcf(h,x)

   error = abs(deriv(i)-df)
   et = 0.02844*h**2/6
   er = error - et
  
   print*,deriv(i),error,et,er
END DO


END PROGRAM

!Establecemos la función que calcula la DDC de la función f
FUNCTION ddcf(h,x)
IMPLICIT NONE

REAL(8),INTENT(IN)::h,x
REAL(8)::ddcf,f

ddcf = (f(x+h)-f(x-h))/(2*h)

END FUNCTION

!Establecemos la función f a evaluar
FUNCTION f(x)
IMPLICIT NONE

REAL(8),INTENT(IN)::x
REAL(8)::f

f = x**cos(x)
END FUNCTION
