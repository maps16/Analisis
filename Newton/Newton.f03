Real Function f(x)
  Implicit None
  Real :: x
  f= alog(x*x) -0.7
!Funcion en la que se evalua
End Function f

Real Function df(x)
  Implicit None
  Real:: x
  df= 2/x
  !Derivada de f
End Function df

Program Newton
  Implicit None
  Real :: f, df    !Funcion y su derivada
  Real :: x        !Valor a evaluar y posible raiz
  Real :: g, dg    !Para evaluar las funciones
  Real :: xi       ! Valor posible de la raiz
  Real :: Er,E     !Error de la raiz
  Integer :: n     !Iteraciones

  Write(*,*) "Cual es el primer punto a evaluar"
  Read *, x
  Write(*,*) "Error Esperado"
  Read *, E

  
  n=0
  g=f(x)

  Do while (g/=0) 
     g=f(x)
     dg=df(x)
     x=xi
     x= x-(g/dg)
!     x=xn
     Er=(x-xi)/x
     write (*,*) "|", n, "|", x, "|", g,"|",Er,"|"
     n=n+1
!     If(abs(Er)<E .OR. n==1000)
!     Exit
     If (n.EQ.100) Then
        Exit
     End If
!     n=n+1
  End Do

 
  






End Program Newton
