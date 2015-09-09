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
  Real :: x        !Valor a evaluar
  Real :: g, dg    !Para evaluar las funciones
  Real :: xn       ! Valor posible de la raiz
  Integer :: n     !Iteraciones

  Write(*,*) "Cual es el primer punto a evaluar"
  Read *, x
  
  n=0
  g=f(x)

  Do while (g/=0) 
     g=f(x)
     dg=df(x)
     xn= x-(g/dg)
     x=xn
     write (*,*) "|", n, "|", xn, "|", g,"|"
     n=n+1
     If(n==10000)  exit
  End do 
  






End Program Newton
