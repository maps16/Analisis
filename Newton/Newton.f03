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
  Real :: xn,xi       ! Valor posible de la raiz
  Real :: Er,E     !Error de la raiz
  Integer :: n     !Iteraciones

  Write(*,*) "Cual es el primer punto a evaluar"
  Read *, x
  Write(*,*) "Error Esperado"
  Read *, E

  
  n=0
  g=f(x)
  Write(*,*) "|","Iteracion    ","|","Raiz Aprox        ","|","Raiz Evaluada     ","|","Error Estimado    ","|"
  Do while (g/=0)
     xi=x
     g=f(x)
     dg=df(x)
     xn= x-(g/dg)
     x=xn
     write (*,*) "|", n+1, "|", xn, "|", g,"|",abs(Er), "|"          
     Er=(xn-xi)/xn
     n=n+1 

     If (n==100 .OR. abs(Er)<E) Then                                  
        Exit
     End If
  End Do

  Write (*,*) "Error Obtenido: " ,abs(Er)
 
  






End Program Newton
