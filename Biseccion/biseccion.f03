Real  Function f (x)
  Implicit none
  Real*8 :: x
  f=dlog(x*x)-0.7
  !Funcion a evaluar
End Function f

Program Biseccion
  Implicit none
  Real :: f           !Funcion
  Real*8 :: a, b, r   !,x     !intervalo y Posible valor de la raiz x=2
  Real*8 :: fa, fb, fr  !Evalucion de los intervalos
  Real*8 :: Er, k !Numero de iteraciones
 ! Integer :: n, i        !iteraciones
  
  
  Write (*,*) "Inserte un intervalo [a,b] donde b>a."
  Write (*,*) "Valor de a"
  Read *, a
  Write (*,*) "Valor de b"
  Read *, b
  Write (*,*) "Valor del Error Estimado"  
  Read *, Er


  fa=f(a)
  fb=f(b)
  !  k= log((b-a)/0.0001)/log(2.00)
  !  Write (*,*) fa, " | ", fb       !!!Debug!!!
  If (fa*fb < 0) Then 
     
     k= log((b-a)/Er)/log(2.00)
     Write (*,*) "Se realizaran alrededor de ", int(k)+1 , " iteraciones"
     
     Do while (k>0)
        r=(a+b)/2
        fr= f(r)
        
        If(fa*fr<0)Then 
           b=r
           !fb=fr
           k=k-1
        Else 
           a=r
           !fa=fr
           k= k-1
        End IF
        Write (*,*) "|" ,int(k+1)+1, " | ", r," | ", fr, " |"
     End Do
     
     write (*,*) "Valor de r  es ", r, " con un error aproximado de ",Er
     
  Else  
     Write (*,*) "No se puede realizar por fallar en alguna condición." ! ya que o es la raiz o no se encuentra el intervalo dado o es una raiz compleja."
  End If
  
  ! Er= (b-a)/(2**n)
  ! write (*,*) "Valor de r  es ", r, " con un error aproximado de ", Er
  ! write (*,*) "Valor es r ", Er
  
End Program Biseccion
