      program trubka
c     Purpose: For computing trajectories of the light beam, travelling trough a cylindrical enviroment
c     Algorithm author: Zajan Ondøej
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    13.3.2019
c     Last edit:  18.4.2019
c	  Inputs:  	n(1) - number of trajectories between the colisions
c 			  	r(1) - radius of the tube
c 				l(1) - length of the tube			
c	  Outputs: 'kq.txt' - file for storing the trajectories
c 	  Required subroutines: start_pos,start_vec,next_colision
      logical s						
	  integer n
      double precision a(3),st(3),r,l
	  data r/1.d0/ l/10.d0/ n/200000/
	  open(1,file='kq.txt',action='write')
	  write(1,"(1x,'T=K.t+Q, t_0 - parameter where light hits the wall!Zápis do souboru
     >ofthe tube')")
	  write(1,"(1x,'|',8x,'K_x',9x,'|',9x,'K_y',9x,'|',9x,'K_z',9x,'|'
     >,9x,'Q_x',9x,'|',9x,'Q_y',9x,'|',9x,'Q_z',9x,'|',9x,'t_0',9x,'|'
     >)")                                                             !Hlavièka souboru
	  call random_seed()
	  call start_pos(st,r)
      call start_vec(a,r,l)
	  do i=1,n
	  call next_colision(a,st,s,r,l)
	  enddo	
	  close(1)
	  end

      subroutine next_colision(k,st,s,r,l)
c 	  Purpose: Subr. for computing trajectories of the light beam, travelling trough a cylindrical environment
c     Algorithm author: Zajan Ondøej
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    13.3.2019
c     Last edit:  18.4.2019
c	  Inputs:  k(3) - directional vector
c			   st(3) - starting position 
c   		   r(1) - tube radius
c 			   l(1) - tube length
c	  Outputs: 
c 				k(3) - reflecting directional vector
c 				st(3) - starting position of the directional vector
c  				s(1) - logical value describing direction of the light 
c 				Peripheral number (write) 1 - for writing the parametric equations of the trajectories
c 	  Required subroutines: tecna_normala,normalize,projection,rotate
c 	  Required functions: is_eq,t_end,vec_angle
      implicit none
c     g-teèna,g_n-normála,q-startovní pozice,t-parametry srážky,
c     p1-3 zt2 zt1 pom -pomocné promìnné,t0-parametr srážky,
c 	  f-úhel rotace,n-osa rotace
	  logical s,is_eq
      double precision k(3),q(3),sq,x,r,st(3),t0,g(3),f,l,n(3),ra,d,de
     >gree,vec_size,vec_angle,pom(3),g_n(3),t_end
	  q(1)=st(1)
      q(2)=st(2)
      q(3)=st(3) 
	  t0=t_end(k,q,r)										          !urèí parametr t0 pro místo srážky
	  st(1)=k(1)*t0+q(1)											  !místo kde dojde ke srážce se stìnou
	  st(2)=k(2)*t0+q(2)
	  st(3)=k(3)*t0+q(3)											  !Zápis dat do souboru
	  write(1,"(1x,f21.5,' ',f21.5,' ',f21.5,' ',f21.5,' ',f21.5,' ',f
     >21.5,' ',f21.5)")k,q,t0
	  if(st(3).gt.q(3))then 										  !zjisti smìr dopøedu s=T, zpìt s=F,(beta(-(0;2pi))
	  s=.true.
	  else
	  s=.false.
	  endif   
	  call tecna_normala(g,g_n,st,r)
	  call normalize(g)
	  call normalize(g_n)
	  f=vec_angle(g_n,k)
	  call projection(pom,g,k)										  !projekce vektoru k na rovinu (z,teèna) zápis do pom
	  call normalize(pom)
	  call axb(g_n,pom,n)											  !vektorový souèin mezi normálou a projekcí
	  n=-n
	  call normalize(n)
	  f=2.d0*f
	  call rotate(n,f,k)											  !Rotace vektoru k o uhel f, podle n
      return
      end
	  
	  subroutine rotate(n,f,vektor)									  !osa,uhel,vektor
c     Purpose: General ratation matrix - ratating vector "vektor" according to axis "n" by angle "f" (rad) 
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    20.3.2019
c     Last edit:  23.3.2019
c     Inputs:	n(3) - rotation axis
c 				f(1) - rotation angle
c 				vektor(3) - rotating vector
c 	  Outputs:	vektor(3) - rotated vector
	  implicit none
	  double precision vektor(3),n(3),f,sq,x,pom(3)
	  integer i
	  sq(x)=x*x
	  pom(1)=(dcos(f)+sq(n(1))*(1-dcos(f)))*vektor(1)+(n(1)*n(2)*(1
     >-dcos(f))-n(3)*dsin(f))*vektor(2)+(n(1)*n(3)*(1-dcos(f))+n(2)*ds
     >in(f))*vektor(3)
	  pom(2)=(n(1)*n(2)*(1-dcos(f))+n(3)*dsin(f))*vektor(1)+(dcos(f
     >)+sq(n(2))*(1-dcos(f)))*vektor(2)+(n(2)*n(3)*(1-dcos(f))-n(1)*ds
     >in(f))*vektor(3)
	  pom(3)=(n(1)*n(3)*(1-dcos(f))-n(2)*dsin(f))*vektor(1)+(n(2)*n
     >(3)*(1-dcos(f))+n(1)*dsin(f))*vektor(2)+(dcos(f)+sq(n(3))*(1-dco
     >s(f)))*vektor(3) 
	  vektor=pom
	  return 
	  end
	  
	  subroutine projection(proj,g,st)								  !vysledek,teèna, end vektor
c     Purpose: Projection of vector "st" on plane created by "g" and "(0,0,1)"
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    5.4.2019
c     Last edit:  5.4.2019
c 	  Inputs: 	g(3) - tangent of the tube
c 				st(3) - projecting vector
c 	  Outputs:	st(3) - projected vector
      implicit none
	  double precision g(3),st(3),proj(3)
	  proj(1)=((st(1)*g(1)**2)/(g(1)**2+g(2)**2))+((st(2)*g(1)*g(2))/(
     >g(1)**2+g(2)**2))
      proj(2)=((st(1)*g(1)*g(2))/(g(1)**2+g(2)**2))+((st(2)*g(2)**2)/(
     >g(1)**2+g(2)**2))
	  proj(3)=st(3)
	  return
	  end
	  
	  subroutine normalize(x)										  !zjednotkuje vektor x
c     Purpose: Normalize the vector "x"
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    5.4.2019
c     Last edit:  5.4.2019
c     Inputs:	x(3) - normalizing vector
c 	  Outputs:	x(3) - normalized vector
c 	  Required functions: is_eq
	  implicit none
	  logical is_eq
	  double precision x(3),pom
	  if(.not.(is_eq(0.d0,x(1)).and.is_eq(0.d0,x(2)).and.is_eq(0.d0,x(
     >3))))then
	  pom=dsqrt(x(1)**2+x(2)**2+x(3)**2)
	  x(1)=x(1)/pom													  !pøedìlání na jednotkový vektor
	  x(2)=x(2)/pom
	  x(3)=x(3)/pom
	  endif
	  return 
	  end
	
	  double precision function vec_size(vec)
c 	  Purpose: Get the size of the vector "vec"
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    5.4.2019
c     Last edit:  5.4.2019
c 	  Inputs:	vec(3) - Given vector
c 	  Outputs:	vec_size - size of the given vector
	  double precision vec(3)
	  vec_size=dsqrt(vec(1)**2+vec(2)**2+vec(3)**2)
	  return 
	  end
	  
	  double precision function vec_angle(vec_1,vec_2)				  !0 až 180 stupòù
c     Purpose: Gen the angle beween the vector "vec_1" and "vec_2"
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej, Endrychová Alžbìta
c     Created:    5.4.2019
c     Last edit:  18.4.2019
c     Inputs:	vec_1(3) - first vector 
c 				vec_2(3) - second vector
c 	  Outputs:	vec_angle - angle between the vectors
	  double precision vec_1(3),vec_2(3),vec_size
	  vec_angle=dacos((vec_1(1)*vec_2(1)+vec_1(2)*vec_2(2)+vec_1(
     >3)*vec_2(3))/(vec_size(vec_1)*vec_size(vec_2)))
	  return 
	  end
	  
	  subroutine lk(g,p,n,k,coef)									  !lineární kombinace vektorù g,p,n|k
c     Purpose: Linear combination of the vector "k" by vectors "g", "p", "n" (a*g+b*p+c*n=k).
c     Algorithm author: Zajan Ondøej
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    12.4.2019
c     Last edit:  16.4.2019
c     Inputs: 	g(3) - first vector
c 				p(3) - second vector
c  				k(3) - third vector
c 	  Outputs:	coef(3) - combination of the vectors
	  implicit none
	  double precision g(3),n(3),p(3),k(3),coef(3),pom
	  pom=((g(3)*p(1)-p(3)*g(1))/(g(1)*p(2)-g(2)*p(1)));
	  coef(3)=(k(3)-(g(3)/g(1))*k(1)+pom*(k(2)-(g(2)/g(1))*k(1)))/(n(3
     >)-(g(3)/g(1))*n(1)+pom*(n(2)-(g(2)/g(1))*n(1)))
	  coef(2)=(k(2)-(g(2)/g(1))*k(1)-n(2)*coef(3)+(g(2)/g(1))*n(1)*coe
     >f(3))/(p(2)-(g(2)/g(1))*p(1))
	  coef(1)=(k(1)-coef(3)*n(1)-coef(2)*p(1))/(g(1))
	  return 
	  end
	  
	  double precision function t_end(k,q,r)
c     Purpose: Get the parameter "t_end" of the parametric equation T=k.t+q, where the light hits the wall of the tube
c     Algorithm author: Zajan Ondøej
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    13.3.2019
c     Last edit:  18.4.2019
c     Inputs:	r(1) - radius of the tube
c 				k(3) - directional vector
c 				q(3) - starting position
c 	  Outputs:	t_end(1) - the parameter of the colision
c 	  Required functions: is_eq
	  implicit none
	  double precision p1,p2,p3,zt1(3),zt2(3),t(2),q(3),k(3),r,x,sq
	  logical is_eq
	  sq(x)=x*x
	  p1=dsqrt(-sq(k(1))*sq(q(2))+sq(k(1))*sq(r)+2.d0*k(1)*k(2)*q(1)* !Najdi místo, kde se pøímka protíná s trubkou
     -q(2)-sq(k(2))*sq(q(1))+sq(k(2))*sq(r))
	  p2=(sq(k(1))+sq(k(2)))!2a
	  p3=(k(1)*q(1)+(k(2)*q(2)))!b
	  t(1)=-(p3+p1)/p2
	  zt1(1)=k(1)*t(1)+q(1)
	  zt1(2)=k(2)*t(1)+q(2)
	  zt1(3)=k(3)*t(1)+q(3)
	  t(2)=(-p3+p1)/p2
	  zt2(1)=k(1)*t(2)+q(1)
	  zt2(2)=k(2)*t(2)+q(2)
	  zt2(3)=k(3)*t(2)+q(3)															  	  
	  if(.not.is_eq(t(1),0.d0).and..not.is_eq(t(2),0.d0))then		  !obì t nenulové -> nezaèínám na stìnì / start
	  if(t(1).gt.0.d0)then									          !!LK vektoru vektoru st(t0) vektorem k+q, pokud vyjdou záporné koeficienty, hodnota t0 je nesprávná, prímka mírí na opacnou stranu pokud je t záporný
	  t_end=t(1)													  !pokud je t kladný, pak prímka mírí správným smerem -- platí pouze pro start
	  else
	  t_end=t(2)
	  endif
	  else															  !zaèínám na stìnì /nonstart
	  if(.not.(is_eq(zt1(3),q(3)).and.is_eq(zt1(2),q(2)).and.is_eq(zt1!pokud se pri parametru t1 rovná prunik s pocátecní pozicí, parametr t1 je nesprávný
     >(1),q(1))))then 												  
	  t_end=t(1)													  !t1 je správný parametr
	  else										 				      !t(2) je parametr pro srážku a t(1) je parametr pro startovní pozici
	  t_end=t(2)
	  endif
	  endif
	  return
	  end
	  
	  subroutine tecna_normala(g,g_n,st,r)
c 	  Purpose: Get the tangent and the normal of the tube on the position of the colision
c     Algorithm author: Zajan Ondøej
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    13.3.2019
c     Last edit:  18.4.2019
c     Inputs:	st(3) - position of the needed tangent
c 				r(1) - radius of the tube
c 	  Outputs:	g(3) - directional vector of the tangent
c 				g_n(3) - directonal vector of the normal
c 	  Required functions: is_eq
	  implicit none
	  double precision g(3),g_n(3),x,sq,st(3),r
	  logical is_eq
	  sq(x)=x*x
	  g(1)=1.d0-st(1)												  !posunutí do pocátku
	  g(3)=0!0	  
	  if(is_eq(sq(r),sq(st(1))))then								  !prípad kdy je tecna rovnobežná s osou y
	  g(1)=0.d0
	  g(2)=1.d0
	  else
	  if(st(2).gt.0.d0)then
	  g(2)=-((st(1))/(dsqrt(sq(r)-sq(st(1)))))*(1.d0-st(1))!-		  !bez - by to byla normála, úhel e <-90;90>
	  else 				
	  g(2)=((st(1))/(dsqrt(sq(r)-sq(st(1)))))*(1.d0-st(1))			 
	  endif
	  endif
	  g_n(1)=-g(2)													  !-normála
	  g_n(2)=g(1)
	  g_n(3)=0.d0
	  if((st(1)/g_n(1))<0.d0)g_n=-g_n							      !!Uprav normálu tak, aby vždy míøila smìrem k bodu st
	  return 
	  end
	  
	  subroutine axb(a,b,n)
c 	  Purpose:	Get the crossection of the "a" and "b" (axb=n)
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    5.4.2019
c     Last edit:  6.4.2019
c     Inputs:	a(3) - first vector
c 				b(3) - second vector
c 	  Outputs:	n(3) - crossection of the given vectors
	  implicit none
	  double precision a(3),b(3),n(3)
	  n(1)=a(2)*b(3)-a(3)*b(2)!n=g_n x pom
	  n(2)=a(3)*b(1)-a(1)*b(3)!Vektorový souèin mezi projekcí a normálou
	  n(3)=a(1)*b(2)-a(2)*b(1)	  
	  return 
	  end 
	  
      logical function is_eq(a,b)
c 	  Purpose:	Func. for determining if the two variables are equal
c     Algorithm author: Zajan Ondøej
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    14.3.2019
c     Last edit:  14.3.2019
c     Inputs:	a(1) - first variable
c 				b(1) - second variable
c 	  Outputs:	is_eq - return logial value, a==b => .true. || a!=b => .false. 
      implicit none
      double precision a,b
      if(dabs(a-b)<(epsilon(0.d0)*1000))then
          is_eq=.true.
      else
          is_eq=.false.
      endif
      return
      end
	  
	  double precision function degree(x)
c 	  Purpose:	Return angle "x(rad)" in degrees
c     Algorithm author: Zajan Ondøej
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    29.3.2019
c     Last edit:  29.3.2019
c     Inputs:	x(1) - value needs to be in rad
c 	  Outputs:	degree - return "x" in degrees
	  double precision pi,x
	  pi=3.1415926535897932384626433832795d0
	  degree=(x*180.d0)/pi
	  return
	  end
	  
	  double precision function rad(x)
c 	  Purpose:	Return angle "x(degrees)" in rads
c     Algorithm author: Zajan Ondøej
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    29.3.2019
c     Last edit:  29.3.2019
c     Inputs:	x(1) - value need to be in degrees
c 	  Outputs:	rad - return "x" in rads
	  double precision pi,x
	  pi=3.1415926535897932384626433832795d0
	  rad=(x*pi)/180.d0
	  return
	  end

      subroutine start_vec(a,r,l)
c 	  Purpose:	Get random starting directional vector "a"
c     Algorithm author: Zajan Ondøej
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    13.3.2019
c     Last edit:  5.4.2019
c     Inputs:	r(1) - radius of the tube
c 				l(1) - length of the tube
c 	  Outputs:	a(3) - starting vector
c 	  Required functions: is_eq
      implicit none
	  logical is_eq
      double precision a(3),p,r,l
	  do while(is_eq(a(1),0.d0).and.is_eq(a(2),0.d0).and.is_eq(a(3),0.!Aby všechny tri složky nebyly nulové
     >d0))
      call random_number(a(1))
      call random_number(a(2))
      call random_number(a(3))
	  enddo
	  a(1)=a(1)*r
	  call random_number(p)
	  if(p<0.5d0)a(1)=-a(1)
	  a(2)=a(2)*r
	  call random_number(p)
	  if(p<0.5d0)a(2)=-a(2)
	  a(3)=a(3)*l
      end

      subroutine start_pos(st,r)
c 	  Purpose: Get random starting position 
c     Algorithm author: Zajan Ondøej
c     Code Author: Zajan Ondøej
c     Code Editors: Zajan Ondøej
c     Created:    13.3.2019
c     Last edit:  20.3.2019
c     Inputs:	r(1) - radius of the tube
c 	  Outputs:	st(3) - coordinate of the starting position
      !sqrt(x^2+y^2)<r
      implicit none
      double precision st(3),p,r
      call random_number(st(1))!x v (-r;r)
      call random_number(p)!pro znaménko
      st(1)=st(1)*r;
      if(p<5.d-1)st(1)=-st(1)
      call random_number(st(2))
      st(2)=st(2)*dsqrt((r*r)-(st(1)*st(1)))
      call random_number(p)
      if(p<5.d-1)st(2)=-st(2)
	  st(3)=0.d0
      end
	  