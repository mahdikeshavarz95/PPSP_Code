
************* Sets & Indice *************
sets
         i index for projects /p1*P60/
         k index for project phase /1*20/
         m index for project mode /m0,m1,m2/
         t index for time period /1*45/;

alias(i,j);
alias(k,u,up);
alias(t,tp,tz);
alias(t,tt);

$call gdxxrw.exe PPSP.xlsx par=h rng=Sheet3!A2:BI62 rdim=1 cdim=1  par=e rng=Sheet4!A2:BI62 rdim=1 cdim=1  par=dd rng=sheet2!AP2:BJ62 rdim=1 cdim=1  par=Re rng=sheet2!A2:E461 rdim=2 cdim=1  par=c rng=sheet2!G2:K461 rdim=2 cdim=1  par=d rng=sheet2!M2:Q461 rdim=2 cdim=1  par=sv rng=sheet2!T2:AN12 rdim=1 cdim=1
Parameter h(i,j),e(i,j), dd(i,k), Re(i,k,m) , c(i,k,m) , d(i,k,m) , sv(i,k)
$gdxin PPSP.gdx
$load h, e ,dd , Re , c , d , sv,
$gdxin

************* Data *************
scalars
v initial budget /150/
r inter-period inflation rate /0.02/
L Big M /200000/
tpc total period counts /45/;

parameters
f(j) number of phases for project j /p1 4, p2 3, p3 3, p4 6, p5 10, p6 3, p7 10, p8 15, p9 15, P10 3,
                                     p11 2, p12 3, p13 4, p14 7, p15 10, p16 20, p17 18, p18 20, p19 10,
                                     p20 15, p21 8, p22 10, p23 12, p24 5, p25 5, p26 10, p27 10, p28 15,
                                     p29 12, p30 20, p31 6, p32 16, p33 4, p34 4, p35 5, p36 5, p37 6,
                                     p38 3, p39 7, p40 7, p41 3, p42 4, p43 5, p44 5, p45 5, p46 4, p47 5,
                                     p48 3, p49 6, p50 5, p51 4, p52 6, p53 8, p54 12, p55 3, p56 8, p57 7,
                                     p58 6, p59 5, p60 4/


************* Variables *************
variable
z1 objective function1
z2 objective function2

binary variable av(i,k)

binary variable x(i,k,m,t)
*equals 1 if phase k of project i starts at period t in mode m

binary variable y(i)
* equals 1 if project i ever be selected

positive variable bs(t) surplus budget at period t

positive variable td(i,k)
*delay or tardiness

************* Equations *************
equations
ObjectiveFunction1
ObjectiveFunction2
co3(i,j)
co4(i,j)
co5(i,j)
co6(i,j)
co7(i,k)
co8(i,k)
co9(i,k)
co10(i)
co11(i,u)
co12(i,k)
co13
co14(t)
co15(i)

co16_1(i,k)
co16_2(i,k)
co16_3(i,k)
co16_4(i,k)
co16_5(i,k)
co16_6(i,k)
* equation 19 to 24 in the paper
;

ObjectiveFunction1    .. z1=e= sum(t$(ord(t)=tpc), bs(t));


ObjectiveFunction2    .. z2=e= sum((i,k), td(i,k)/10);

co3(i,j)$(h(i,j)=1)  .. sum((k,m,t)$(ord(k)=1 and ord(m)<=2), ord(t) * x(i,k,m,t) )
                        + L*(1-y(i))
                        =g= sum( (k,m,t)$(ord(k)=f(j) and ord(m)<=2), x(j,k,m,t)*(ord(t)+d(j,k,m) ) );

co4(i,j)$(h(i,j)=1)  ..sum((k,m,t)$(ord(k)=f(j) and ord(m)<=2), x(j,k,m,t) )
                        =g=
                        sum((k,m,t)$(ord(k)=1 and ord(m)<=2),x(i,k,m,t) )  ;

co5(i,j)$(h(i,j)=1)    .. sum( (k,m,t)$(ord(m)<=2 and ord(k)=1),x(i,k,m,t) )
                        =L= L*( 1-SUM( (k,m,t)$(ord(m)=3 and ord(k)<=f(j)), x(j,k,m,t) ) );

co6(i,j)$(e(i,j)=1)   .. sum( (k,m,t)$(ord(m)<=2 and ord(k)=1), x(i,k,m,t) )
                         +sum( (k,m,t)$(ord(m)<=2 and ord(k)=1),x(j,k,m,t))
                         =L=1;

co7(i,k)$(ord(k)<f(i))   .. sum((m,t),ord(t)*x(i,k+1,m,t))
                          =g= sum((m,t)$(ord(m)<=2),x(i,k,m,t)*(ord(t)+d(i,k,m)))
                          - L * sum( t, x(i,k,'m2',t) );

co8(i,k)$(ord(k)<=f(i)) .. sum((m,t), x(i,k,m,t)) =g= sum((m,t), x(i,k+1,m,t));

co9(i,k)$(ord(k)<=f(i))  .. sum((m,t),x(i,k,m,t)) =l= y(i);

co10(i)    .. sum( (k,m,t)$(ord(k)<=f(i) and ord(t)>=2 and ord(m)=3),x(i,k,m,t) ) =l= y(i);


co11(i,u)$(ord(u)<=f(i))   .. sum( (k,m,t),x(i,k,m,t)$(ord(k)>ord(u) and ord(k)<=f(i)) )
                             =l=
                             L*( 1-sum( (m,t)$(ord(m)=3),x(i,u,m,t) ) );

co12(i,k)$(ord(k)<f(i))   ..   sum(t, x(i,k+1,'m1',t) ) + sum(t, x(i,k+1,'m2',t) )
                                  =g= sum((m,t)$(ord(m)=2),x(i,k,m,t))
                                  -L*( sum(t, x(i,k,'m2',t) ) ) ;

co13      .. sum( (i,k,m,t)$(ord(k)=1 and ord(t)=1)
                 ,x(i,k,m,t)*c(i,k,m) ) + bs('1') =l= v ;

co14(t)$(ord(t)>1)   .. bs(t-1)*(1+r)
                       + sum( (i,k,m,tz)$(ord(m)<=2),x(i,k,m,tz)$(ord(tz)=ord(t)-d(i,k,m))*Re(i,k,m) )
                       + sum( (i,k,m)$(ord(m)=3),x(i,k,m,t)*sv(i,k) )
                       =e=
                       bs(t)+ sum( (i,k,m)$(ord(m)<=2), x(i,k,m,t)*c(i,k,m) ) ;

co15(i)       .. sum( (k,m,t)$(ord(k)<=f(i) and ord(m)<=2),x(i,k,m,t)*( d(i,k,m)+ord(t) ) ) =l= tpc ;


co16_1(i,k)$(ord(k)<=f(i))  .. 0-sum((m,t),(ord(t)+d(i,k,m)-dd(i,k))*x(i,k,m,t)) =l= L*av(i,k) ;
co16_2(i,k)$(ord(k)<=f(i))  .. sum((m,t),(ord(t)+d(i,k,m)-dd(i,k))*x(i,k,m,t))-0 =l= L*(1-av(i,k)) ;

co16_3(i,k)$(ord(k)<=f(i)) .. td(i,k)=g=0 ;
co16_4(i,k)$(ord(k)<=f(i)) .. td(i,k)=g= sum((m,t),(ord(t)+d(i,k,m)-dd(i,k))*x(i,k,m,t))  ;
co16_6(i,k)$(ord(k)<=f(i)) .. td(i,k)=l=0+L*(1-AV(i,k)) ;
co16_6(i,k)$(ord(k)<=f(i)) .. td(i,k)=l=sum((m,t),(ord(t)+d(i,k,m)-dd(i,k))*x(i,k,m,t))+L*AV(i,k) ;

************* Modeling & Solving *************
model ppsp /all/ ;
option limcol=50000;
option limrow=50000;
option optca=0, optcr=0;
option MIP=cplex;

*solve ppsp using mip max z1;
*display x.l, y.l, z1.l, td.l, av.l;

$STitle eps-constraint method

set kk set of objective functions /1,2/
alias(kk,kp)
;

Set k1(kk)  the first element of kk,
    km1(kk) all but the first elements of kk;

k1(kk)$(ord(kk)=1) = yes; km1(kk)=yes; km1(k1) = no;

Parameter

    maxobj(kk)  maximum value from the payoff table
    minobj(kk)  minimum value from the payoff table

    payoff(kk,kp) payoff table
;
Scalar
    rhs     right hand side of the constrained obj functions in eps-constraint
 ;

* generate peyoff table using lexicographic method

solve ppsp using mip max z1;
payoff('1','1')=z1.l;
z1.lo=z1.l;

solve ppsp using mip min z2;
payoff('1','2')=z2.l;
z1.lo=-inf;


solve ppsp using mip min z2;
payoff('2','2')=z2.l;
z2.up=z2.l
solve ppsp using mip max z1;
payoff('2','1')=z1.l;

minobj(kk)=smin(kp,payoff(kp,kk));
maxobj(kk)=smax(kp,payoff(kp,kk));
z1.lo=-inf;
z2.up=inf;

* gridpoints are calculated as the range (difference between max and min) of
* the 2nd objective function from the payoff table
$set ngridpoints 4
Set gg            grid points /g0*g%ngridpoints%/
    grid(kk,gg)    grid ;


grid(km1,gg) = yes;

Parameter
    gridrhs(kk,gg) rhs of eps-constraint at grid point
    step(kk)      step of grid points in objective functions
    ;


step(km1)   = (maxobj(km1)- minobj(km1))/%ngridpoints%;
gridrhs(grid(km1,gg)) = maxobj(km1) - (ord(gg)-1)*step(km1);


scalar delta /.00001/;
variable  s,z1a;
positive variable s;

Equations
   aobj aumented objective function
   acon augmented constraint
;

aobj.. z1a=e=z1+delta*s;
acon..   z2+s =e= rhs;

Model mod_payoff    / ppsp, acon,aobj / ;
parameter
zsol(gg,kk);


loop(gg,

rhs=gridrhs('2',gg);
solve mod_payoff using mip max z1a;
zsol(gg,'1')=z1.l;
zsol(gg,'2')=z2.l;


if (mod_payoff.modelstat<>%ModelStat.Optimal% and
      mod_payoff.modelstat<>%ModelStat.IntegerSolution%,
zsol(gg,'1')=inf;
zsol(gg,'2')=inf;);



 );


display payoff,minobj,maxobj,step,gridrhs,zsol;

*Code by Mahdi Keshavarz