{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
type C=(Double,Double)
instance Num C where (x,y)*(z,t)=(z*x-y*t,y*z+x*t);(x,y)+(z,t)=(x+z,y+t);abs((x,y))=(sqrt(x*x+y*y),0.0)
a=27;b=79;c=(-2.0,-1.0);d=(1.0,1.0);e=(-1.064,-0.15);r((x,y))=x;i((x,y))=y
f::C->C->Int->Int
f c z 0=0;f c z n=if(r(abs(z))>2)then n else f c ((z*z)+c) (n-1)
h j k = map(\z->(f (z) (0,0) 32,(fst z>l - q/2)))[(x,y)|y<-[p,(p+((o-p)/a))..o],x<-[m,(m + q)..l]]where o=i k;p=i j;m=r j;l=r k;q=(l-m)/b;
u j k = concat(map v $ h j k)where v (i,p)=(" ░▒▓█"!!((i`div`3)`rem`5)):rst p;rst True="\n";rst _=""
main = putStrLn(im 0)where cl n (x,y)=let cs=(1.1**n-1) in ((x+cs*(r e))/cs+1,(y+cs*(i e))/cs+1);bl n=cl n c;tr n=cl n d;im n=u (bl n) (tr n)++"\x1b[H\x1b[25A"++im (n+1)
