clearvars
lenght=10; %z nejv?tšího dosahu paprsku to lze spo?ítat .. p?ed?lat
radius=1;   %z místa srážky to lze spo?ítat .. p?ed?lat
data=textread('kq.txt','','headerlines',2);

figure
hold on
circle(1,0);

for i=1:size(data,1)
line(data(i,1:3),data(i,4:6),data(i,7),'red');
end
i=1;

xlabel('x','Interpreter','latex');
ylabel('y','Interpreter','latex');
zlabel('z','Interpreter','latex');
grid on;

function line(k,q,t,color)
d=0:0.001:abs(t);
d=d*sign(t);

x=@(d)k(1)*(d)+q(1);
y=@(d)k(2)*(d)+q(2);
z=@(d)k(3)*(d)+q(3);

h1=plot3(x(d),y(d),z(d),color);
h2=scatter3(x(0),y(0),z(0),'X','green');
h3=scatter3(x(t),y(t),z(t),'X','blue');
h1.LineWidth=1;
h2.LineWidth=2;
h3.LineWidth=2;
end

function h = cylinder(r,l)
for i=0:(l/2):l
circle(r,i);
end
end

function h = circle(r,z)
t = 0:pi/50:2*pi;
s=size(t);
zp=zeros(s(2),1);
for i = 1 : s(2)
  zp(i) = z;
end
st = sin(t)*r;
ct = cos(t)*r;
plot3(st,ct,zp,'black')
end