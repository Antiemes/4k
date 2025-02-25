#version 120

uniform float t;
#define PI 3.1415926538

float hash11(float p)
{
    p = fract(p * .1031);
    p *= p + 33.33;
    p *= p + p;
    return fract(p);
}

vec2 hash21(float p)
{
	vec3 p3 = fract(vec3(p) * vec3(.1031, .1030, .0973));
	p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}

vec3 hash31(float p)
{
   vec3 p3 = fract(vec3(p) * vec3(.1031, .1030, .0973));
   p3 += dot(p3, p3.yzx+33.33);
   return fract((p3.xxy+p3.yzz)*p3.zyx); 
}

float hash13(vec3 p3)
{
	p3  = fract(p3 * .1031);
    p3 += dot(p3, p3.zyx + 31.32);
    return fract((p3.x + p3.y) * p3.z);
}

vec2 scrot(vec2 uv, float sc, float rot)
{
    float s = sin(rot);
    float c = cos(rot);
    return (sc*uv)*mat2(c,s,-s,c);
}

float sdParallelogram(in vec2 p, float wi, float he, float sk)
{
    vec2 e = vec2(sk,he);
    p = (p.y<0.0)?-p:p;
    vec2  w = p - e; w.x -= clamp(w.x,-wi,wi);
    vec2  d = vec2(dot(w,w), -w.y);
    float s = p.x*e.y - p.y*e.x;
    p = (s<0.0)?-p:p;
    vec2  v = p - vec2(wi,0); v -= e*clamp(dot(v,e)/dot(e,e),-1.0,1.0);
    d = min( d, vec2(dot(v,v), wi*he-abs(s)));
    return sqrt(d.x)*sign(-d.y);
}

float rsdParallelogram(in vec2 p, float wi, float he, float sk, in float r)
{
  return sdParallelogram(p, wi, he, sk) - r;
}

float orsdParallelogram(in vec2 p, float wi, float he, float sk, in float r, float r2)
{
  return abs(rsdParallelogram(p, wi, he, sk, r)) - r2;
}

float sdOrientedBox(in vec2 p, in vec2 a, in vec2 b, float th)
{
    float l = length(b-a);
    vec2  d = (b-a)/l;
    vec2  q = (p-(a+b)*0.5);
          q = mat2(d.x,-d.y,d.y,d.x)*q;
          q = abs(q)-vec2(l,th)*0.5;
    return length(max(q,0.0)) + min(max(q.x,q.y),0.0);    
}

float sdTriangle(in vec2 p, in vec2 p0, in vec2 p1, in vec2 p2)
{
    vec2 e0 = p1-p0, e1 = p2-p1, e2 = p0-p2;
    vec2 v0 = p -p0, v1 = p -p1, v2 = p -p2;
    vec2 pq0 = v0 - e0*clamp( dot(v0,e0)/dot(e0,e0), 0.0, 1.0 );
    vec2 pq1 = v1 - e1*clamp( dot(v1,e1)/dot(e1,e1), 0.0, 1.0 );
    vec2 pq2 = v2 - e2*clamp( dot(v2,e2)/dot(e2,e2), 0.0, 1.0 );
    float s = sign( e0.x*e2.y - e0.y*e2.x );
    vec2 d = min(min(vec2(dot(pq0,pq0), s*(v0.x*e0.y-v0.y*e0.x)),
                     vec2(dot(pq1,pq1), s*(v1.x*e1.y-v1.y*e1.x))),
                     vec2(dot(pq2,pq2), s*(v2.x*e2.y-v2.y*e2.x)));
    return -sqrt(d.x)*sign(d.y);
}

/* discontinuous pseudorandom uniformly distributed in [-0.5, +0.5]^3 */
vec3 random3(vec3 c)
{
	float j = 4096.0*sin(dot(c,vec3(17.0, 59.4, 15.0)));
	vec3 r;
	r.z = fract(512.0*j);
	j *= .125;
	r.x = fract(512.0*j);
	j *= .125;
	r.y = fract(512.0*j);
	return r-0.5;
}

/* skew constants for 3d simplex functions */
const float F3 =  0.3333333;
const float G3 =  0.1666667;

/* 3d simplex noise */
float simplex3d(vec3 p) {
	 /* 1. find current tetrahedron T and it's four vertices */
	 /* s, s+i1, s+i2, s+1.0 - absolute skewed (integer) coordinates of T vertices */
	 /* x, x1, x2, x3 - unskewed coordinates of p relative to each of T vertices*/
	 
	 /* calculate s and x */
	 vec3 s = floor(p + dot(p, vec3(F3)));
	 vec3 x = p - s + dot(s, vec3(G3));
	 
	 /* calculate i1 and i2 */
	 vec3 e = step(vec3(0.0), x - x.yzx);
	 vec3 i1 = e*(1.0 - e.zxy);
	 vec3 i2 = 1.0 - e.zxy*(1.0 - e);
	 	
	 /* x1, x2, x3 */
	 vec3 x1 = x - i1 + G3;
	 vec3 x2 = x - i2 + 2.0*G3;
	 vec3 x3 = x - 1.0 + 3.0*G3;
	 
	 /* 2. find four surflets and store them in d */
	 vec4 w, d;
	 
	 /* calculate surflet weights */
	 w.x = dot(x, x);
	 w.y = dot(x1, x1);
	 w.z = dot(x2, x2);
	 w.w = dot(x3, x3);
	 
	 /* w fades from 0.6 at the center of the surflet to 0.0 at the margin */
	 w = max(0.6 - w, 0.0);
	 
	 /* calculate surflet components */
	 d.x = dot(random3(s), x);
	 d.y = dot(random3(s + i1), x1);
	 d.z = dot(random3(s + i2), x2);
	 d.w = dot(random3(s + 1.0), x3);
	 
	 /* multiply d by w^4 */
	 w *= w;
	 w *= w;
	 d *= w;
	 
	 /* 3. return the sum of the four surflets */
	 return dot(d, vec4(52.0));
}


float lcdmask(vec2 uv, bool sega, bool segb, bool segc, bool segd, bool sege, bool segf, bool segg)
{
    float d;
    uv = uv+vec2(.5, .5);
    d =        orsdParallelogram(uv-vec2(.5, .3), .12, .12, .035, .08, .06);
    d = min(d, orsdParallelogram(uv-vec2(.61, .7), .12, .12, .035, .08, .06));
    d = max(d,-sdOrientedBox(uv-vec2(.35, .19), vec2(0., 0.), vec2(-.15, -.2), .02)); //Lower left
    d = max(d,-sdOrientedBox(uv-vec2(.59, .19), vec2(0., 0.), vec2(.025, -.2), .02)); //Lower right
    d = max(d,-sdOrientedBox(uv-vec2(.77, .8), vec2(0., 0.), vec2(.1, .2), .02)); //Upper right
    d = max(d,-sdOrientedBox(uv-vec2(.53, .8), vec2(0., 0.), vec2(-.1, .2), .02)); //Upper left
    
    d = max(d,-sdOrientedBox(uv-vec2(.45, .57), vec2(0., 0.), vec2(-.14, -.04), .02)); //Mid left 1
    d = max(d,-sdOrientedBox(uv-vec2(.42, .4), vec2(0., 0.), vec2(-.14, .15), .02)); //Mid left 2
    d = max(d,-sdOrientedBox(uv-vec2(.83, .43), vec2(0., 0.), vec2(-.14, .15), .02)); //Mid right 1
    d = max(d,-sdOrientedBox(uv-vec2(.8, .46), vec2(0., 0.), vec2(-.14, -.04), .02)); //Mid right 2
    
    
    if (!sege) d = max(d,-sdTriangle(uv, vec2(.22, .03), vec2(.47, .35), vec2(0., .85))); //lower left
    if (!segd) d = max(d,-sdTriangle(uv, vec2(.20, .03), vec2(.55, .43), vec2(.61, .03))); //bottom
    if (!segc) d = max(d,-sdTriangle(uv, vec2(.95, .51), vec2(.57, .40), vec2(.62, -.05))); //lower right
    if (!segb) d = max(d,-sdTriangle(uv, vec2(.95, .3), vec2(.66, .60), vec2(.93, 1.1))); //upper right
    if (!sega) d = max(d,-sdTriangle(uv, vec2(.35, 1.1), vec2(.66, .59), vec2(.92, 1.04))); //top
    if (!segf) d = max(d,-sdTriangle(uv, vec2(.38, 1.09), vec2(.63, .62), vec2(.25, .51))); //upper left
    if (!segg)
    {
        d = max(d,-sdTriangle(uv, vec2(.82, .455), vec2(.65, .63), vec2(.28, .525))); //mid
        d = max(d,-sdTriangle(uv, vec2(.82, .465), vec2(.45, .36), vec2(.28, .535))); //mid
    }
    
    //d = min(d, sdTriangle(uv, vec2(1., 1.), vec2(.5, 1.), vec2(1., .5))); //
    //d = min(d, sdTriangle(uv, vec2(.0, .0), vec2(.5, 0.), vec2(0., .5))); //
    
    
    return d;
}


vec3 lcd(vec2 uv, vec2 pos, vec3 col1, vec3 col2, float rnd, float sc, float rot, float num)
{
    float sh=.005;
    uv = scrot(uv-pos, 1./sc, rot);
 	vec3 p3 = vec3(uv, t*0.025);
	
	float value1, value2;

	value1 = simplex3d(p3*24.0);
	value2 = simplex3d((p3+vec3(.1456, -.1242, .536345))*24.0);
	
	value1 = 0.5 + 0.8*value1;
	value2 = 0.5 + 0.8*value2;
    
    float c, d;
    
    bool sega=false, segb=false, segc=false, segd=false, sege=false, segf=false, segg=false;
    if (num < .5) {sega=segb=segc=segd=sege=segf=true;}
    else if (num < 1.5) {segb=segc=true;}
    else if (num < 2.5) {sega=segb=segg=sege=segd=true;}
    else if (num < 3.5) {sega=segb=segc=segd=segg=true;}
    else if (num < 4.5) {segf=segg=segb=segc=true;}
    else if (num < 5.5) {sega=segf=segg=segc=segd=true;}
    else if (num < 6.5) {sega=sege=segf=segg=segc=segd=true;}
    else if (num < 7.5) {sega=segb=segc=true;}
    else if (num < 8.5) {sega=segb=segc=segd=sege=segf=segg=true;}
    else if (num < 9.5) {sega=segb=segc=segd=segf=segg=true;}
    else if (num <10.5) {sega=sege=segf=segg=true;} //F
    else if (num <11.5) {segc=segd=sege=true;} //u
    else if (num <12.5) {segc=segg=sege=true;} //n
    else if (num <13.5) {segd=sege=segg=true;} //c
    else if (num <14.5) {segd=sege=segf=segg=true;} //t
    else if (num <15.5) {sege=true;} //i
    else if (num <16.5) {segc=segd=sege=segg=true;} //o
    else if (num <17.5) {segc=segg=sege=true;} //n
    
    d = lcdmask(uv, sega, segb, segc, segd, sege, segf, segg);
    
    c = 1.-smoothstep(.0-sh, .0+sh, d);

    vec3 r=col1;
    vec3 b=col2;
    vec3 col = vec3(hash13(vec3(uv*9873., t*99.45)));
    float tt = t + hash11(t*3452. + 9384. + num * 1345. + col1.x * 2356.);
    if (sin(tt)*cos(tt*3.234+.34)+sin(tt*.235)<rnd)
    {
        col = 0.1 * col + r*value1 + b*value2;
    }
    
    return col*c;
}

void main()
{
  vec2 iResolution = vec2(1920., 1080.);
	vec2 uv = (gl_FragCoord.xy - iResolution.xy / 2.) / iResolution.y;
	//vec2 uv = (gl_FragCoord.xy - vec2(960., 540.)) / 540.;
	//uv.x *= iResolution.x/iResolution.y;

  float bass = clamp(pow(sin((t+0.625)*PI), 30.)*5., 0., 1.);

  vec3 color = vec3(.0);
  vec2 pos;
  vec3 col1;
  vec3 col2;
  float rndamount;
  float scalefactor;
  float rotation;
  int numnrs;
  float dispnum[20];
  vec2 spacing;

  int mode;

  numnrs = 12;
  pos = vec2(0., 0.);
  spacing = vec2(0.);
  rotation = 0.;

  float sub = mod(t, 16.) / 16.;

  if (t < 15.)
  {
    mode = 0;
    if (t < 12)
    {
      numnrs = int(t);
    }
  }
  else if (t < 31.)
  {
    mode = 1;
  }
  else if (t < 47.)
  {
    mode = 2;
  }
  else if (t < 63.)
  {
    mode = 3;
  }
  else if (t < 79.)
  {
    mode = 4;
  }
  else mode = 3;


  //mode = 3;

  for (int i=0; i<10; i++) dispnum[i]=hash11(float(i) * 3256. + mode * 30.) * 10.;
  //dispnum mod(hash11(numbers * 7634. + 2983.)*40., 10.)  ));
  for (int i = 0; i<numnrs; i++)
  {
    if (mode == 0)
    {
      pos = vec2(0., 0.);
      col1 = hash31(float(i) * 8989. + 9843.);
      col2 = hash31(float(i) * 2349. + 1239.);
      spacing = vec2(0.);
      scalefactor = hash11(float(i)*534.) + .2 + .2*sin(t*(0.4+hash11(float(i)*275.)) + hash11(float(i)*345. + 445.));
      rotation = 0.;
      rndamount = 0.;
      if (sub>.4)
      {
        uv=scrot(uv, .6, .3);
      }
    }
    else if (mode == 1)
    {
      pos = vec2(-1., 0.);
      col1 = hash31(float(i) * 3562. + 6251.);
      col2 = hash31(float(i) * 2435. + 6317.);
      spacing = vec2(.5, .0);
      scalefactor = hash11(float(i)*534.) + .2 + .2*sin(t*(0.4+hash11(float(i)*275.)) + hash11(float(i)*345. + 445.));
      rotation = 0.;
      rndamount = 0.;
      if (sub > .6)
      {
        uv = scrot(uv, .6, -.4);
        pos = vec2(-.5, -.2);
        spacing = vec2(.1, .02);
        rotation = hash11(float(i)*1345.) + .2 + .2*sin(t*(0.4+hash11(float(i)*435.)) + hash11(float(i)*5134. + 934.));
      }
    }
    else if (mode == 2)
    {
      pos = vec2(-1., 0.);
      col1 = hash31(float(i) * 2106. + 4171.);
      col2 = hash31(float(i) * 5621. + 1239.);
      spacing = vec2(.5, .0);
      scalefactor = hash11(float(i)*534.) + .2 + .2*sin(t*(0.4+hash11(float(i)*275.)) + hash11(float(i)*345. + 445.));
      rotation = hash11(float(i)*8345. + 654.) - .5;
      rndamount = -3.0*bass + 1.5;
    }
    else if (mode == 3)
    {
      pos = vec2(-.6, -.3);
      col1 = hash31(float(i) * 3175. + 2192.);
      col2 = hash31(float(i) * 12506. + 1099.);
      spacing = vec2(.12, .05);
      scalefactor = hash11(float(i)*534.)*.8 + .3 + .15*sin(t*(0.4+hash11(float(i)*275.)) + hash11(float(i)*345. + 445.));
      scalefactor = mix(scalefactor, scalefactor * .15 + .3, sub);
      rotation = .4;
      rndamount = -3.0*bass + 1.5;
      dispnum[0] = 10.;
      dispnum[1] = 11.;
      dispnum[2] = 12.;
      dispnum[3] = 13.;
      dispnum[4] = 14.;
      dispnum[5] = 15.;
      dispnum[6] = 16.;
      dispnum[7] = 17.;

      dispnum[8] = 2.;
      dispnum[9] = 0.;
      dispnum[10] = 2.;
      dispnum[11] = 2.;
    }
    else if (mode == 4)
    {
      pos = vec2(-.6, -.3);
      col1 = hash31(float(i) * 2192. + 3175.);
      col2 = hash31(float(i) * 1099. + 12506.);
      spacing = vec2(.12, .05);
      scalefactor = hash11(float(i)*982.)*.8 + .3 + .15*sin(t*(0.4+hash11(float(i)*275.)) + hash11(float(i)*532. + 445.));
      scalefactor = mix(scalefactor, scalefactor * .15 + .3, sub);
      rotation = .4;
      rndamount = -3.0*bass + 1.5;
      dispnum[0] = 10.;
      dispnum[1] = 11.;
      dispnum[2] = 12.;
      dispnum[3] = 13.;
      dispnum[4] = 14.;
      dispnum[5] = 15.;
      dispnum[6] = 16.;
      dispnum[7] = 17.;

      dispnum[8] = 2.;
      dispnum[9] = 0.;
      dispnum[10] = 2.;
      dispnum[11] = 2.;
      if (sub > .6)
      {
        pos = vec2(-.2);
        uv = scrot(uv, .6, -.4);
        spacing = vec2(.02, .03);
        rotation = hash11(float(i)*1345.) + .2 + .2*sin(t*(0.4+hash11(float(i)*435.)) + hash11(float(i)*5134. + 934.));
      }

    }
    color = max(color, lcd(uv,
          //(hash21(numbers * 345.) - vec2(.5, .5)) * 5.,
          pos + spacing * float(i), col1, col2, rndamount, scalefactor, rotation, float(dispnum[i])));
  }

  gl_FragColor = vec4(color, 1.);
}
