#version 120

uniform float t;
#define PI 3.1415926538

bool sc[128]=bool[128](
false, true,  true,  true,  false, true,  false, false, true,  true,  true,  false, false, true,  true,  false, 
true,  false, false, false, false, true,  false, true,  false, false, false, false, true,  false, false, true,  
true,  false, false, false, false, true,  false, true,  false, false, false, false, true,  false, false, true,  
true,  false, false, false, false, true,  false, true,  false, false, false, false, true,  false, false, true,  
true,  false, false, false, false, true,  false, true,  false, false, false, false, true,  true,  true,  true,  
true,  false, false, false, false, true,  false, true,  false, false, false, false, true,  false, false, true,  
true,  false, false, false, false, true,  false, true,  false, false, false, false, true,  false, false, true,  
false, true,  true,  true,  false, true,  false, false, true,  true,  true,  false, true,  false, false, true
);

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
    return uv*mat2(c,s,-s,c)*sc;
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
vec3 random3(vec3 c) {
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
    if (!segd) d = max(d,-sdTriangle(uv, vec2(.22, .03), vec2(.55, .43), vec2(.61, .03))); //bottom
    if (!segc) d = max(d,-sdTriangle(uv, vec2(.95, .5), vec2(.57, .39), vec2(.62, -.05))); //lower right
    if (!segb) d = max(d,-sdTriangle(uv, vec2(.95, .3), vec2(.67, .61), vec2(.93, 1.1))); //upper right
    if (!sega) d = max(d,-sdTriangle(uv, vec2(.37, 1.1), vec2(.66, .59), vec2(.93, 1.1))); //top
    if (!segf) d = max(d,-sdTriangle(uv, vec2(.37, 1.1), vec2(.62, .62), vec2(.25, .52))); //upper left
    if (!segg)
    {
        d = max(d,-sdTriangle(uv, vec2(.8, .455), vec2(.65, .62), vec2(.3, .525))); //mid
        d = max(d,-sdTriangle(uv, vec2(.8, .465), vec2(.45, .37), vec2(.3, .535))); //mid
    }
    
    //d = min(d, sdTriangle(uv, vec2(1., 1.), vec2(.5, 1.), vec2(1., .5))); //
    //d = min(d, sdTriangle(uv, vec2(.0, .0), vec2(.5, 0.), vec2(0., .5))); //
    
    
    return d;
}


vec3 lcd(vec2 uv, vec2 pos, vec3 col1, vec3 col2, float rnd, float sc, float rot, float num)
{
    float sh=.005;
    uv = scrot(uv, 1./sc, rot) - pos;
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
    float tt = t + hash11(t*3452. + 9384.);
    if (sin(tt)*cos(tt*3.234+.34)+sin(tt*.235)<rnd)
    {
        col = 0.1 * col + r*value1 + b*value2;
    }
    
    return col*c;
}

vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

//float box( vec3 p, vec3 s )
//{
//  return length(max(abs(p)-s,0.0))-.1;
//}
//float noise(vec3 p) //Thx to Las^Mercury
//{
//	vec3 i = floor(p);
//	vec4 a = dot(i, vec3(1., 57., 21.)) + vec4(0., 57., 21., 78.);
//	vec3 f = cos((p-i)*acos(-1.))*(-.5)+.5;
//	a = mix(sin(cos(a)*a),sin(cos(1.+a)*(1.+a)), f.x);
//	a.xy = mix(a.xz, a.yw, f.y);
//	return mix(a.x, a.y, f.z);
//}
//
//float sphere(vec3 p, vec4 spr)
//{
//	return length(spr.xyz-p) - spr.w;
//}
//
//float flame(vec3 p)
//{
//	float d = sphere(p*vec3(1.,.5,1.), vec4(3.,2.,.0,1.));
//	return d + (noise(p+vec3(.0,-t*4.,.0)) + noise(p*3.)*.5)*.15*(p.y-2.) ;
//}
//
//float map( in vec3 p, inout vec3 color)
//{
//  float bass = clamp(pow(sin((t+0.625)*PI), 30.)*5., 0., 1.);
//  
//  float d2;
//	p.x -= 17./2.;
//	float d = 999999.;
//	vec3 pp = p-vec3(17./2.,0.,0.);
//  vec3 pos = floor(p);
//	pp.xy = mod(p.xy,vec2(1.))-.5;
//
//  //float noise1 = noise(vec3(p.x+345.34, p.y*345.34, t*345435.34))*(1.-smoothstep(.0, .1, mod(t/1., 1.)));
//  //d = min(d, box(pp,vec3(.3,.3+0.5*noise1,1.)) );
//  d = min(d, box(pp,vec3(.3,.3,1.)) );
//
//
//	float h1 = noise(pos + vec3(657.345, 345.256, 2435.2435));
//    float h2 = noise(pos);
//    float hue = mix(h1, h2, abs(sin(t * PI - 1.)));
//    vec3 c1 = hsv2rgb(vec3(.5, 1.4, .25));
//    vec3 c2 = hsv2rgb(vec3(.64, 1.5, .4));
//    vec3 color2 = mix(c1, c2, smoothstep(.15, .85, noise(pos)));
//
//    color = hsv2rgb(vec3(hue, 2., .3));
//    
//    //color = mix(color, color2, pow(sin(t * 1.5), 5.));
//
//    int xx = int(mod(pos.x - 5., 20.));
//    int yy = int(pos.y - 9.);
//
//    if (xx >= 0 && xx < 16 && yy >= 0 && yy < 8)
//    {
//      int i = xx + (7 - yy) * 16;
//
//      if (sc[i])
//      {
//          //float ss = .3 + min(pow(sin(t * 1. * 4. - 4.0), 24.), .9) * .1;
//          float ss = .3 + bass*0.1;
//          d2 = box(pp, vec3(ss, ss, 1.1));
//          if (d2<d)
//          {
//            d=d2;
//            color = hsv2rgb(vec3(sin(t), .4, .9));
//          }
//      }
//    }
//
//
//  return d;
//}
//
//vec3 raymarch(in vec3 org, in vec3 dir)
//{
//	float d = 0.0, glow = 0.0, eps = 0.02;
//	vec3  p = org;
//  vec3 color;
//	
//	for(int i=0; i<64; i++)
//	{
//		d = map(p, color) + eps;
//		p += d * dir;
//		if( d<eps )
//			break;
//	}
//	return p;
//}
//vec3 normal(in vec3 p)
//{
//  vec3 color;
//    vec3 eps = vec3(0.01,0.0,0.0);
//    return normalize(vec3(
//        map(p+eps.xyy, color)-map(p-eps.xyy, color),
//        map(p+eps.yxy, color)-map(p-eps.yxy, color),
//        map(p+eps.yyx, color)-map(p-eps.yyx, color)
//    ));
//}
//float ambiantOcclusion( in vec3 p, in vec3 n, in float d)
//{
//    float dlt = 0.1;
//    float oc = 1.0;
//    vec3 color;
//    
//    for(int i=1; i<=6; i++)
//    {
//		float dist = abs(map(p+n*dlt, color));
//		dlt += dist;
//		oc += map(p+n*dlt, color)+dist;
//    }
//    oc /= 6.;
//    
//    return 1. - exp(-oc*d);
//}

void main()
{
  vec2 iResolution = vec2(1920., 1080.);
	vec2 uv = -1.0 + 2.0 * gl_FragCoord.xy / iResolution.xy;
	uv.x *= iResolution.x/iResolution.y;


/*

  //vec3 org = vec3(0.,2.5,8.);
	//vec3 org = vec3(0., 12., 8.);
	vec3 org = vec3(-2. + abs(sin(t * PI / 8.)) * 8.0 + t * PI, 12.0, 10.);
	vec3 dir = normalize( vec3( v.xy, -1.5+length(v)*.25 ) );


	vec3 p = raymarch(org,dir);
	vec3 n = normal(p);
	vec3 col;
	float f = flame(p);
	if(map(p, col)<.1)
	{
    col *= 1.5;
		col += ambiantOcclusion(p,-dir,1.5);
		col *= ambiantOcclusion(p,n,1.5);
		//col += vec3(1.,.5,.1) / (.5+pow(f,2.));
	}
  col = col;
	gl_FragColor = vec4(col*min(t*.25,1.), 1.);*/

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

  numnrs = 10;
  pos = vec2(-1., -.3);
  spacing = vec2(0.);
  dispnum[0] = 0.;
  dispnum[1] = 1.;
  float numbers = 23.4;
  col1 = hash31(numbers * 8989. + 9843.);
  col2 = hash31(numbers * 2349. + 1239.);
  scalefactor = 0.5+0.5*sin(t*hash11(numbers * 6457. + 545));
  rotation = 0.;

  mode = 0;


  for (int i=0; i<10; i++) dispnum[i]=float(i);
  //dispnum mod(hash11(numbers * 7634. + 2983.)*40., 10.)  ));
  for (int i = 0; i<numnrs; i++)
  {
    if (mode == 0)
    {
      scalefactor = hash11(float(i)*534.) + .2 + .2*sin(t*(0.4+hash11(float(i)*275.)) + hash11(float(i)*345. + 445.));
    }
    color = max(color, lcd(uv,
          //(hash21(numbers * 345.) - vec2(.5, .5)) * 5.,
          pos + spacing * float(i), col1, col2, rndamount, scalefactor, rotation, float(dispnum[i])));
  }


  gl_FragColor = vec4(color, 1.);
}
