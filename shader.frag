#version 120

uniform float t;

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

vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}
float box( vec3 p, vec3 s )
{
  return length(max(abs(p)-s,0.0))-.1;
}
float noise(vec3 p) //Thx to Las^Mercury
{
	vec3 i = floor(p);
	vec4 a = dot(i, vec3(1., 57., 21.)) + vec4(0., 57., 21., 78.);
	vec3 f = cos((p-i)*acos(-1.))*(-.5)+.5;
	a = mix(sin(cos(a)*a),sin(cos(1.+a)*(1.+a)), f.x);
	a.xy = mix(a.xz, a.yw, f.y);
	return mix(a.x, a.y, f.z);
}

float sphere(vec3 p, vec4 spr)
{
	return length(spr.xyz-p) - spr.w;
}

float flame(vec3 p)
{
	float d = sphere(p*vec3(1.,.5,1.), vec4(3.,2.,.0,1.));
	return d + (noise(p+vec3(.0,-t*4.,.0)) + noise(p*3.)*.5)*.15*(p.y-2.) ;
}

float map( in vec3 p, inout vec3 color)
{
  float d2;
	p.x -= 17./2.;
	float d = 999999.;
	vec3 pp = p-vec3(17./2.,0.,0.);
  vec3 pos = floor(p);
	pp.xy = mod(p.xy,vec2(1.))-.5;

  d = min(d, box(pp,vec3(.3,.3,1.)) );


	float h1 = noise(pos + vec3(657.345, 345.256, 2435.2435));
    float h2 = noise(pos);
    float h = mix(h1, h2, abs(sin(t * 1.5 - 1.)));
    vec3 c1 = hsv2rgb(vec3(.5, 1.4, .25));
    vec3 c2 = hsv2rgb(vec3(.64, 1.5, .4));
    vec3 color2 = mix(c1, c2, smoothstep(.15, .85, noise(pos)));

    color = hsv2rgb(vec3(h, 2., .3));
    
    //color = mix(color, color2, pow(sin(t * 1.5), 5.));

    int xx = int(mod(pos.x - 5., 20.));
    int yy = int(pos.y - 9.);

    if (xx >= 0 && xx < 16 && yy >= 0 && yy < 8)
    {
      int i = xx + (7 - yy) * 16;

      if (sc[i])
      {
          float ss = .3 + min(pow(sin(t * 1.5 * 4. - 4.0), 24.), .9) * .1;
          d2 = box(pp, vec3(ss, ss, 1.1));
          if (d2<d)
          {
            d=d2;
            color = hsv2rgb(vec3(sin(t), .4, .9));
          }
      }
    }


  return d;
}

vec3 raymarch(in vec3 org, in vec3 dir)
{
	float d = 0.0, glow = 0.0, eps = 0.02;
	vec3  p = org;
  vec3 color;
	
	for(int i=0; i<64; i++)
	{
		d = map(p, color) + eps;
		p += d * dir;
		if( d<eps )
			break;
	}
	return p;
}
vec3 normal(in vec3 p)
{
  vec3 color;
    vec3 eps = vec3(0.01,0.0,0.0);
    return normalize(vec3(
        map(p+eps.xyy, color)-map(p-eps.xyy, color),
        map(p+eps.yxy, color)-map(p-eps.yxy, color),
        map(p+eps.yyx, color)-map(p-eps.yyx, color)
    ));
}
float ambiantOcclusion( in vec3 p, in vec3 n, in float d)
{
    float dlt = 0.1;
    float oc = 1.0;
    vec3 color;
    
    for(int i=1; i<=6; i++)
    {
		float dist = abs(map(p+n*dlt, color));
		dlt += dist;
		oc += map(p+n*dlt, color)+dist;
    }
    oc /= 6.;
    
    return 1. - exp(-oc*d);
}

void main()
{
  vec2 iResolution = vec2(1920., 1080.);
	vec2 v = -1.0 + 2.0 * gl_FragCoord.xy / iResolution.xy;
	v.x *= iResolution.x/iResolution.y;
	//vec3 org = vec3(0.,2.5,8.);
	//vec3 org = vec3(0., 12., 8.);
	vec3 org = vec3(0. + abs(sin(t * 1.5 / 8.)) * 8.0 + t * 2.8 * 4.0 / 8., 12.0, 10.);
	vec3 dir = normalize( vec3( v.xy, -1.5+length(v)*.25 ) );


	vec3 p = raymarch(org,dir);
	vec3 n = normal(p);
	vec3 col;
	float f = flame(p);
	if(map(p, col)<.1)
	{
    col *= 0.5;
		col += ambiantOcclusion(p,-dir,1.5);
		col *= ambiantOcclusion(p,n,1.5);
		//col += vec3(1.,.5,.1) / (.5+pow(f,2.));
	}
	gl_FragColor = vec4(col*min(t*.25,1.), 1.);
}
