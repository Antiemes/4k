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

float map( in vec3 p )
{
	p.x -= 17./2.;
	float d = 999999.;
	vec3 pp = p-vec3(17./2.,0.,0.);
	pp.xy = mod(p.xy,vec2(1.))-.5;
	d = min(d, box(pp,vec3(.3,.3,1.)) );
	d = max(d, p.x);
	d = max(d, -p.x-17.);
	d = max(d, p.y-4.);
	d = max(d, -p.y);
	d = max(d, -box(p+vec3(16.5,-3.5,0.), vec3(.4,.4,2.)) );
	d = max(d, -box(p+vec3(14.5,-3.5,0.), vec3(.4,.4,2.)) );
	d = max(d, -box(p+vec3(11.5,-3.5,0.), vec3(1.4,.4,2.)) );
	d = max(d, -box(p+vec3(5.5,-3.5,0.), vec3(2.4,.4,2.)) );
	d = max(d, -box(p+vec3(0.5,-3.5,0.), vec3(.4,.4,2.)) );
	d = max(d, -box(p+vec3(15.5,-1.5,0.), vec3(.4,1.4,2.)) );
	d = max(d, -box(p+vec3(13.5,-1.5,0.), vec3(.4,1.4,2.)) );
	d = max(d, -box(p+vec3(11.5,-1.5,0.), vec3(.4,1.4,2.)) );
	d = max(d, -box(p+vec3(6.5,-1.5,0.), vec3(.4,1.4,2.)) );
	d = max(d, -box(p+vec3(4.5,-1.5,0.), vec3(.4,1.4,2.)) );
	d = max(d, -box(p+vec3(9.,-.5,0.), vec3(.9,.4,2.)) );
	d = max(d, -box(p+vec3(9.,-2.5,0.), vec3(.9,.4,2.)) );
	d = max(d, -box(p+vec3(2.,-1.5,0.), vec3(.9,1.4,3.)) );
	d = max(d, -box(p+vec3(5.,-2.5,0.), vec3(.9,.4,3.)) );
	d = min(d, (flame(p+vec3(17./2.,0.,0.))));
	return d;
}

vec3 raymarch(in vec3 org, in vec3 dir)
{
	float d = 0.0, glow = 0.0, eps = 0.02;
	vec3  p = org;
	
	for(int i=0; i<64; i++)
	{
		d = map(p) + eps;
		p += d * dir;
		if( d<eps )
			break;
	}
	return p;
}
vec3 normal(in vec3 p)
{
    vec3 eps = vec3(0.01,0.0,0.0);
    return normalize(vec3(
        map(p+eps.xyy)-map(p-eps.xyy),
        map(p+eps.yxy)-map(p-eps.yxy),
        map(p+eps.yyx)-map(p-eps.yyx)
    ));
}
float ambiantOcclusion( in vec3 p, in vec3 n, in float d)
{
    float dlt = 0.1;
    float oc = 1.0;
    
    for(int i=1; i<=6; i++)
    {
		float dist = abs(map(p+n*dlt));
		dlt += dist;
		oc += map(p+n*dlt)+dist;
    }
    oc /= 6.;
    
    return 1. - exp(-oc*d);
}

void main()
{
  vec2 iResolution = vec2(1920., 1080.);
	vec2 v = -1.0 + 2.0 * gl_FragCoord.xy / iResolution.xy;
	v.x *= iResolution.x/iResolution.y;
	vec3 col = vec3(0.);
	//vec3 org = vec3(0.,2.5,8.);
	vec3 org = vec3(0.,7.5,8.);
	//vec3 org = vec3(0. + abs(sin(t * 1.5)) * 8.0 + t * 2.8 * 4.0, 13.0, 8.);
	vec3 dir = normalize( vec3( v.xy, -1.5+length(v)*.25 ) );


	vec3 p = raymarch(org,dir);
	vec3 n = normal(p);
	col = vec3(0.) ;
	float f = flame(p);
	if(f<.1)
	{
		col = vec3(1.,.5,.1);
	}
	else if(map(p)<.1)
	{
		col += ambiantOcclusion(p,-dir,1.5);
		col *= ambiantOcclusion(p,n,1.5);
		col += vec3(1.,.5,.1) / (.5+pow(f,2.));
	}
	gl_FragColor = vec4(col*min(t*.25,1.), 1.);
}
