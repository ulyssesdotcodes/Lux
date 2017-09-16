uniform float uAspectRatio;

out vec4 fragColor;


float when_gt(float x, float y) {
  return max(sign(x - y), 0.0);
}

float when_lt(float x, float y) {
  return max(sign(y - x), 0.0);
}

float when_lt_gt(float x, float mi, float ma) {
  return when_gt(x, mi) * when_lt(x, ma);
}


void main()
{
    vec4 color = texture2D( sTD2DInputs[0], vUV.st );
    float wscale = min(1.77777, uAspectRatio);
    float hscale = min(0.5625, 1 / uAspectRatio);
    vec2 res = uTD2DInfos[0].res.zw;
    float wcrop = (res.x - res.y * wscale) * 0.5;
    float hcrop = (res.y - res.x * hscale) * 0.5;
    vec2 xy = vUV.st * res;
    vec2 cut = vec2(when_lt_gt(xy.x, wcrop, res.x - wcrop), when_lt_gt(xy.y, hcrop, res.y - hcrop));
    fragColor = color * cut.x * cut.y;
}