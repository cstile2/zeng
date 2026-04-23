#version 410 core
out vec4 FragColor;

in vec3 f_normal;
in vec2 f_tex_coord;
in vec3 world_pos;
in vec4 FragPosLightSpace;

// material parameters
uniform vec3 albedo;
uniform sampler2D albedo_texture;
uniform float metallic;
uniform float roughness;
uniform float ao;

uniform vec3 sun_direction;

// lights
uniform vec3 light_positions[4];
uniform vec3 light_colors[4];

uniform sampler2D shadow_map_linear;

uniform vec3 cam_pos;

const float PI = 3.14159265359;

float DistributionGGX(vec3 N, vec3 H, float roughness)
{
    float a      = roughness*roughness;
    float a2     = a*a;
    float NdotH  = max(dot(N, H), 0.0);
    float NdotH2 = NdotH*NdotH;
	
    float num   = a2;
    float denom = (NdotH2 * (a2 - 1.0) + 1.0);
    denom = PI * denom * denom;
	
    return num / denom;
}
float GeometrySchlickGGX(float NdotV, float roughness)
{
    float r = (roughness + 1.0);
    float k = (r*r) / 8.0;

    float num   = NdotV;
    float denom = NdotV * (1.0 - k) + k;
	
    return num / denom;
}
float GeometrySmith(vec3 N, vec3 V, vec3 L, float roughness)
{
    float NdotV = max(dot(N, V), 0.0);
    float NdotL = max(dot(N, L), 0.0);
    float ggx2  = GeometrySchlickGGX(NdotV, roughness);
    float ggx1  = GeometrySchlickGGX(NdotL, roughness);
	
    return ggx1 * ggx2;
}
vec3 fresnelSchlick(float cosTheta, vec3 F0)
{
    return F0 + (1.0 - F0) * pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0);
}  

float shadow_map_calculation(vec4 fragPosLightSpace)
{
    // perform perspective divide
    vec3 projCoords = fragPosLightSpace.xyz / fragPosLightSpace.w;
    // transform to [0,1] range
    projCoords = projCoords * 0.5 + 0.5;
    // get closest depth value from light's perspective (using [0,1] range fragPosLight as coords)
    // float closestDepth = max(texture(shadow_map, projCoords.xy).r, texture(shadow_map_linear, projCoords.xy).r);
    float closestDepth = texture(shadow_map_linear, projCoords.xy).r;

    // get depth of current fragment from light's perspective
    float currentDepth = projCoords.z;
    // check whether current frag pos is in shadow
    // float shadow = currentDepth - 0.001 > closestDepth ? 1.0 : 0.0;
    float bias = max(0.005 * (1.0 - dot(normalize(f_normal), sun_direction)), 0.0005);
    float shadow = currentDepth - bias > closestDepth ? 1.0 : 0.0;

    if(projCoords.z > 1.0)
        shadow = 0.0;
    
    return shadow;
}  

void main()
{
    vec3 combined_albedo = texture(albedo_texture, f_tex_coord).xyz * albedo;
    float opacity = texture(albedo_texture, f_tex_coord).a;

    vec3 N = normalize(f_normal);
    vec3 V = normalize(cam_pos - world_pos);

    float fresnel = pow(1.0 - dot(N, V), 5.0);

    float dist = length(cam_pos - world_pos);

    vec3 F0 = vec3(0.04);
    F0 = mix(F0, combined_albedo, metallic);

    // reflectance equation
    vec3 Lo = vec3(0.0);
    for (int i = 0; i < 1; ++i)
    {
        // calculate per-light radiance
        // vec3 L = normalize(light_positions[i] - world_pos);
        vec3 L = sun_direction; // vec3(0, 1, 0);
        vec3 H = normalize(V + L);
        float distance    = length(light_positions[i] - world_pos);
        // float attenuation = 1.0 / (distance * distance);
        float attenuation = 1.0;
        vec3 radiance     = light_colors[i] * attenuation;
        
        // cook-torrance brdf
        float NDF = DistributionGGX(N, H, roughness);
        float G   = GeometrySmith(N, V, L, roughness);
        vec3 F    = fresnelSchlick(max(dot(H, V), 0.0), F0);
        
        vec3 kS = F;
        vec3 kD = vec3(1.0) - kS;
        kD *= 1.0 - metallic;
        
        vec3 numerator    = NDF * G * F;
        float denominator = 4.0 * max(dot(N, V), 0.0) * max(dot(N, L), 0.0) + 0.0001;
        vec3 specular     = numerator / denominator;
            
        // add to outgoing radiance Lo
        float NdotL = max(dot(N, L), 0.0);
        Lo += (kD * combined_albedo / PI + specular) * radiance * NdotL; 
    }   
  
    float shadow_map = shadow_map_calculation(FragPosLightSpace);  

    vec3 ambient = vec3(0.15) * combined_albedo * ao;
    vec3 color = ambient + Lo * (1.0 - shadow_map);
    // color = combined_albedo;

    // dist = dist * 0.001;
    // dist = dist / (dist + 1.0);
    // float fog_density = 0.2f;
    // float fog_t = 1.0 - exp(-dist * fog_density);
    // float fog_t = clamp(sqrt(dist)/4.0, 0.0, 1.0);
    // float fog_t = 1.0 - (20.0/(dist*dist));
    // fog_t = clamp(fog_t, 0.0, 1.0);
    // vec3 fog_color = vec3(0.01);//vec3(0.3,0.2,0.3);
    // color = mix(color, fog_color, fog_t);

    // color = color / (color + vec3(1.0)); // tone mapping
    float avg = (color.x + color.y + color.z)/3.0;
    color = color * 1.5 / (avg + 1.0);
    FragColor = vec4(color, opacity);
}