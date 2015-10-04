#version 420

in vec2 texcoord;

out vec4 f_out;

uniform sampler2D tex;

void main() {
  f_out = texture(tex, texcoord);
}
