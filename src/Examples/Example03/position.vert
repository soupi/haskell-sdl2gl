#version 150 core

in vec2 position;

void main()
{
  gl_Position = vec4(position, -0.5, 1.0);
}
