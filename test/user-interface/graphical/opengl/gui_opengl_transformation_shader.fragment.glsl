/*
 * Simple fragment shader.
 *
 * Each fragment shader instance is to transform exactly one fragment (as
 * generated by the rasterization) into another that is based on a series of
 * colors and a single depth value.
 *
 * At least generally, this transformation should behave as a pure
 * (context-free) function.
 *
 * Refer to https://www.khronos.org/opengl/wiki/Fragment_Shader for more
 * information.
 *
 */


/*
 * The GLSL version now matches the one of OpenGL (3.3);
 * using the (default) core profile.
 *
 */
#version 330 core


// Refer to the corresponding vertex shader for these:
//in vec3 my_color;
in vec2 my_tex_coord;


/* User-defined output data, as three floating-point coordinates in [0.0, 1.0]
 * (see gui_color:render_rgb_color(); alpha of 1.0 implied) for the fragment of
 * interest; no layout specified here:
 *
 */
out vec4 my_output_color;


/* Instead of relying on the default texture unit (GL_TEXTURE0), the
 * application may, prior to binding any texture of interest, activate
 * a given texture unit (e.g. GL_TEXTURE5); then it may associate, based
 * on the uniform support, this sampler to the corresponding texture
 * location (i.e. 5).
 *
 */
uniform sampler2D my_texture_sampler;



void main()
{

	// For all fragments, the output color will be pure green:
	//my_output_color = vec3(0.0, 1.0, 0.0);

	// Built-in function for fragment shaders:
	my_output_color = texture(my_texture_sampler, my_tex_coord);

}