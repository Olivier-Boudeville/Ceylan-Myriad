/*
 * This is the MyriadGUI base, built-in vertex shader.
 *
 * Based on the myriadgui_vbo_layout uniform variable, this shader is able to
 * discover the structure of the vertex attributes it has to process, and apply
 * it.
 *
 * Each vertex shader instance is to transform exactly one vertex (received
 * from the vertex stream) into another.
 *
 * At least generally, this transformation should behave as a pure
 * (context-free) function.
 *
 * Refer to https://www.khronos.org/opengl/wiki/Vertex_Shader for more
 * information.
 *
 */


/*
 * The GLSL version now matches the one of OpenGL (3.3);
 * using the (default) core profile.
 *
 */
#version 330 core

// MyriadGUI base shader defines:
#include "gui_shader.glsl.h"


/* The conventionally-named uniform variable (see the
 * myriad_gui_vbo_layout_unif_name define) used by MyriadGUI in order that a
 * vertex shader knows the VBO layout, therefore the vertex attributes, that it
 * shall expect:
 *
 */
uniform uint myriad_gui_vbo_layout;


/* Input vertex data, different for all executions of this shader, based on the
 * value to which the myriadgui_vbo_layout uniform variable above is set by the
 * main program; refer to gui_shader:vbo_layout() for more details.
 *
 * Output is the gl_Position vec4.
 *
 */

layout (location = 0) in vec3 myriad_gui_input_vertex;
layout (location = 1) in vec3 myriad_gui_input_normal;
layout (location = 2) in vec3 myriad_gui_input_color;
layout (location = 3) in vec2 myriad_gui_input_texcoord;


void apply_vtx3() {

	/* This is an identity transformation, basically (so my_input_vertex is
	 * expected to be already in normalized device coordinates):
	 */
	gl_Position.xyz = myriad_gui_input_vertex;

}

void main(){

	/* gl_Position is a predefined vec4 output corresponding to the clip-space
	 * output position of the current vertex.
	 *
	 */

	switch (myriad_gui_vbo_layout) {

		case VTX3:
			apply_vtx3();
			break;

		// All other VBO layouts:
		default:
			break;

	}

	gl_Position.w = 1.0;

}
