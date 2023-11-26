% Description of the Myriad OTP library application, typically used by rebar3.

% Note: if this file is named myriad.app, it is a *generated* file, whose real
% source is conf/myriad.app.src, from which _build/lib/myriad/ebin/myriad.app is
% obtained and copied to ebin/myriad.app; finally src/myriad.app.src is a mere
% symlink to this last file, so we have:
%
% ./conf/myriad.app.src [only real source]
% ./_build/lib/myriad/ebin/myriad.app
% ./ebin/myriad.app
% ./src/myriad.app.src -> ../ebin/myriad.app
%
% For more information see the 'rebar3-create-app-file' make target and its
% associated comments.

% See also:
% - http://erlang.org/doc/man/app.html
% - https://learnyousomeerlang.com/building-otp-applications


{application, myriad,
 [{description, "Ceylan-Myriad, a generic-purpose Erlang toolbox, as an OTP application library here (see http://myriad.esperide.org)"},
  {vsn, "1.0.34"},

  % No process registered:
  {registered, []},

  {applications, [ kernel,
				   stdlib

				   % Even if using a JSON parser (see USE_JSON), no strict need
				   % to declare it here, as it can be discovered and used at
				   % runtime rather than at compile-time (and thus Myriad
				   % remains with no dependency):
				   %
				   %,jsx
				   %,jiffy

				 ]},

  {deps, [

	   % Even if enabling a JSON parser, no need to select a backend
	   % here (done at runtime; see comment above):
	   %
	   %{jsx, "~> 3.0"}
	   %{jiffy, "1.0.5"}

  ]},

  {env,[]},

  % Flat hierarchy in ebin here:
  {modules, [app_facilities, asset_tool_app, ast_base, ast_bitstring, ast_clause, ast_expression, ast_function, ast_generation, ast_guard, ast_info, ast_map, ast_pattern, ast_record, ast_scan, ast_transform, ast_type, ast_utils, ast_value, audio_utils, basic_utils, bijective_table, bin_utils, bounding_surface, bounding_volume, cipher_utils, code_utils, cond_utils, const_bijective_table, const_bijective_topics, const_table, csv_utils, email_utils, environment, example_parse_transform, executable_utils, file_utils, fsm_utils, generate_password_app, gltf_support, graph_utils, gui, gui_bitmap, gui_button, gui_canvas, gui_color, gui_constants, gui_dialog, gui_event, gui_font, gui_frame, gui_id, gui_image, gui_keyboard, gui_menu, gui_mouse, gui_opengl, gui_opengl_constants, gui_panel, gui_render, gui_shader, gui_sizer, gui_statusbar, gui_text, gui_texture, gui_toolbar, gui_widget, gui_window, gui_window_manager, gui_wx_backend, hashtable, hash_utils, id_utils, java_utils, json_utils, language_utils, lazy_hashtable, linear, linear_2D, linear_3D, linear_4D, list_table, list_utils, locale_utils, map_hashtable, math_utils, matrix, matrix2, matrix3, matrix4, merge_app, merge_utils, mesh, meta_utils, monitor_utils, myriad_build, myriad_parse_transform, naming_utils, net_utils, octree, option_list, otp_utils, pair, password_generation, physics_utils, plot_utils, point, point2, point3, point4, polygon, preferences, process_dictionary, process_utils, projection, protobuf_support, python_utils, quaternion, random_utils, rdf_utils, resource, rest_utils, ring_utils, rk4_solver, script_utils, set_utils, shell_utils, sms_utils, speech_support, system_utils, term_ui, test_facilities, text_ui, text_utils, time_utils, trace_bridge, trace_utils, tracked_hashtable, transform4, tree, type_utils, ui, unit_utils, vector, vector2, vector3, vector4, web_utils, xml_utils]},

  {licenses, ["Ceylan-Myriad is licensed by its author (Olivier Boudeville) under a disjunctive tri-license, giving you the choice of one of the three following sets of free software/open source licensing terms:
	- the Mozilla Public License (MPL), version 1.1 or later (very close to the former Erlang Public License, except aspects regarding Ericsson and/or the Swedish law)
	- the GNU General Public License (GPL), version 3.0 or later
	- the GNU Lesser General Public License (LGPL), version 3.0 or later"]},

  % Library application, not an active one, so no specific behaviour of its own:
  % {mod, {myriad_app,[]}}

  { links, [ {"Official website", "http://myriad.esperide.org" },
			 {"Github", "https://github.com/Olivier-Boudeville/Ceylan-Myriad"} ]}

  % By default we already excluded sources files (*.erl) that provide a
  % conditional support (i.e. that depend on third-party prerequisites) from the
  % 'modules' entry above, and it is sufficient:
  %
  %{exclude_files, [...]}

 ]}.
