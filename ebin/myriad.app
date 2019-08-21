% Description of the Myriad OTP library application, typically used by rebar3.

% Note: if this file is named myriad.app, it is a *generated* file, whose source
% is conf/myriad.app.src.

% See also:
% - http://erlang.org/doc/man/app.html
% - https://learnyousomeerlang.com/building-otp-applications


{application, myriad,
 [{description, "Ceylan-Myriad, a generic-purpose Erlang toolbox, as an OTP application library here (see http://myriad.esperide.org)"},
  {vsn, "1.0.12"},

  % No process registered:
  {registered, []},

  {applications, [kernel,stdlib]},

  {env,[]},

  % Flat hierarchy in ebin here:
  {modules, [generate_password_app, password_generation, merge_app, merge_utils, ui, gui_color, gui, gui_event, gui_wx_backend, asset_tool_app, gui_opengl, gui_text, gui_canvas, text_ui, term_ui, text_utils, basic_utils, executable_utils, code_utils, id_utils, list_utils, random_utils, fsm_utils, rdf_utils, trace_utils, python_utils, script_utils, bin_utils, monitor_utils, otp_utils, language_utils, ring_utils, net_utils, test_facilities, most_basic_example_app, file_utils, cipher_utils, naming_utils, set_utils, rest_utils, sms_utils, unit_utils, app_facilities, time_utils, system_utils, csv_utils, java_utils, web_utils, graph_utils, linear_3D, linear_4D, linear_2D, bounding_box, rk4_solver, math_utils, linear, polygon, ast_record, cond_utils, ast_info, ast_type, ast_pattern, ast_value, myriad_parse_transform, ast_utils, type_utils, ast_bitstring, ast_clause, example_parse_transform, ast_expression, ast_base, ast_scan, ast_transform, meta_utils, ast_generation, ast_guard, ast_function, ast_map, hashtable, const_table, process_dictionary, tree, bijective_table, map_hashtable, lazy_hashtable, list_table, pair, tracked_hashtable, preferences, option_list]},

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
