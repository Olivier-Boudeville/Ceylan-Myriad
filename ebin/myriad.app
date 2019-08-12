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
  {modules, [generate_password_app, password_generation, merge_app, merge_utils, example_parse_transform, ast_transform, ast_record, ast_bitstring, ast_utils, ast_base, ast_pattern, myriad_parse_transform, ast_value, ast_expression, ast_function, ast_type, ast_scan, ast_info, ast_clause, type_utils, cond_utils, meta_utils, ast_guard, ast_generation, ast_map, rest_utils, java_utils, csv_utils, time_utils, basic_utils, graph_utils, otp_utils, language_utils, ring_utils, fsm_utils, net_utils, id_utils, python_utils, file_utils, list_utils, text_utils, system_utils, executable_utils, random_utils, trace_utils, bin_utils, script_utils, monitor_utils, rdf_utils, app_facilities, unit_utils, test_facilities, set_utils, web_utils, naming_utils, most_basic_example_app, sms_utils, code_utils, cipher_utils, gui_wx_backend, gui_canvas, asset_tool_app, gui_text, gui, gui_color, gui_event, gui_opengl, ui, term_ui, text_ui, linear, linear_4D, math_utils, bounding_box, linear_3D, linear_2D, rk4_solver, polygon, map_hashtable, pair, lazy_hashtable, tree, option_list, process_dictionary, bijective_table, list_table, const_table, tracked_hashtable, preferences, hashtable]},

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
