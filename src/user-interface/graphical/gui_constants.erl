% Copyright (C) 2023-2023 Olivier Boudeville
%
% This file is part of the Ceylan-Oceanic library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Monday, March 6, 2023.


% @doc Module defining most of the MyriadGUI constants.
%
% Called by gui:generate_support_modules/0.
%
-module(gui_constants).



-export([ get_object_type_topic_spec/0, get_window_style_topic_spec/0,
		  get_frame_style_topic_spec/0, get_button_style_topic_spec/0,
		  get_sizer_flag_topic_spec/0, get_menu_item_id_topic_spec/0,
		  get_bitmap_id_topic_spec/0, get_icon_name_id_topic_spec/0,
		  get_menu_item_kind_topic_spec/0, get_status_bar_style_topic_spec/0,
		  get_toolbar_style_topic_spec/0, get_event_type_topic_spec/0 ] ).


-export([ list_topic_spec_functions/0 ]).


% @doc Lists all the functions of this module that define a topic specification.
-spec list_topic_spec_functions() -> [ basic_utils:function_name() ].
list_topic_spec_functions() ->

	% Directly adapted from the first export define:
	[ get_object_type_topic_spec, get_window_style_topic_spec,
	  get_frame_style_topic_spec, get_button_style_topic_spec,
	  get_sizer_flag_topic_spec, get_menu_item_id_topic_spec,
	  get_bitmap_id_topic_spec, get_icon_name_id_topic_spec,
	  get_menu_item_kind_topic_spec, get_status_bar_style_topic_spec,
	  get_toolbar_style_topic_spec, get_event_type_topic_spec ].



% For the wx defines:
-include("gui_internal_defines.hrl").


% Implementation notes:
%
% These constants correspond to many of the ones that were defined in
% gui_wx_backend.erl.
% All topics could be maybe-ones to resist to unknown elements, yet for a GUI we
% prefer crashing.
%
% At least generally the first elements are MyriadGUI ones, and the second ones
% are wx ones.
%
% Quite often both conversion directions cannot be enabled, as different wx
% defines correspond actually to the same value (then only the first_to_second
% conversion direction is requested).
%
% For a topic T, we generate here gui_generated::get_{first,second}_for_T/1 (if
% both directions are enabled).


% Shorthands:

-type topic_spec( F, S ) :: const_bijective_topics:topic_spec( F, S ).

%-type bijective_table( F, S ) :: bijective_table:bijective_table( F, S ).

-type bit_mask() :: basic_utils:bit_mask().

-type myriad_object_type() :: gui:myriad_object_type().

-type wx_object_type() :: gui:wx_object_type().

-type window_style_opt() :: gui:window_style_opt().
-type frame_style_opt() :: gui:frame_style_opt().
-type button_style_opt() :: gui:button_style_opt().
-type sizer_flag_opt() :: gui:sizer_flag_opt().
-type menu_item_id() :: gui:menu_item_id().
-type menu_item_kind() :: gui:menu_item_kind().

-type status_bar_style() :: gui:status_bar_style().
-type toolbar_style() :: gui:toolbar_style().
-type bitmap_id_opt() :: gui:bitmap_id_opt().
-type icon_name_id() :: gui:icon_name_id().

-type wx_art_id() :: gui_wx_backend:wx_art_id().
-type wx_enum() :: gui_wx_backend:wx_enum().

-type wx_id() :: gui_id:wx_id().

-type event_type() :: gui_event:event_type().
-type wx_event_type() :: gui_event:wx_event_type().


% @doc Returns the two-way conversion specification for the 'object_type' topic.
%
% First elements are myriad_object_type(), second ones are wx_object_type().
%
-spec get_object_type_topic_spec() ->
		topic_spec( myriad_object_type(), wx_object_type() ).
get_object_type_topic_spec() ->

	% We use our recommended order (first set for internal, second one for
	% third-party).

	Entries = [
		{ object,                wxObject         },
		{ event_handler,         wxEvtHandler     },
		{ window,                wxWindow         },
		{ control,               wxControl        },
		{ button,                wxButton         },
		{ panel,                 wxPanel          },
		{ status_bar,            wxStatusBar      },
		{ top_level_window,      wxTopLevelWindow },
		{ dialog,                wxDialog         },
		{ frame,                 wxFrame          },
		{ sizer,                 wxSizer          },
		{ bitmap,                wxBitmap         },
		{ memory_device_context, wxMemoryDC       } ],

	% Thus strict look-up:
	{ object_type, Entries }.



% @doc Returns the two-way conversion specification for the 'window_style'
% topic.
%
-spec get_window_style_topic_spec() ->
						topic_spec( window_style_opt(), bit_mask() ).
get_window_style_topic_spec() ->

	Entries = [
		{ default_border,            ?wxBORDER_SIMPLE          },
		{ simple_border,             ?wxBORDER_SIMPLE          },
		{ sunken_border,             ?wxBORDER_SUNKEN          },
		{ raised_border,             ?wxBORDER_RAISED          },
		{ static_border,             ?wxSTATIC_BORDER          },
		{ theme_border,              ?wxBORDER_THEME           },
		{ no_border,                 ?wxBORDER_NONE            },
		{ double_border,             ?wxBORDER_DOUBLE          },
		{ transparent,               ?wxTRANSPARENT_WINDOW     },
		{ tab_traversable,           ?wxTAB_TRAVERSAL          },
		{ grab_all_keys,             ?wxWANTS_CHARS            },
		{ with_vertical_scrollbar,   ?wxVSCROLL                },
		{ with_horizontal_scrollbar, ?wxHSCROLL                },
		{ never_hide_scrollbars,     ?wxALWAYS_SHOW_SB         },
		{ clip_children,             ?wxCLIP_CHILDREN          },

		% Forces a complete redraw of the window whenever it is resized instead
		% of redrawing just the part of the window affected by resizing: (see
		% https://docs.wxwidgets.org/3.0/classwx_window.html)
		%
		{ full_repaint_on_resize,    ?wxFULL_REPAINT_ON_RESIZE } ],

	% Cannot be bijective, as a second_to_first function cannot be defined: some
	% wx defines collide, at least on some configurations (e.g. platforms; for
	% example ?wxBORDER_THEME may be equal to ?wxBORDER_DOUBLE):
	%
	{ window_style, Entries, _ElemLookup=strict,
	  _Direction=first_to_second }.



% @doc Returns the two-way conversion specification for the 'frame_style'
% topic.
%
-spec get_frame_style_topic_spec() ->
						topic_spec( frame_style_opt(), bit_mask() ).
get_frame_style_topic_spec() ->

	Entries = [
		{ default,         ?wxDEFAULT_FRAME_STYLE },
		{ caption,         ?wxCAPTION },

		% Useless 'minimize' (Windows-only);
		%{ minimize, ?wxMINIMIZE },

		{ minimize_icon,   ?wxMINIMIZE_BOX },

		% Useless 'maximize' (Windows-only);
		%{ maximize, ?wxMAXIMIZE_BOX},

		{ close_icon,      ?wxCLOSE_BOX },
		{ stay_on_top,     ?wxSTAY_ON_TOP },
		{ system_menu,     ?wxSYSTEM_MENU },
		{ resize_border,   ?wxRESIZE_BORDER },
		{ tool_window,     ?wxFRAME_TOOL_WINDOW },
		{ no_taskbar,      ?wxFRAME_NO_TASKBAR },
		{ float_on_parent, ?wxFRAME_FLOAT_ON_PARENT },
		{ shaped,          ?wxFRAME_SHAPED} ],

	{ frame_style, Entries }.



% @doc Returns the two-way conversion specification for the 'button_style'
% topic.
%
-spec get_button_style_topic_spec() ->
						topic_spec( button_style_opt(), bit_mask() ).
get_button_style_topic_spec() ->

	Entries = [
		{ default,          0              },
		{ left_justified,   ?wxBU_LEFT     },
		{ right_justified,  ?wxBU_RIGHT    },
		{ top_justified,    ?wxBU_TOP      },
		{ bottom_justified, ?wxBU_BOTTOM   },
		{ exact_fit,        ?wxBU_EXACTFIT }

		% Not implemented:
		%{ flat,          },

			  ],

	{ button_style, Entries }.



% @doc Returns the two-way conversion specification for the 'sizer_flag' topic.
-spec get_sizer_flag_topic_spec() ->
						topic_spec( sizer_flag_opt(), bit_mask() ).
get_sizer_flag_topic_spec() ->

	Entries = [
		{ default,                 0                               },
		{ top_border,              ?wxTOP                          },
		{ bottom_border,           ?wxBOTTOM                       },
		{ left_border,             ?wxLEFT                         },
		{ right_border,            ?wxRIGHT                        },
		{ all_borders,             ?wxALL                          },
		{ expand_fully,            ?wxEXPAND                       },
		{ expand_shaped,           ?wxSHAPED                       },
		{ fixed_size,              ?wxFIXED_MINSIZE                },
		{ counted_even_if_hidden,  ?wxRESERVE_SPACE_EVEN_IF_HIDDEN },
		{ align_center,            ?wxALIGN_CENTER                 },
		{ align_left,              ?wxALIGN_LEFT                   },
		{ align_right,             ?wxALIGN_RIGHT                  },
		{ align_top,               ?wxALIGN_TOP                    },
		{ align_bottom,            ?wxALIGN_BOTTOM                 },
		{ align_center_vertical,   ?wxALIGN_CENTER_VERTICAL        },
		{ align_center_horizontal, ?wxALIGN_CENTER_HORIZONTAL      } ],

	% Not a bijection, the second element '0' is present thrice:
	{ sizer_flag, Entries, _ElemLookup=strict,
	  _Direction=first_to_second }.



% @doc Returns the two-way maybe-conversion specification for the 'menu_item_id'
% topic.
%
% Converts wx standard menu item identifiers.
%
-spec get_menu_item_id_topic_spec() -> topic_spec( menu_item_id(), wx_id() ).
get_menu_item_id_topic_spec() ->

	Entries = [
		{ new_menu_item,             ?wxID_NEW               },
		{ open_menu_item,            ?wxID_OPEN              },
		{ close_menu_item,           ?wxID_CLOSE             },
		{ save_menu_item,            ?wxID_SAVE              },
		{ save_as_menu_item,         ?wxID_SAVEAS            },
		{ revert_to_saved_menu_item, ?wxID_REVERT_TO_SAVED   },
		{ undelete_menu_item,        ?wxID_UNDELETE          },
		{ print_menu_item,           ?wxID_PRINT             },
		{ preview_menu_item,         ?wxID_PREVIEW           },
		{ revert_menu_item,          ?wxID_REVERT            },
		{ edit_menu_item,            ?wxID_EDIT              },
		{ file_menu_item,            ?wxID_FILE              },
		{ properties_menu_item,      ?wxID_PROPERTIES        },
		{ cut_menu_item,             ?wxID_CUT               },
		{ copy_menu_item,            ?wxID_COPY              },
		{ paste_menu_item,           ?wxID_PASTE             },
		{ delete_menu_item,          ?wxID_DELETE            },
		{ find_menu_item,            ?wxID_FIND              },
		{ select_all_menu_item,      ?wxID_SELECTALL         },
		{ replace_menu_item,         ?wxID_REPLACE           },
		{ replace_all_menu_item,     ?wxID_REPLACE_ALL       },
		{ clear_menu_item,           ?wxID_CLEAR             },
		{ ok_menu_item,              ?wxID_OK                },
		{ cancel_menu_item,          ?wxID_CANCEL            },
		{ apply_menu_item,           ?wxID_APPLY             },
		{ yes_menu_item,             ?wxID_YES               },
		{ no_menu_item,              ?wxID_NO                },
		{ add_menu_item,             ?wxID_ADD               },
		{ convert_menu_item,         ?wxID_CONVERT           },
		{ execute_menu_item,         ?wxID_EXECUTE           },
		{ remove_menu_item,          ?wxID_REMOVE            },
		{ home_menu_item,            ?wxID_HOME              },
		{ refresh_menu_item,         ?wxID_REFRESH           },
		{ stop_menu_item,            ?wxID_STOP              },
		{ index_menu_item,           ?wxID_INDEX             },
		{ select_color_menu_item,    ?wxID_SELECT_COLOR      },
		{ select_font_menu_item,     ?wxID_SELECT_FONT       },
		{ forward_menu_item,         ?wxID_FORWARD           },
		{ backward_menu_item,        ?wxID_BACKWARD          },
		{ up_menu_item,              ?wxID_UP                },
		{ down_menu_item,            ?wxID_DOWN              },
		{ top_menu_item,             ?wxID_TOP               },
		{ bottom_menu_item,          ?wxID_BOTTOM            },
		{ first_menu_item,           ?wxID_FIRST             },
		{ last_menu_item,            ?wxID_LAST              },
		{ jump_to_menu_item,         ?wxID_JUMP_TO           },
		{ info_menu_item,            ?wxID_INFO              },
		{ zoom_factor_one,           ?wxID_ZOOM_100          },
		{ zoom_factor_fit,           ?wxID_ZOOM_FIT          },
		{ zoom_factor_in,            ?wxID_ZOOM_IN           },
		{ zoom_factor_out,           ?wxID_ZOOM_OUT          },
		{ undo_menu_item,            ?wxID_UNDO              },
		{ redo_menu_item,            ?wxID_REDO              },
		{ help_menu_item,            ?wxID_HELP              },
		{ preferences_menu_item,     ?wxID_PREFERENCES       },
		{ about_menu_item,           ?wxID_ABOUT             },
		{ floppy_menu_item,          ?wxID_FLOPPY            },
		{ hard_disk_menu_item,       ?wxID_HARDDISK          },
		{ network_menu_item,         ?wxID_NETWORK           },
		{ exit_menu_item,            ?wxID_EXIT              },
		{ undefined,                 ?wxID_ANY               } ],

	{ menu_item_id, Entries, _ElemLookup=maybe }.



% @doc Returns the two-way maybe-conversion specification for the 'bitmap_id'
% topic.
%
% Converts wx standard bitmap identifiers.
%
-spec get_bitmap_id_topic_spec() ->
						topic_spec( bitmap_id_opt(), wx_art_id() ).
get_bitmap_id_topic_spec() ->

	Entries = [
		{ error_bitmap,            "wxART_ERROR"            },
		{ question_bitmap,         "wxART_QUESTION"         },
		{ warning_bitmap,          "wxART_WARNING"          },
		{ information_bitmap,      "wxART_INFORMATION"      },
		{ add_bookmark_bitmap,     "wxART_ADD_BOOKMARK"     },
		{ delete_bookmark_bitmap,  "wxART_DEL_BOOKMARK"     },
		{ help_side_panel_bitmap,  "wxART_HELP_SIDE_PANEL"  },
		{ help_settings_bitmap,    "wxART_HELP_SETTINGS"    },
		{ help_book_bitmap,        "wxART_HELP_BOOK"        },
		{ help_folder_bitmap,      "wxART_HELP_FOLDER"      },
		{ help_page_bitmap,        "wxART_HELP_PAGE"        },
		{ go_back_bitmap,          "wxART_GO_BACK"          },
		{ go_forward_bitmap,       "wxART_GO_FORWARD"       },
		{ go_up_bitmap,            "wxART_GO_UP"            },
		{ go_down_bitmap,          "wxART_GO_DOWN"          },
		{ go_to_parent_bitmap,     "wxART_GO_TO_PARENT"     },
		{ go_home_bitmap,          "wxART_GO_HOME"          },
		{ goto_first_bitmap,       "wxART_GOTO_FIRST"       },
		{ goto_last_bitmap,        "wxART_GOTO_LAST"        },
		{ print_bitmap,            "wxART_PRINT"            },
		{ help_bitmap,             "wxART_HELP"             },
		{ tip_bitmap,              "wxART_TIP"              },
		{ report_view_bitmap,      "wxART_REPORT_VIEW"      },
		{ list_view_bitmap,        "wxART_LIST_VIEW"        },
		{ new_folder_bitmap,       "wxART_NEW_DIR"          },
		{ folder_bitmap,           "wxART_FOLDER"           },
		{ open_folder_bitmap,      "wxART_FOLDER_OPEN"      },
		{ go_folder_up_bitmap,     "wxART_GO_DIR_UP"        },
		{ executable_file_bitmap,  "wxART_EXECUTABLE_FILE"  },
		{ normal_file_bitmap,      "wxART_NORMAL_FILE"      },
		{ tick_mark_bitmap,        "wxART_TICK_MARK"        },
		{ cross_mark_bitmap,       "wxART_CROSS_MARK"       },
		{ missing_image_bitmap,    "wxART_MISSING_IMAGE"    },
		{ new_bitmap,              "wxART_NEW"              },
		{ file_open_bitmap,        "wxART_FILE_OPEN"        },
		{ file_save_bitmap,        "wxART_FILE_SAVE"        },
		{ file_save_as_bitmap,     "wxART_FILE_SAVE_AS"     },
		{ file_delete_bitmap,      "wxART_DELETE"           },
		{ copy_bitmap,             "wxART_COPY"             },
		{ cut_bitmap,              "wxART_CUT"              },
		{ paste_bitmap,            "wxART_PASTE"            },
		{ undo_bitmap,             "wxART_UNDO"             },
		{ redo_bitmap,             "wxART_REDO"             },
		{ plus_bitmap,             "wxART_PLUS"             },
		{ minus_bitmap,            "wxART_MINUS"            },
		{ close_bitmap,            "wxART_CLOSE"            },
		{ quit_bitmap,             "wxART_QUIT"             },
		{ find_bitmap,             "wxART_FIND"             },
		{ find_and_replace_bitmap, "wxART_FIND_AND_REPLACE" },
		{ full_screen_bitmap,      "wxART_FULL_SCREEN"      },
		{ edit_bitmap,             "wxART_EDIT"             },
		{ hard_disk_bitmap,        "wxART_HARDDISK"         },
		{ floppy_bitmap,           "wxART_FLOPPY"           },
		{ cdrom_bitmap,            "wxART_CDROM"            },
		{ removable_bitmap,        "wxART_REMOVABLE"        },
		{ backend_logo_bitmap,     "wxART_WX_LOGO"          } ],

	{ bitmap_id, Entries, _ElemLookup=maybe }.



% @doc Returns the two-way maybe-conversion specification for the 'icon_name_id'
% topic.
%
-spec get_icon_name_id_topic_spec() ->
						topic_spec( icon_name_id(), wx_art_id() ).
get_icon_name_id_topic_spec() ->

	% Based on the wx standard bitmap identifiers:
	Entries = [
		{ asterisk_icon,    ?wxICON_ASTERISK    },
		{ stop_icon,        ?wxICON_STOP        },
		{ information_icon, ?wxICON_INFORMATION },
		{ question_icon,    ?wxICON_QUESTION    },
		{ error_icon,       ?wxICON_ERROR       },
		{ warning_icon,     ?wxICON_WARNING     },
		{ hand_icon,        ?wxICON_HAND        },
		{ exclamation_icon, ?wxICON_EXCLAMATION } ],

	% Not a bijection, the element '512' is present thrice, the element '256'
	% and '2048' are present twice:
	%
	{ icon_name_id, Entries, _ElemLookup=maybe, _Direction=first_to_second }.



% @doc Returns the two-way conversion specification for the 'menu_item_kind'
% topic.
%
-spec get_menu_item_kind_topic_spec() ->
						topic_spec( menu_item_kind(), wx_enum() ).
get_menu_item_kind_topic_spec() ->

	Entries = [
		{ normal,    ?wxITEM_NORMAL    },
		{ toggle,    ?wxITEM_CHECK     },
		{ radio,     ?wxITEM_RADIO     },
		{ separator, ?wxITEM_SEPARATOR },
		{ dropdown,  ?wxITEM_DROPDOWN  } ],

	% No ?wxITEM_MAX

	{ menu_item_kind, Entries }.


% @doc Returns the two-way conversion specification for the 'status_bar_style'
% topic.
%
-spec get_status_bar_style_topic_spec() ->
						topic_spec( status_bar_style(), wx_enum() ).
get_status_bar_style_topic_spec() ->

	Entries = [
		{ normal, ?wxSB_NORMAL },
		{ flat,   ?wxSB_FLAT   },
		{ raised, ?wxSB_RAISED },
		{ sunken, ?wxSB_SUNKEN } ],

	{ status_bar_style, Entries }.



% @doc Returns the two-way conversion specification for the 'toolbar_style'
% topic.
%
-spec get_toolbar_style_topic_spec() ->
						topic_spec( toolbar_style(), wx_enum() ).
get_toolbar_style_topic_spec() ->

	Entries = [
		{ top,               ?wxTB_TOP           },
		{ bottom,            ?wxTB_BOTTOM        },
		{ left,              ?wxTB_VERTICAL      },
		{ right,             ?wxTB_RIGHT         },
		{ flat,              ?wxTB_FLAT          },
		{ dockable,          ?wxTB_DOCKABLE      },
		{ no_icons,          ?wxTB_NOICONS       },
		{ text,              ?wxTB_TEXT          },
		{ no_divider,        ?wxTB_NODIVIDER     },
		{ no_align,          ?wxTB_NOALIGN       },
		{ horizontal_layout, ?wxTB_HORZ_LAYOUT   },
		{ no_tooltips,       ?wxTB_NO_TOOLTIPS   },

		% Warning: ?wxTB_DEFAULT_STYLE is not a constant, it is actually a call:
		% 'wxe_util:get_const(wxTB_DEFAULT_STYLE)', which must moreover be
		% evaluated after wx is started (otherwise it fails in
		% 'persistent_term:get(wx_consts)'). We replaced it as the constant it
		% resolves to (at least in our setting):
		%
		%{ default,           ?wxTB_DEFAULT_STYLE } ],
		{ default,           4 } ],

	% Not a bijection, the element '4' is present twice:
	{ toolbar_style, Entries, _ElemLookup=strict, _Direction=first_to_second }.



% @doc Returns the two-way conversion specification for the 'event_type' topic.
-spec get_event_type_topic_spec() ->
						topic_spec( event_type(), wx_event_type() ).
get_event_type_topic_spec() ->

	Entries = [

		% Mouse section:

		{ onMouseMoved, motion },

		{ onMouseLeftButtonPressed,       left_down   },
		{ onMouseLeftButtonReleased,      left_up     },
		{ onMouseLeftButtonDoubleClicked, left_dclick },

		{ onMouseMiddleButtonPressed,       middle_down   },
		{ onMouseMiddleButtonReleased,      middle_up     },
		{ onMouseMiddleButtonDoubleClicked, middle_dclick },

		{ onMouseRightButtonPressed,       right_down   },
		{ onMouseRightButtonReleased,      right_up     },
		{ onMouseRightButtonDoubleClicked, right_dclick },


		{ onMouseFourthButtonPressed,       aux1_down   },
		{ onMouseFourthButtonReleased,      aux1_up     },
		{ onMouseFourthButtonDoubleClicked, aux1_dclick },


		{ onMouseFifthButtonPressed,       aux2_down   },
		{ onMouseFifthButtonReleased,      aux2_up     },
		{ onMouseFifthButtonDoubleClicked, aux2_dclick },

		{ onMouseWheelScrolled, mousewheel },

		{ onMouseEnteredWindow, enter_window },
		{ onMouseLeftWindow,    leave_window },


		% Keyboard section:

		{ onCharEntered,     char      },
		{ onCharEnteredHook, char_hook },
		{ onKeyPressed,      key_down  },
		{ onKeyReleased,     key_up    },


		% Menu section/tool(bar) section:

		{ onItemSelected,     command_menu_selected },
		{ onToolbarEntered,   command_tool_enter    },
		{ onToolRightClicked, command_tool_rclicked },


		% Window section:

		{ onShown,         show },
		{ onResized,       size },
		{ onRepaintNeeded, paint },
		{ onButtonClicked, command_button_clicked },
		{ onWindowClosed,  close_window } ],

	{ event_type, Entries }.
