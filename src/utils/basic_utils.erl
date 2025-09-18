% Copyright (C) 2007-2025 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
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
% Creation date: July 1, 2007.

-module(basic_utils).

-moduledoc """
Gathering of various **convenient facilities of all sorts**.

See the `basic_utils_test` module for the corresponding test.
""".



-doc "Allows to count elements (positive integer, possibly zero).".
-type count() :: non_neg_integer().



-doc "Allows to count elements (strictly positive integer).".
-type non_null_count() :: pos_integer().



-doc """
Allows to count levels (e.g. indentation ones, nesting ones).

Starts at `0`.
""".
-type level() :: zero_index().



-doc "Describes an (Erlang, inter-process) messsage.".
-type message() :: any().



-doc "Describes a PID or a port.".
-type pid_or_port() :: pid() | port().



-doc "For tables.".
-type atom_key() :: atom().




% Error-related types.


-doc """
Term designating a reason (which may be any term).

Note: useful to have self-describing types.
""".
-type reason() :: any().


-doc "Describes the reason for a process exiting.".
-type exit_reason() :: reason().


-doc "Describes the reason for a process-level error.".
-type error_reason() :: reason().


-doc """
A synthetic description/identifier of a type of error, as a mere atom.

For example `file_not_found`.
""".
-type error_tag() :: atom().


-doc "Designates a plain string explaning an error.".
-type error_diagnosis() :: ustring().


-doc "Designates a binary string explaning an error.".
-type error_bin_diagnosis() :: bin_string().



-doc """
Designates an error type (a specific, simple error reason), when we know it is
an atom (often the first element of an error tuple), like `invalid_name`.
""".
-type error_type() :: atom().



-doc """
An error pseudo-tuple, that is an error tuple (e.g. `{invalid_name,1.0}`) or a
single error term (instead of a tuple with a single element), preferably an atom
(like `invalid_password`). Typically to be thrown.

See also: `throw_diagnosed/{1,2}`.
""".
-type error_tuploid() :: error_tuploid( error_reason() ).



-doc "To specify at least some information about the error type of a tuploid.".
-type error_tuploid( T ) :: type_utils:tuploid( T ).



-doc """
A textual description associated to an error (typically for richer traces).
""".
-type error_message() :: error_diagnosis() | error_bin_diagnosis().



-doc "An error with its textual diagnosis.".
-type diagnosed_error_reason() :: { error_tuploid(), error_message() }.



-doc "An error with its textual diagnosis.".
-type diagnosed_error_reason( T ) :: { error_tuploid( T ), error_message() }.



-doc "The most classical way of reporting an error.".
-type tagged_error() :: { 'error', error_reason() }.



-doc "A (possibly tagged) error term. For example `badarg`.".
-type error_term() :: tagged_error() | error_reason().



-doc "A (tagged) error term with a diagnosis.".
-type diagnosed_error_term() :: { 'error', diagnosed_error_reason() }.



-doc "A (tagged) error term with a diagnosis.".
-type diagnosed_error_term( T ) :: { 'error', diagnosed_error_reason( T ) }.



-doc """
Tells whether an operation succeeded; if not, an error reason is specified (as a
term).
""".
-type base_status() :: 'ok' | error_term().



-doc """
Tells whether an operation succeeded; if not, an error reason is specified (as a
pair).
""".
-type base_outcome() :: 'ok' | error_term().



% These *fallible*/* and all types may or may not be clearer forms that the
% complete types that they capture. To be used wisely.



-doc """
Return type for operations that may fail (with a sufficient likelihood that no
exception is to be raised then, thus the choice is left to the caller), and then
do not report any particular cause of failure.

The corresponding terms are thus of the form ``{ok, TSuccessValue}`` or
just ``error``.
""".
-type coarse_fallible( TSuccess ) :: successful( TSuccess ) | 'error'.



-doc """
Return type for operations that may fail (with a sufficient likelihood that no
exception is to be raised then, thus the choice is left to the caller), when
wanting to specify the error type as well.

The corresponding terms are thus of the form ``{ok, TSuccessValue}`` or
``{error, TFailureValue}``.
""".
-type fallible( TSuccess, TFailure ) ::
    successful( TSuccess ) | failing( TFailure ).


-doc """
Return type for operations that may fail (with a sufficient likelihood that no
exception is to be raised then, thus the choice is left to the caller).

The corresponding terms are thus of the form ``{ok, TSuccessValue}`` or
``{error, AnyFailureValue}``.

This is a rather convenient, commonly used type.
""".
-type fallible( TSuccess ) :: fallible( TSuccess, TFailure :: any() ).


-doc """
Return type for operations that may fail (with a sufficient likelihood that no
exception is to be raised then, thus the choice is left to the caller).

The corresponding terms are thus of the form ``{ok, AnySuccessValue}`` or
``{error, AnyFailureValue}``.
""".
-type fallible() :: fallible( TSuccess :: any() ).


-doc """
Return type for operations that may return `{ok,SuccessStr}` or
`{error,FailureStr}`.
""".
-type string_fallible() ::
    fallible( TSuccess :: ustring(), TFailure :: ustring() ).



-doc """
Return type for operations that may only succeed.

(typically useful for fallible-like signatures induced by a framework)
""".
-type successful( TSuccess ) :: { 'ok', TSuccess }.


-doc """
Return type for operations that may only succeed.

The corresponding terms are thus of the form ``{ok, AnySuccessValue}``.

(typically useful for fallible-like signatures induced by a framework)
""".
-type successful() :: successful( TSuccess :: any() ).



-doc """
Return type for operations that may only fail.

(typically useful for fallible-like signatures induced by a framework)
""".
-type failing( TFailure ) :: { 'error', TFailure }.


-doc """
Return type for operations that may only fail.

The corresponding terms are thus of the form ``{error, AnyFailureValue}``.

(typically useful for fallible-like signatures induced by a framework)
""".
-type failing() :: failing( TFailure :: any() ).



-doc """
A tagged error with information, like `{type_scanning_failed, {ErrStr,
ErrorLocation, TypeStr}}`.
""".
-type tagged_error_info( TErrorInfoTuploid ) ::
    { error_tag(), TErrorInfoTuploid }.


-doc "Defined for the clarity of user code.".
-type error_info_tuploid() :: tuploid().


-doc """
A tagged error with information, like `{type_scanning_failed, {ErrStr,
ErrorLocation, TypeStr}}`.

To provide better, optional error interpretation, for any given function
(e.g. `parse_type/*`) that returns a `tagged_error_info/0` term, a specific
textual diagnosis function
(e.g. `interpret_parse_type_error(tagged_error_info()) -> ustring()` may be
defined.

See `type_utils:{parse_type, interpret_parse_type_error}/1` for an example
thereof.
""".
-type tagged_error_info() :: tagged_error_info( error_info_tuploid() ).



-doc """
Return type for operations that may fail, allowing the caller to act based on
the different causes of errors.

Thus either `{ok,TSuccessValue}` or `{error, {ErrorTag, TErrorInfoTuploid}}`
like `{error, {type_scanning_failed, {ErrStr, ErrorLocation, TypeStr}}}`.

Having the error tag in a pair (rather than in a tuple of potentially variable
size) facilitates the caller-side error management. The tuploid, which is the
most relevant form to aggregate extra information, may be further interpreted by
the caller if needed.
""".
-type tagged_fallible( TSuccess, TErrorInfoTuploid ) ::
    fallible( TSuccess, tagged_error_info( TErrorInfoTuploid ) ).



-doc """
Return type for operations that may fail, allowing the caller to act based on
the different causes of errors.

Thus either `{ok,TSuccessValue}` or `{error, {ErrorTag, SomeErrorTuploid}}` like
`{error, {type_scanning_failed, {ErrStr, ErrorLocation, TypeStr}}}`.

Having the error tag in a pair (rather than in a tuple of potentially variable
size) facilitates the caller-side error management. The tuploid, which is the
most relevant form to aggregate extra information, may be further interpreted by
the caller if needed.
""".
-type tagged_fallible( TSuccess ) ::
    tagged_fallible( TSuccess, tuploid() ).



-doc """
Return type for operations that may fail, allowing the caller to act based on
the different causes of errors.

Thus either `{ok, AnySuccessValue}` or `{error, {ErrorTag, SomeErrorTuploid}}`
like `{error, {type_scanning_failed, {ErrStr, ErrorLocation, TypeStr}}}`.

Having the error tag in a pair (rather than in a tuple of potentially variable
size) facilitates the caller-side error management. The tuploid, which is the
most relevant form to aggregate extra information, may be further interpreted by
the caller if needed.
""".
-type tagged_fallible() :: tagged_fallible( TSuccess :: any() ).



-doc """
Thus either `{ok, TSuccessValue}` or `{error, {TuploidTFailureValue,ErrorMsg}}`.
""".
-type diagnosed_fallible( TSuccess, TFailure ) ::
	fallible( TSuccess, diagnosed_error_reason( TFailure ) ).


-doc """
Thus either `{ok,TSuccessValue}` or `{error, {ErrorTuploid,ErrorMsg}}`.
""".
-type diagnosed_fallible( TSuccess ) ::
    fallible( TSuccess, diagnosed_error_reason() ).


-doc """
Thus either `{ok,TValue}` or `{error, {ErrorAtomTag, ErrorMsgStr}}`
(e.g. `{error, {parsing_failed, "Syntax error in expression 'xxx': yyy"}}`).

A very flexible result value, allowing the caller to throw and/or trace errors,
selectively (based on the tag) and each time with a proper diagnosis.
""".
-type diagnosed_tagged_fallible( TSuccess ) ::
    fallible( TSuccess, diagnosed_error_reason() ).



-doc """
Describes a setting in terms of general error reporting.

Typically stored as a persistent term, and read and applied by the various error
reporting systems.

The default setting is `standard_ellipsed` (ligthest / most convenient,
generally sufficient); we recommend switching to `standard_ellipsed_file_full`
to debug tricky issues, and to `standard_and_file_ellipsed` in production.
""".
-type error_report_output() ::

     % Full report to be put on standard error output (only):
    'standard_full'

    % Ellipsed report on standard error output only; the default:
  | 'standard_ellipsed'

    % Ellipsed report on standard error output, and full version on file:
  | 'standard_ellipsed_file_full'

    % Ellipsed report on standard error output, and ellipsed (but less) on file:
  | 'standard_and_file_ellipsed'.



-doc """
To denote that a piece of data comes from the program boundaries (interfaces
with the outside word, possibly in link with the user) and thus may or may not
be of the expected type (as long as it has not been checked).

(opaque, unspecified type - yet not declared as `opaque` to avoid a compilation
warning telling it is "underspecified and therefore meaningless").
""".
-type external_data() :: term().



-doc "Designates data whose type and value have not been checked yet.".
-type unchecked_data() :: term().



-doc "Designates user-specified data (users shall not be trusted either).".
-type user_data() :: external_data().



-doc """
Designates an accumulator (of any type), to document typically fold-like
operations.

(useful for documentation purposes)
""".
-type accumulator() :: any().



-doc "A component of a version.".
-type version_number() :: non_neg_integer().



-doc "By default we consider that a version is a triplet of integers.".
-type version() :: three_digit_version().



-doc "Version as a pair of integers, typically `{MajorVersion, MinorVersion}`.".
-type two_digit_version() :: { version_number(), version_number() }.



-doc """
Version as a triplet of integers, typically `{MajorVersion, MinorVersion,
ReleaseVersion}`, or `{MajorVersion, Enhancements, BugFixes}`, or
`{MajorVersion, MinorVersion, Path}` (see semantic versioning,
[https://semver.org/]).
""".
-type three_digit_version() ::
	{ version_number(), version_number(), version_number() }.



-doc """
Version as a quadruplet of integers, typically `{MajorVersion, MinorVersion,
ReleaseVersion, BuildVersion}`.
""".
-type four_digit_version() :: { version_number(), version_number(),
								version_number(), version_number() }.


-doc "Any version number.".
-type any_version() :: two_digit_version() | three_digit_version()
					 | four_digit_version().



-doc """
For all non-null indices, i.e. the strictly positive ones, i.e. the ones that
start at `1`.

This is the convention that Myriad (and Erlang) enforce as much as possible.
""".
-type positive_index() :: pos_integer().



-doc """
For the indices that may be null, typically starting at zero (e.g. in some file
formats). Whenever possible, prefer `positive_index/0`.
""".
-type zero_index() :: non_neg_integer().



-doc """
To distinguish from the built-in type, which can be a parameterised module.
""".
-type module_name() :: atom().



-doc "Name of a function.".
-type function_name() :: atom().



-doc "Argument of a function.".
-type argument() :: any().


-doc """
Any kind of argument-level option.

For example: `foo:create(..., [basic_utils:option()])`.
""".
-type any_option() :: tagged_list:tagged_element().



% Shorthand for Module, Function, Arity:
%
% (commented-out, as mfa() is a builtin type; it cannot be redefined)
%
% -type mfa() :: { module_name(), function_name(), arity() }.



-doc "A command (module-function-arguments).".
-type command_spec() :: { module_name(), function_name(), [ argument() ] }.



-doc """
The name of a layer (e.g. `"Myriad"`).
""".
-type layer_name() :: ustring().



-doc "The name of a record.".
-type record_name() :: atom().



-doc "The name of a field of a record.".
-type field_name() :: atom().



-doc "To specify whether a given feature shall be enabled or not.".
-type activation_switch() :: 'enable' | 'disable'.



-doc "Possible outcome of a partial-order comparison of two elements.".
-type comparison_result() :: 'lower' | 'equal' | 'higher'.



-doc """
Compile-time execution target (not to be mixed up with `execution_context/0`).
""".
-type execution_target() :: 'development' | 'production'.



-doc """
Runtime-time execution context (not to be mixed up with `execution_target/0`,
although gathering the same values - but conveying a different meaning).
""".
-type execution_context() :: 'development' | 'production'.



-doc "The exception classes that can be raised.".
-type exception_class() :: 'throw' | 'exit' | 'error'.



-doc """
Exception term corresponding to a corresponding bound pattern, as in `catch
ExceptionClass:ExceptionPattern:StackTrace ->`.
""".
-type exception_term() :: term().



% i.e. byte():
-doc "The status code returned by a shell command.".
-type status_code() :: 0..255.



-doc """
Useful as a temporary type placeholder, during development (easy to grep and
eliminate afterwards).
""".
-type fixme() :: any().


-export_type([ count/0, non_null_count/0, level/0,
			   message/0, pid_or_port/0, atom_key/0,
			   reason/0, exit_reason/0, error_reason/0, error_tag/0,
			   error_diagnosis/0, error_bin_diagnosis/0,
			   error_type/0, error_tuploid/0, error_message/0,
			   diagnosed_error_reason/0, tagged_error/0,
			   error_term/0, diagnosed_error_term/0, diagnosed_error_term/1,
			   base_status/0, base_outcome/0,

			   coarse_fallible/1, fallible/0, fallible/1, fallible/2,
               string_fallible/0, successful/0, successful/1, failing/0,
               failing/1,

               tagged_error_info/0, tagged_error_info/1, error_info_tuploid/0,
               tagged_fallible/0, tagged_fallible/1, tagged_fallible/2,

			   diagnosed_fallible/1, diagnosed_fallible/2,
               diagnosed_tagged_fallible/1,
               error_report_output/0,

			   external_data/0, unchecked_data/0, user_data/0,
			   accumulator/0,
			   version_number/0, version/0, two_digit_version/0, any_version/0,
			   three_digit_version/0, four_digit_version/0,
			   positive_index/0, zero_index/0,
			   module_name/0, function_name/0, argument/0, any_option/0,
			   command_spec/0, layer_name/0, record_name/0, field_name/0,
			   activation_switch/0,
			   comparison_result/0, execution_target/0, execution_context/0,
			   exception_class/0, exception_term/0, status_code/0,
			   fixme/0 ]).




% Notification-related functions.
-export([ speak/1, notify_user/1, notify_user/2 ]).



% Message-related functions.
-export([ flush_pending_messages/0, flush_pending_messages/1,
		  notify_pending_messages/0, check_no_pending_message/0,

		  wait_for/2, wait_for/4,

		  wait_for_acks_nothrow/3, wait_for_acks_nothrow/4,

		  wait_for_acks/4, wait_for_acks/5,
		  wait_for_summable_acks/5,
		  wait_for_many_acks/4, wait_for_many_acks/5,
		  send_to_pid_set/2 ]).


% Run-related functions.
%
% Not in code_utils, as we want them in a bootstrapped module.
-export([ run/1, run/2, run/3, exec/1, exec/2, exec/3,
		  get_myriad_root_path/0 ]).


% Version-related functions.
-export([ get_myriad_version/0, get_myriad_version_string/0,
		  % See also text_utils:version_to_string/1:
		  parse_version/1,
		  check_three_digit_version/1, check_any_version/1,
		  compare_versions/2 ]).


% Miscellaneous functions.
-export([ get_process_info/1, get_process_info/2,
		  display_process_info/1,
		  checkpoint/1,

		  assert/1, assert_true/1, assert_false/1,
		  assert_equal/2, assert_different/2,

		  display/1, display/2, display_timed/2, display_timed/3,
		  display_error/1, display_error/2,
          write_error_on_file/1, write_error_on_file/2,

		  throw_diagnosed/1, throw_diagnosed/2,
		  debug/1, debug/2,

		  get_unix_process_specific_string/0,
		  get_process_specific_value/0, get_process_specific_value/1,
		  get_process_specific_value/2,
		  get_process_size/1,
		  is_alive/1, is_alive/2, is_alive/3,
		  is_debug_mode_enabled/0, get_execution_target/0,
          setup_execution_target/0, setup_execution_target/1,
		  describe_term/1,
		  create_uniform_tuple/2,
		  stop/0, stop/1, stop_on_success/0, stop_on_failure/0,
		  stop_on_failure/1,

		  identity/1, if_else/3, repeat/2,

		  check_undefined/1, check_all_undefined/1, are_all_defined/1,
		  check_defined/1, check_not_undefined/1, check_all_defined/1,
		  set_option/2,

		  ignore_unused/1,
		  do_nothing/0, freeze/0, crash/0, crash/1, enter_infinite_loop/0,
		  trigger_oom/0 ]).


-compile( { inline, [ set_option/2 ] } ).


% To manage error output:
-export([ get_all_error_report_outputs/0, check_error_report_output/1,
          get_error_report_output/0, set_error_report_output/1 ]).



% Hints about retrieving the name of the function being currently evaluated by a
% process (as a ?FUNCTION macro could do):
%
% - either:
%
% current_function() ->
%    catch throw( x ), [_, {_, F, _, _} | _] = erlang:get_stacktrace(),
%    F.
%
% - or, maybe better:
%
% erlang:element(2, erlang:element(2,
%   erlang:process_info(self(), current_function)))).



% The default period (elementary time-slice; in milliseconds) according to which
% a duration (typically of a time-out) is divided.
%
-define( default_period_ms, 1000 ).


% To define get_execution_target/0:
-include("basic_utils.hrl").




% Implementation notes:
%
% The 'cond_utils' facilities shall not be used in this module, as it is a
% pioneer one (they are thus not bootstrapped yet).


% Local types:

-doc "Module name as an iolist".
-type io_list_mod() :: text_utils:io_list().


% Type shorthands:

-type set( T ) :: set_utils:set( T ).

-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type directory_path() :: file_utils:directory_path().
-type file_path() :: file_utils:file_path().
-type abs_file_path() :: file_utils:abs_file_path().

-type void() :: type_utils:void().
-type tuploid() :: type_utils:tuploid().
-type option( T ) :: type_utils:option( T ).

-type atom_node_name() :: net_utils:atom_node_name().

-type byte_size() :: system_utils:byte_size().

-type time_out() :: time_utils:time_out().
-type finite_time_out() :: time_utils:finite_time_out().
-type ms_period() :: time_utils:ms_period().

-type milliseconds() :: unit_utils:milliseconds().


% Even if not exported:
%-type process_info_result_item() :: erlang:process_info_result_item().
-type process_info_result_item() :: any().



-doc """
Creates a tuple of the specified size, all elements having the same, specified,
value.
""".
-spec create_uniform_tuple( Size :: count(), Value :: any() ) -> tuple().
create_uniform_tuple( Size, Value ) ->
	List = lists:duplicate( Size, Value ),
	list_to_tuple( List ).



-doc """
Stops smoothly the underlying VM, with a normal, success status code (`0`).

Also also to potentially override Erlang standard teardown procedure.
""".
-spec stop() -> no_return().
stop() ->
	stop( _Success=0 ).



-doc """
Stops smoothly, synchronously the underlying VM, with the specified error code.

Also allows to potentially override Erlang standard teardown procedure.
""".
-spec stop( status_code() ) -> no_return().
stop( StatusCode ) ->

	%trace_utils:debug( "Immediate stop." ),

	% Far less brutal than erlang:halt/{0,1}, yet awfully slow, and
	% actually non-blocking:
	%
	%init:stop( StatusCode ),

	% So, finally preferred (as blocking and fast):
	halt( StatusCode ),

	%trace_utils:debug( "Stopped." ),

	% To avoid that the calling process continues with the next instructions:
	% (would happen with init:stop/1, but not halt/{0,1})
	%
	freeze().



-doc """
Stops smoothly, synchronously the underlying VM, with a normal, success status
code (`0`).
""".
-spec stop_on_success() -> no_return().
stop_on_success() ->
	stop( _Success=0 ).



-doc """
Stops smoothly the underlying VM, with a default error status code (`1`).
""".
-spec stop_on_failure() -> no_return().
stop_on_failure() ->
	stop_on_failure( _OurDefaultErrorCode=5 ).


-doc "Stops smoothly the underlying VM, with the specified error status code.".
-spec stop_on_failure( status_code() ) -> no_return().
stop_on_failure( StatusCode ) ->
	stop( StatusCode ).



-doc """
Identity function: returns its argument as it is.

Useful to:

- avoid having the compiler being too smart by notifying annoying, spurious
messages (e.g. no clause will ever match) in some tests

- to prevent manually *Last-Call Optimisation* by calling this function on the
result (last expression) of a function, in order that this last function does
not disappear from stacktraces
""".
-spec identity( term() ) -> term().
identity( Term ) ->
	Term.



-doc """
Returns, if the first argument is `true`, the second argument, otherwise the
third.

Interesting as more compact that a `case` or a `if` clause.

`if_else(Condition, A, B)` can be seen just as a shortcut (see its actual code),
and a good candidate for parse-transfrom based inlining. Note that due to strict
evaluation, both arguments will always be evaluated.
""".
% First argument not necessarily boolean(), just 'true' | T:
-spec if_else( Condition :: term(),
			   IfTrue :: term(), IfNotTrue :: term() ) -> term().
if_else( _Condition=true, IfTrue, _IfNotTrue ) ->
	IfTrue;

if_else( _Condition, _IfTrue, IfNotTrue ) ->
	IfNotTrue.


% No if_defined/2 to be defined - just refer to set_option/2.


-doc """
Repeats the specified lamba function the specified number of times.

Useful for example to receive a given number of test messages.
""".
-spec repeat( fun( () -> void() ), count() ) -> void().
repeat( _Fun, _Count=0 ) ->
    %trace_utils:debug( "Repeating over." ),
    ok;

repeat( Fun, Count ) ->
    %trace_utils:debug_fmt( "Repeating #~B.", [ Count ] ),
    Fun(),
    repeat( Fun, Count-1 ).




-doc "Checks that the specified term is `undefined`, and returns it.".
-spec check_undefined( term() ) -> 'undefined'.
check_undefined( undefined ) ->
	undefined;

check_undefined( Term ) ->
	throw( { not_undefined, Term } ).



-doc """
Checks that all elements of the specified list are equal to `undefined`; returns
that list.
""".
-spec check_all_undefined( term() ) -> [ term() ].
check_all_undefined( List ) ->
	[ check_undefined( Term ) || Term <- List ].



-doc "Checks that the specified term is not `undefined`; returns that term.".
-spec check_not_undefined( term() ) -> term().
check_not_undefined( undefined ) ->
	throw( is_undefined );

check_not_undefined( Term ) ->
	Term.



-doc """
Checks that the specified term is "defined" (that is not equal to `undefined`);
returns that term.
""".
-spec check_defined( term() ) -> term().
check_defined( Term ) ->
	check_not_undefined( Term ).



-doc """
Checks that all elements of the specified list are "defined" (that is not
`undefined`); returns that list.
""".
-spec check_all_defined( [ term() ] ) -> [ term() ].
check_all_defined( List ) ->
	[ check_defined( Term ) || Term <- List ].



-doc """
Returns whether all the elements specified are defined (that is are not equal to
the `undefined` atom).
""".
-spec are_all_defined( [ term() ] ) -> boolean().
are_all_defined( _Elems=[] ) ->
	true;

are_all_defined( _Elems=[ undefined | _T ] ) ->
	false;

are_all_defined( _Elems=[ _E | T ] ) ->
	are_all_defined( T ).



-doc """
Returns, if the first argument is defined, in the sense of different from
`undefined`, the first argument, otherwise returns the second argument.

Allows to apply a default if a option-term is not defined, in a more compact
form than with a `case`.

For example, if `TestedValue =:= undefined`, then `foo =
basic_utils:set_option(TestedValue, _Otherwise=foo)`, whereas if `TestedValue
=:= 4` then `4 = basic_utils:set_option(TestedValue, foo)`.

Ideally the default term would be lazily evaluated (e.g. if calling an
initialisation function, notably for the second term).

Not named `if_defined/2` to avoid being mixed up with build-time functions in
`cond_utils`.
""".
-spec set_option( option( term() ), term() ) -> term().
set_option( _OptionTerm=undefined, TDef ) ->
	TDef;

set_option( T, _TDef ) ->
	T.



-doc """
Ignores its specified argument.

Useful to define, for debugging purposes, terms that will be (temporarily)
unused without blocking the compilation.

For example `basic_utils:ignore_unused(A)` or `basic_utils:ignore_unused([A, B,
C]).`.
""".
-spec ignore_unused( any() ) -> void().
ignore_unused( _Term ) ->
	% Preferred silent:
	ok.
	%trace_utils:warning_fmt( "unused term (~p) ignored "
	%   "(thanks to basic_utils:ignore_unused/1).", [ _Term ] ).



-doc """
Does nothing at all, at the expense of a remote call.

May be useful for experiments, for example in link with LCO (*Last Call
Optimisation*).
""".
do_nothing() ->
	ok.



-doc """
Freezes the current process immediately.

Useful to block the process while for example an ongoing, asynchronous
termination occurs.

See also: `enter_infinite_loop/0`.
""".
-spec freeze() -> no_return().
freeze() ->

	%trace_utils:debug( "Freezing..." ),

	receive

		not_expected_to_be_received ->
			trace_utils:error( "Process has been unexpectedly unfrozen." ),
			freeze()

	end.



-doc """
Crashes the current process immediately.

Useful for testing reliability, for example.
""".
-spec crash() -> void().
crash() ->

	trace_bridge:warning_fmt( "*** Crashing on purpose process ~w ***",
							  [ self() ] ),

	% Must outsmart the compiler; there should be simpler solutions:
	A = system_utils:get_core_count(),
	B = system_utils:get_core_count(),

	% Dividing thus by zero:
	1 / ( A - B ).



-doc """
Crashes the current process immediately, displaying the specified term.

Useful for testing reliability, for example.
""".
-spec crash( term() ) -> any().
crash( Term ) ->

	trace_bridge:warning_fmt( "*** Crashing on purpose process ~w: ~p ***",
							  [ self(), Term ] ),

	% Must outsmart the compiler; there should be simpler solutions:
	A = system_utils:get_core_count(),
	B = system_utils:get_core_count(),

	% Dividing thus by zero:
	1 / ( A - B ).



-doc """
Makes the current process enter in an infinite, mostly idle loop.

Useful for testing reliability, for example.

See also: `freeze/0`.
""".
enter_infinite_loop() ->

	io:format( "~p in infinite loop...", [ self() ] ),

	% Loops every minute:
	timer:sleep( 60000 ),

	enter_infinite_loop().



-doc """
Triggers a OOM crash, that is a "Out of Memory" error.

Useful for testing reliability, for example.
""".
trigger_oom() ->

	io:format( "~p triggering OOM (out of memory) crash...", [ self() ] ),

	% Expected: Crash dump was written to: erl_crash.dump
	%  binary_alloc: Cannot allocate 1000000000031 bytes of memory (of type
	% "binary").

	<<1:8000000000000>>.






% Notification-related functions.


-doc "Speaks the specified message (using currently espeak).".
-spec speak( ustring() ) -> void().
speak( Message ) ->
	system_utils:run_background_command(
		"espeak -s 140 \"" ++ Message ++ "\"" ).



-doc """
Notifies the user of the specified message, with log output and synthetic voice.
""".
-spec notify_user( ustring() ) -> void().
notify_user( Message ) ->
	io:format( Message ),
	speak( Message ).



-doc """
Notifies the user of the specified message, with log output and synthetic
voice.

Example: `basic_utils:notify_user("Hello ~w", [Name]).`.
""".
-spec notify_user( format_string(), format_values() ) -> void().
notify_user( Message, FormatList ) ->

	ActualMessage = io_lib:format( Message, FormatList ),

	io:format( ActualMessage ),
	speak( ActualMessage ).




% Message-related section.


-doc "Flushes all the messages still in the mailbox of this process.".
-spec flush_pending_messages() -> void().
flush_pending_messages() ->

	receive

		_ ->
			flush_pending_messages()

	after 0 ->
		ok

	end.



-doc """
Flushes all the messages still in the mailbox of this process that (exactly)
match the specified one.
""".
-spec flush_pending_messages( any() ) -> void().
flush_pending_messages( Message ) ->

	receive

		Message ->
			flush_pending_messages( Message )

	after 0 ->
		ok

	end.



-doc """
Reads all pending messages in the mailbox of this process and notifies about
them on the console.

Does not block.

Useful for tests.
""".
-spec notify_pending_messages() -> void().
notify_pending_messages() ->

	receive

		Message ->
			trace_utils:warning_fmt( "Following message was pending: ~p.",
									 [ Message ] ),
			notify_pending_messages()

	after 0 ->

		ok

	end.



-doc """
Ensures that no message is pending in the mailbox of this process.

Does not block.

Useful for tests.
""".
-spec check_no_pending_message() -> void().
check_no_pending_message() ->

	receive

		Message ->
			trace_utils:error_fmt( "Following message was pending in the "
				"mailbox of ~w:~n  ~p", [ self(), Message ] ),
			throw( { pending_message_in_mailbox, Message, self() } )

	after 0 ->
		ok

	end.



-doc """
Waits (indefinitively) for the specified count instances of the specified
message to be received.
""".
-spec wait_for( term(), count() ) -> void().
wait_for( _Message, _Count=0 ) ->
	ok;

wait_for( Message, Count ) ->

	% Not available here: cond_utils:if_defined( myriad_debug_waited_operations,

	%trace_utils:debug_fmt( "Waiting for ~B messages '~p'.",
	%                       [ Count, Message ] )

	receive

		Message ->
			wait_for( Message, Count-1 )

	end.



-doc """
Waits (indefinitively) for the specified count of the specified message to be
received, displaying repeatedly on the console a notification should the
duration between two receivings exceed the specified time-out.

Typical usage: `basic_utils:wait_for({foobar_result, done}, _Count=5,
_Duration=2000, "Still waiting for ~B task(s) to complete")`.
""".
-spec wait_for( term(), count(), milliseconds(), format_string() ) -> void().
wait_for( _Message, _Count=0, _TimeOutDuration, _TimeOutFormatString ) ->
	ok;

wait_for( Message, Count, TimeOutDuration, TimeOutFormatString ) ->

	%trace_utils:debug_fmt( "Waiting for ~B messages '~p'.",
	%                       [ Count, Message ] ),

	receive

		Message ->
			%trace_utils:debug_fmt( "Received message '~p'",
			%                       [ Message ] ),

			wait_for( Message, Count-1 )

	after TimeOutDuration ->

		io:format( TimeOutFormatString ++ " after ~ts",
				   [ Count, time_utils:duration_to_string( TimeOutDuration ) ] )

	end.




% Wait patterns, safer and better defined once for all.


-doc """
Waits until receiving from all the expected senders the specified
acknowledgement message, expected to be in the form of `{AckReceiveAtom,
WaitedSenderPid}`.

Returns a (possibly empty) list of the PIDs of the senders that failed to answer
within the specified time-out.

See `wait_for_many_acks/{4,5}` if having a large number of senders that are
waited for.
""".
-spec wait_for_acks_nothrow( [ pid() ], finite_time_out(), atom() ) ->
											[ pid() ].
wait_for_acks_nothrow( WaitedSenders, MaxMsDuration, AckReceiveAtom ) ->
	wait_for_acks_nothrow( WaitedSenders, MaxMsDuration, ?default_period_ms,
						   AckReceiveAtom ).



-doc """
Waits until receiving from all the expected senders the specified
acknowledgement message, expected to be in the form of `{AckReceiveAtom,
WaitedSenderPid}`, ensuring a check is performed at least at specified period.

Returns a (possibly empty) list of the PIDs of the senders that failed to answer
within the specified time-out.

See `wait_for_many_acks/{4,5}` if having a large number of senders that are
waited for.
""".
-spec wait_for_acks_nothrow( [ pid() ], finite_time_out(), ms_period(),
							 atom() ) -> [ pid() ].
wait_for_acks_nothrow( WaitedSenders, MaxMsDuration, Period,
					   AckReceiveAtom ) ->

	%trace_utils:debug_fmt( "Waiting (no-throw) for ~p (period: ~ts, "
	%   "max duration: ~ts, ack atom: '~ts').",
	%   [ WaitedSenders, time_utils:duration_to_string( Period ),
	%     time_utils:duration_to_string( MaxMsDuration ),
	%     AckReceiveAtom ] ),

	InitialTimestamp = time_utils:get_timestamp(),

	wait_for_acks_nothrow_helper( WaitedSenders, InitialTimestamp,
		MaxMsDuration, Period, AckReceiveAtom ).



% (helper)
wait_for_acks_nothrow_helper( _WaitedSenders=[], _InitialTimestamp,
		_MaxMsDuration, _Period, _AckReceiveAtom ) ->
	[];

wait_for_acks_nothrow_helper( WaitedSenders, InitialTimestamp,
							  MaxMsDuration, Period, AckReceiveAtom ) ->

	receive

		{ AckReceiveAtom, WaitedPid } ->

			NewWaited = list_utils:delete_existing( WaitedPid, WaitedSenders ),

			%trace_utils:debug_fmt( "(received ~p, still waiting for "
			%   "instances ~p)", [ WaitedPid, NewWaited ] ),

			wait_for_acks_nothrow_helper( NewWaited, InitialTimestamp,
				MaxMsDuration, Period, AckReceiveAtom )

	after Period ->

			NewMsDuration =
				1000 *time_utils:get_duration_since( InitialTimestamp ),

			case ( MaxMsDuration =/= infinity ) andalso
						( NewMsDuration > MaxMsDuration ) of

				true ->
					WaitedSenders;

				false ->
					% Still waiting then:

					%trace_utils:debug_fmt(
					%   "(still waiting for instances ~p)",
					%   [ WaitedSenders ] ),

					wait_for_acks_nothrow_helper( WaitedSenders,
						InitialTimestamp, MaxMsDuration, Period,
						AckReceiveAtom )

			end

	end.



-doc """
Waits until receiving from all expected senders the specified acknowledgement
message, expected to be in the form of `{AckReceiveAtom, WaitedSenderPid}`.

Throws a `{ThrowAtom, StillWaitedSenders}` exception on time-out (if any, as the
time-out can be disabled if set to `infinity`).

See `wait_for_many_acks/{4,5}` if having a large number of senders that are
waited for.
""".
-spec wait_for_acks( [ pid() ], time_out(), atom(), atom() ) -> void().
wait_for_acks( WaitedSenders, MaxMsDuration, AckReceiveAtom, ThrowAtom ) ->
	wait_for_acks( WaitedSenders, MaxMsDuration, ?default_period_ms,
				   AckReceiveAtom, ThrowAtom ).



-doc """
Waits until receiving from all expected senders the specified acknowledgement
message, expected to be in the form of `{AckReceiveAtom, WaitedSenderPid}`,
ensuring that a check is performed at least at the specified period.

Throws a `{ThrowAtom, StillWaitedSenders}` exception on time-out.

See `wait_for_many_acks/{4,5}` if having a large number of senders waited for.
""".
-spec wait_for_acks( [ pid() ], time_out(), ms_period(), atom(), atom() ) ->
															void().
wait_for_acks( WaitedSenders, MaxMsDuration, Period, AckReceiveAtom,
			   ThrowAtom ) ->

	%trace_utils:debug_fmt( "Waiting for ~p (period: ~ts, "
	%   "max duration: ~ts, ack atom: '~ts', throw atom: '~ts').",
	%   [ WaitedSenders, time_utils:duration_to_string( Period ),
	%     time_utils:duration_to_string( MaxMsDuration ), AckReceiveAtom,
	%     ThrowAtom ] ),

	InitialTimestamp = time_utils:get_timestamp(),

	case wait_for_acks_nothrow_helper( WaitedSenders, InitialTimestamp,
			MaxMsDuration, Period, AckReceiveAtom ) of

		[] ->
			ok;

		StillWaitedSenders ->
			throw( { ThrowAtom, StillWaitedSenders } )

	end.



-doc """
Waits until receiving from all the expected senders the specified
acknowledgement message, expected to be in the form of: `{AckReceiveAtom, ToAdd,
WaitedSenderPid}`.

Returns the sum of the specified initial value with all the ToAdd received
values.

Throws a `{ThrowAtom, StillWaitedSenders}` exception on time-out (if any, as the
time-out can be disabled if set to `infinity`).
""".
-spec wait_for_summable_acks( [ pid() ], number(), time_out(), atom(),
							  atom() ) -> number().
wait_for_summable_acks( WaitedSenders, InitialValue, MaxDurationInSeconds,
						AckReceiveAtom, ThrowAtom ) ->

	wait_for_summable_acks( WaitedSenders, InitialValue, MaxDurationInSeconds,
							?default_period_ms, AckReceiveAtom, ThrowAtom ).



-doc """
Waits until receiving from all the expected senders the specified
acknowledgement message, expected to be in the form of: `{AckReceiveAtom, ToAdd,
WaitedSenderPid}`, ensuring a check is performed at least at specified period
and summing all ToAdd values with the specified initial one.

Throws a `{ThrowAtom, StillWaitedSenders}` exception on time-out.
""".
-spec wait_for_summable_acks( [ pid() ], number(), time_out(),
							  milliseconds(), atom(), atom() ) -> number().
wait_for_summable_acks( WaitedSenders, CurrentValue, MaxDurationInSeconds,
						Period, AckReceiveAtom, ThrowAtom ) ->

	InitialTimestamp = time_utils:get_timestamp(),

	wait_for_summable_acks_helper( WaitedSenders, CurrentValue,
		InitialTimestamp, MaxDurationInSeconds, Period, AckReceiveAtom,
		ThrowAtom ).


% (helper)
wait_for_summable_acks_helper( _WaitedSenders=[], CurrentValue,
		_InitialTimestamp, _MaxDurationInSeconds,  _Period, _AckReceiveAtom,
		_ThrowAtom ) ->
	CurrentValue;

wait_for_summable_acks_helper( WaitedSenders, CurrentValue, InitialTimestamp,
			MaxDurationInSeconds, Period, AckReceiveAtom, ThrowAtom ) ->

	receive

		{ AckReceiveAtom, ToAdd, WaitedPid } ->

			NewWaited = list_utils:delete_existing( WaitedPid, WaitedSenders ),

			%trace_utils:debug_fmt( "(received ~p, still waiting for "
			%   "instances ~p).", [ WaitedPid, NewWaited ] ),

			wait_for_summable_acks_helper( NewWaited, CurrentValue + ToAdd,
				InitialTimestamp, MaxDurationInSeconds, Period,
				AckReceiveAtom, ThrowAtom )

	after Period ->

			NewDuration = time_utils:get_duration_since( InitialTimestamp ),

			case ( MaxDurationInSeconds =/= infinity ) andalso
						( NewDuration > MaxDurationInSeconds ) of

				true ->
					throw( { ThrowAtom, WaitedSenders } );

				false ->
					% Still waiting then:

					%trace_utils:debug_fmt(
					%   "(still waiting for instances ~p)",
					%   [ WaitedSenders ] ),

					wait_for_summable_acks_helper( WaitedSenders, CurrentValue,
						InitialTimestamp, MaxDurationInSeconds, Period,
						AckReceiveAtom, ThrowAtom )

			end

	end.



-doc """
Waits until receiving from all expected (numerous) senders the specified
acknowledgement message.

Throws specified exception on time-out.

Note: each sender shall be unique (as they will be gathered in a set, that does
not keep duplicates).
""".
-spec wait_for_many_acks( set( pid() ), finite_time_out(), atom(), atom() ) ->
											void().
wait_for_many_acks( WaitedSenders, MaxMsDuration, AckReceiveAtom, ThrowAtom ) ->
	wait_for_many_acks( WaitedSenders, MaxMsDuration, ?default_period_ms,
						AckReceiveAtom, ThrowAtom ).



-doc """
Waits until receiving from all expected (numerous) senders the specified
acknowledgement message.

Throws specified exception on time-out, checking at the specified period.
""".
-spec wait_for_many_acks( set( pid() ), finite_time_out(), ms_period(),
						  atom(), atom() ) -> void().
wait_for_many_acks( WaitedSenders, MaxMsDuration, Period, AckReceiveAtom,
					ThrowAtom ) ->

	InitialTimestamp = time_utils:get_timestamp(),

	wait_for_many_acks_helper( WaitedSenders, InitialTimestamp,
		MaxMsDuration, Period, AckReceiveAtom, ThrowAtom ).


% For this version we prefer a look-up optimised list to a plain one.
%
% (helper)
%
wait_for_many_acks_helper( WaitedSenders, InitialTimestamp, MaxMsDuration,
						   Period, AckReceiveAtom, ThrowAtom ) ->

	set_utils:is_empty( WaitedSenders ) orelse
		receive

			{ AckReceiveAtom, WaitedPid } ->

				NewWaited = set_utils:delete_existing( WaitedPid,
													   WaitedSenders ),

				wait_for_many_acks_helper( NewWaited, InitialTimestamp,
					MaxMsDuration, Period, AckReceiveAtom, ThrowAtom )

		after Period ->

				NewMsDuration =
					1000 * time_utils:get_duration_since( InitialTimestamp ),

				case NewMsDuration > MaxMsDuration of

					true ->
						throw( { ThrowAtom, WaitedSenders } );

					false ->
						% Still waiting then:
						wait_for_many_acks_helper( WaitedSenders,
							InitialTimestamp, MaxMsDuration, Period,
							AckReceiveAtom, ThrowAtom )

				end

		end.



-doc """
Sends the specified message to all elements (supposed to be PID) of the
specified set, and returns the number of sent messages.
""".
-spec send_to_pid_set( term(), set( pid() ) ) -> count().
send_to_pid_set( Message, PidSet ) ->

	% Conceptually (not a basic list, though):
	% [ Pid ! Message || Pid <- PidSet ]

	% With iterators, it is done slightly slower yet with less RAM rather than
	% first using set_utils:to_list/1 then iterating on the resulting plain
	% list:
	%
	Iterator = set_utils:iterator( PidSet ),

	% Returns the count:
	send_to_pid_set( Message, set_utils:next( Iterator ), _Count=0 ).



% (helper)
send_to_pid_set( _Message, none, Count ) ->
	Count;

send_to_pid_set( Message, { Pid, NewIterator }, Count ) ->
	Pid ! Message,
	send_to_pid_set( Message, set_utils:next( NewIterator ), Count+1 ).



-doc """
Runs the `run/0` function from the specified module.

Designed as a convenient launcher used with `erl -run`, dealing notably best
with outputs, error management and stacktraces.
""".
-spec run( io_list_mod() ) -> void().
run( ModIOList ) ->
	run( ModIOList, _FunctionName=run ).



-doc """
Runs the specified 0-arity function from the specified module.

Designed as a convenient launcher used with `erl -run`, dealing notably best
with outputs, error management and stacktraces.
""".
-spec run( io_list_mod(), function_name() ) -> void().
run( ModIOList, FunctionName ) ->
	run( ModIOList, FunctionName, _Args=[] ).



-doc """
Runs the specified function from the specified module, with the specified
arguments.

Designed as a convenient launcher used with `erl -run`, dealing notably best
with outputs, error management and stacktraces.
""".
-spec run( io_list_mod(), function_name(), [ argument() ] ) -> void().
run( ModIOList, FunctionName, Args ) ->

	% Not sufficient, as input is ["my_module"], not "my_module":
	%ModName = text_utils:string_to_atom( ModName ),

	ModName = list_to_atom( lists:flatten( ModIOList ) ),

	%trace_utils:debug_fmt( "Running ~ts:~ts with arguments ~p.",
	%                       [ ModName, FunctionName, Args ] ),

	try

		apply( ModName, FunctionName, Args )

	catch Class:Exception:Stacktrace ->

		%trace_utils:debug_fmt( "For exception ~p of class ~p, "
		%   "obtained stacktrace:~n ~p.", [ Exception, Class, Stacktrace ] ),

		{ ExplainStr, ShowStacktrace } = case Exception of

			undef ->
				{ Mod, Fun, Third } = case hd( Stacktrace ) of

					Triplet={ _M, _F, _A } ->
						Triplet;

					{ M, F, A, _FileLoc } ->
						{ M, F, A }

				end,

				Arity = case Third of

					Ar when is_integer( Ar ) ->
						Ar;

					% 'Args' already used!
					SomeArgs when is_list( SomeArgs ) ->
						length( SomeArgs )

				end,
				{ get_hint( code_utils:interpret_undef_exception( Mod, Fun,
																  Arity ) ),
				  true };


			{ application_not_found, _AppName, AppFilename, _AbsBaseDir } ->
				{ get_hint( "To generate 'ebin/~ts', "
					"one may run 'make create-app-file' "
					"from the root of the sources.", [ AppFilename ] ),
				  false };


			{ app_not_compiled, AppName, _AppBeam } ->
				{ get_hint( "The application '~ts' is not available.",
							[ AppName ] ), false };

			% Not interpreted (yet?):
			_ ->
				{ _NoHint="", true }

		end,

		ShowStacktrace andalso
			begin
				case lists:reverse( Stacktrace ) of

					[ {init,do_boot,3,[]}, {init,start_em,1,[]},
					  {basic_utils,run,3, _RunFileLoc} | RevRest ] ->
						manage_minimised_stacktrace( RevRest, Class, Exception,
													 ExplainStr );

					[ {init,start_em,1,[]}, {basic_utils,run,3, _RunFileLoc}
										| RevRest ] ->
						manage_minimised_stacktrace( RevRest, Class, Exception,
													 ExplainStr );

					% Apparently, sometimes (e.g. for gui_splash_test with LCO
					% disabled) the stacktrace does not even reference
					% basic_utils:run/3!
					%
					Rev ->
						manage_minimised_stacktrace( Rev, Class, Exception,
													 ExplainStr )

				end

			end,

		% As this is an error case:
		init:stop( _Status=15 )

	end.



% (helper)
get_hint( HintStr ) ->
	text_utils:format( "~nHint: ~ts~n", [ HintStr ] ).

get_hint( HintFormatStr, HintFormatValue ) ->
	get_hint( text_utils:format( HintFormatStr, HintFormatValue ) ).


% (helper)
manage_minimised_stacktrace( RevRest, Class, Exception, ExplainStr ) ->
	ShrunkStacktrace = lists:reverse( RevRest ),

	StackStr = code_utils:interpret_stacktrace( ShrunkStacktrace ),

	io:format( "~n#### Error: the program crashed with the following ~ts-class "
		"exception:~n  ~p~n~ts~nLatest calls first: ~ts~n",
		[ Class, Exception, ExplainStr, StackStr ] ).




% The exec/N variations for applications behave mostly like the run/N variations
% for tests (with X_app instead of X_test).


-doc """
Executes the `exec/0` function from the specified application module.

Designed as a convenient launcher used with `erl -run`, dealing notably best
with outputs, error management and stacktraces.
""".
-spec exec( io_list_mod() ) -> void().
exec( ModIOList ) ->
	run( ModIOList, _FunctionName=exec ).



-doc """
Executes the specified 0-arity function from the specified application module.

Designed as a convenient launcher used with `erl -run`, dealing notably best
with outputs, error management and stacktraces.
""".
-spec exec( io_list_mod(), function_name() ) -> void().
exec( ModIOList, FunctionName ) ->
	exec( ModIOList, FunctionName, _Args=[] ).



-doc """
Executes the specified function from the specified application module, with the
specified arguments.

Designed as a convenient launcher used with `erl -run`, dealing notably best
with outputs, error management and stacktraces.
""".
-spec exec( io_list_mod(), function_name(), [ argument() ] ) -> void().
exec( ModIOList, FunctionName, Args ) ->
	run( ModIOList, FunctionName, Args ).



-doc """
Returns a path to the root directory of the Ceylan-Myriad installation that is
in use.
""".
-spec get_myriad_root_path() -> directory_path().
get_myriad_root_path() ->
	CodePath = code_utils:get_code_path(),
	script_utils:get_myriad_path_from( CodePath ).



% Version-related functions.


-doc "Returns the version of the Myriad library being used.".
-spec get_myriad_version() -> three_digit_version().
get_myriad_version() ->
	parse_version( get_myriad_version_string() ).



-doc "Returns the version of the Myriad library being used, as a string.".
-spec get_myriad_version_string() -> ustring().
get_myriad_version_string() ->
	% As defined (uniquely) in GNUmakevars.inc:
	?myriad_version.



-doc """
Parses the specified textual version.

For example `"4.2.1"` should become `{4,2,1}`, and `"2.3"` should become
`{2,3}`.
""".
-spec parse_version( ustring() ) -> any_version().
parse_version( VersionString ) ->

	% First transform "4.22.1" into ["4","22","1"]:
	Elems = string:tokens( VersionString, "." ),

	% Then simply switch to {4,22,1}:
	list_to_tuple( [ text_utils:string_to_integer( E ) || E <- Elems ] ).



-doc "Checks that the specified term is a three-digit version, and returns it.".
-spec check_three_digit_version( term() ) -> three_digit_version().
check_three_digit_version( T={ A, B, C } ) when is_integer( A )
				andalso is_integer( B ) andalso is_integer( C ) ->
	T;

check_three_digit_version( T ) ->
	throw( { invalid_three_digit_version, T } ).



-doc "Checks that the specified term is a any-version, and returns it.".
-spec check_any_version( term() ) -> any_version().
check_any_version( T ) when is_tuple( T ) ->
	case lists:all( fun( E ) -> is_integer( E ) andalso E >= 0 end,
					tuple_to_list( T ) ) of

		true ->
			T;

		false ->
			throw( { invalid_any_version, T } )

	end;

check_any_version( V ) ->
	throw( { invalid_any_version, V } ).



-doc """
Compares the two specified any-versions (expected to be of the same size), which
describe two version numbers (e.g. `{0,1,0}` and `{0,1,7}`) and returns either
`first_bigger`, `second_bigger`, or `equal`.

The two compared versions must have the same number of digits.
""".
-spec compare_versions( any_version(), any_version() ) ->
								'equal' | 'first_bigger' | 'second_bigger'.
compare_versions( A, B ) when tuple_size( A ) =:= tuple_size( B ) ->
	% As the default term order is already what we need:
	case A > B of

		true ->
			first_bigger;

		false ->
			case A =:= B of

				true ->
					equal;

				false ->
					second_bigger

			end

	end.




% Miscellaneous functions.


-doc """
Returns all general information regarding the specified process (which is local
or not), provided that it is still alive (otherwise returns `undefined`).
""".
-spec get_process_info( pid() ) -> option( [ process_info_result_item() ] ).
get_process_info( Pid ) ->

	LocalNode = node(),

	% erlang:process_info/1 throws badarg if the process is not local:
	case node( Pid ) of

		LocalNode ->
			erlang:process_info( Pid );

		OtherNode ->

			% The current module may not be on this node:
			case rpc:call( OtherNode, _M=erlang, _F=process_info, _A=[ Pid ] )
                    of

				{ badrpc, Reason } ->
					trace_utils:error_fmt( "No information found for "
						"process ~w running on remote node ~p; reason: ~p.~n",
						[ Pid, OtherNode, Reason ] ),
					throw( { process_info_failed, Pid, Reason } );

				% Either 'undefined' or proplist:
				Res ->
					Res

			end


	end.



-doc """
Returns the specified information regarding the specified process (which is
local or not), provided that it is still alive (otherwise returns `undefined`).
""".
-spec get_process_info( pid(), process_info_result_item() ) ->
								option( process_info_result_item() );
					  ( pid(), [ process_info_result_item() ] ) ->
								option( [ process_info_result_item() ] ).
get_process_info( Pid, ItemTerm ) ->

	LocalNode = node(),

	% erlang:process_info/1 throws badarg if the process is not local:
	case node( Pid ) of

		LocalNode ->
			erlang:process_info( Pid, ItemTerm );

		OtherNode ->

			% The current module may not be on this node:
			case rpc:call( OtherNode, _M=erlang, _F=process_info,
						   _A=[ Pid, ItemTerm ] ) of

				{ badrpc, Reason } ->
					trace_utils:error_fmt( "No information found for "
						"process ~w running on remote node ~p; reason: ~p.~n",
						[ Pid, OtherNode, Reason ] ),
					throw( { process_info_failed, Pid, Reason } );

				% Either 'undefined' or proplist:
				Res ->
					Res

			end

	end.



-doc """
Displays information about the process(es) identified by the specified PID(s).
""".
-spec display_process_info( pid() | [ pid() ] ) -> void().
display_process_info( PidList ) when is_list( PidList ) ->
	[ display_process_info( Pid ) || Pid <- PidList ];

display_process_info( Pid ) when is_pid( Pid ) ->

	LocalNode = node(),

	% erlang:process_info/1 throws badarg if the process is not local:
	case node( Pid ) of


		LocalNode ->
			case erlang:process_info( Pid ) of

				undefined ->
					io:format( "PID ~w refers to a (local) dead process~n",
							   [ Pid ] );

				PropList ->
					Strings = [ io_lib:format( "~ts: ~p", [ K, V ] )
                                    || { K, V } <- PropList ],
					io:format( "PID ~w refers to a local live process, "
						"whose information is: ~ts",
						[ Pid, text_utils:strings_to_string( Strings ) ] )

			end;


		OtherNode ->

			% The current module may not be on this node:
			case rpc:call( OtherNode, _M=erlang, _F=process_info, _A=[ Pid ] )
			   of

				{ badrpc, Reason } ->
					io:format( "No information found for process ~w "
						"running on remote node ~p; reason: ~p.~n",
						[ Pid, OtherNode, Reason ] );

				undefined ->
					io:format( "PID ~w refers to a dead process on "
							   "remote node ~ts.~n", [ Pid, OtherNode ] );

				PropList ->

					Strings = [ io_lib:format( "~ts: ~p", [ K, V ] )
											|| { K, V } <- PropList ],

					io:format( "PID ~w refers to a live process on "
						"remote node ~ts, whose information are: ~ts",
						[ Pid, OtherNode,
						  text_utils:strings_to_string( Strings ) ] )

			end

	end.



-doc """
Displays a numbered checkpoint.

Useful for debugging purposes.
""".
-spec checkpoint( integer() ) -> void().
checkpoint( Number ) ->
	display( "----- CHECKPOINT #~B -----", [ Number ] ).




% Assert subsection.
%
% For counterparts that can be conditionally defined at compilation time, see
% cond_utils:assert/*.


-doc """
Asserts that the specified (runtime) expression is `true`, otherwise throws an
exception.
""".
-spec assert( term() ) -> void().
assert( _Expr=true ) ->
	ok;

assert( Other ) ->
	interpret_failed_assertion( "'~p' is not true", [ Other ] ),
	throw( { assert_failed, Other } ).



% Code of assert/1 duplicated rather than being called, as a nested call would
% change the depth of the stacktrace:


-doc """
Asserts that the specified (runtime) expression is true, otherwise throws an
exception.

Defined for consistency with `assert_false/1`.
""".
-spec assert_true( term() ) -> void().
assert_true( _Expr=true ) ->
	ok;

assert_true( Other ) ->
	interpret_failed_assertion( "'~p' is not true", [ Other ] ),
	throw( { assert_failed, Other } ).



-doc """
Asserts that the specified (runtime) expression is `false`, otherwise throws an
exception.
""".
-spec assert_false( term() ) -> void().
assert_false( _Expr=false ) ->
	ok;

assert_false( Other ) ->
	interpret_failed_assertion( "'~p' is not false", [ Other ] ),
	throw( { assert_false_failed, Other } ).



-doc """
Asserts that the specified (runtime) expressions compare equal, otherwise throws
an exception.

Sometimes searched as `check_equal/2`.
""".
-spec assert_equal( term(), term() ) -> void().
assert_equal( Expr, Expr ) ->
	ok;

assert_equal( Expr1, Expr2 ) ->
	interpret_failed_assertion( "'~p' is not equal to '~p'", [ Expr1, Expr2 ] ),
	throw( { assert_equal_failed, Expr1, Expr2 } ).



-doc """
Asserts that the specified (runtime) expressions compare different, otherwise
throws an exception.
""".
-spec assert_different( term(), term() ) -> void().
assert_different( Expr, Expr ) ->
	interpret_failed_assertion( "both elements are equal to '~p'", [ Expr ] ),
	throw( { assert_different_failed, Expr } );

assert_different( _Expr1, _Expr2 ) ->
	ok.


% (helper)
interpret_failed_assertion( FormatStr, FormatValues ) ->
	Msg = text_utils:format( FormatStr, FormatValues ),
	interpret_failed_assertion( Msg ).


% (helper)
interpret_failed_assertion( Msg ) ->

	{ Mod, Func, Arity, [ { file, SrcFile }, { line, Line } ] } =
		hd( code_utils:get_stacktrace( _SkipLastElemCount=2 ) ),

	trace_utils:error_fmt( "Assertion failed in ~ts:~ts/~B "
		"(file ~ts, line ~B): ~ts.", [ Mod, Func, Arity, SrcFile, Line, Msg ] ).



-doc """
Displays the specified string on the standard output of the console, ensuring as
much as possible that this message is output synchronously, so that it can be
fully processed (typically displayed) by the console even if the virtual machine
is to crash just after.
""".
-spec display( ustring() ) -> void().
display( Message ) ->

	% Finally io:format has been preferred to erlang:display, as the latter one
	% displays quotes around the strings.
	%
	% ~ts, not ~ts, as we want to properly output Unicode characters:
	%
	io:format( "~ts~n", [ Message ] ),

	% Possibly to allow for a yield (100 being far too high):
	timer:sleep( 10 ),

	system_utils:await_output_completion().


	% May not go through group leader (like io:format), thus less likely to
	% crash without displaying the message:
	%
	%erlang:display( lists:flatten( [ Message, ".~n" ] ) ).
	%erlang:display( Message ).



-doc """
Displays the specified format string filled according to the specified values on
the standard output of the console, ensuring as much as possible that this
message is output synchronously, so that it can be output on the console even if
the virtual machine is to crash just after.
""".
-spec display( format_string(), format_values() ) -> void().
display( Format, Values ) ->

	%io:format( "Displaying format '~p' and values '~p'.~n",
	%           [ Format, Values ] ),

	Message = text_utils:format( Format, Values ),

	display( Message ).



-doc """
Displays the specified string on the standard output of the console, ensuring as
much as possible that this message is output synchronously, so that it can be
output on the console even if the virtual machine is to crash just after.
""".
-spec display_timed( ustring(), time_out() ) -> void().
display_timed( Message, TimeOut ) ->

	% Finally io:format has been preferred to erlang:display, as the latter one
	% displays quotes around the strings.

	io:format( "~ts~n", [ Message ] ),
	system_utils:await_output_completion( TimeOut ).

	% May not go through group leader (like io:format), thus less likely to
	% crash without displaying the message:
	%
	%erlang:display( lists:flatten( [ Message, ".~n" ] ) ).
	%erlang:display( Message ).



-doc """
Displays the specified format string filled according to specified values on the
standard output of the console, ensuring as much as possible this message is
output synchronously, so that it can be output on the console even if the
virtual machine is to crash just after.
""".
-spec display_timed( format_string(), format_values(), time_out() ) -> void().
display_timed( Format, Values, TimeOut ) ->

	%trace_utils:debug_fmt( "Displaying format '~p' and values '~p'.",
	%                       [ Format, Values ] ),

	Message = text_utils:format( Format, Values ),

	display_timed( Message, TimeOut ).



-doc """
Displays the specified string on the standard error output of the console,
ensuring as much as possible this message is output synchronously, so that it
can be output on the console even if the virtual machine is to crash just after.
""".
-spec display_error( ustring() ) -> void().
display_error( Message ) ->

	% At least once, following call resulted in no output at all (standard_error
	% not functional):
	%
	% Reintroduced for testing after 21.0:
	%
	io:format( standard_error, "~ts~n", [ Message ] ),

	% So:
	%io:format( "~ts~n", [ Message ] ),

	system_utils:await_output_completion().



-doc """
Triggers the specified diagnosed error: reports first its embedded diagnosis,
then throws this error as an exception.

Typical use:
```
case Expr of

 { ok, X } ->
   [...];

 { ok, Y } ->
   [...];

 { error, DiagnosedReason } ->
	basic_utils:throw_diagnosed( DiagnosedReason )

end
```
""".
-spec throw_diagnosed( diagnosed_error_reason() ) -> no_return().
throw_diagnosed( _DiagnosedReason={ ErrorTuploid, ErrorMsg } ) ->
	trace_bridge:error( ErrorMsg ),
	throw( ErrorTuploid ).



-doc """
Triggers the specified diagnosed error, augmented by the specified term: reports
first its embedded diagnosis, then throws this error, as an augmented tuploid,
as an exception.

Typical use:
```
case Expr of

 { ok, X } ->
   [...];

 { ok, Y } ->
   [...];

 { error, DiagnosedReason } ->
	basic_utils:throw_diagnosed( DiagnosedReason, Z )

end
```
""".
-spec throw_diagnosed( diagnosed_error_reason(), term() ) -> no_return().
throw_diagnosed( _DiagnosedReason={ ErrorTuploid, ErrorMsg },
				 ExtraErrorTerm ) ->
	trace_bridge:error( ErrorMsg ),
	throw( type_utils:augment_tuploid( ErrorTuploid, ExtraErrorTerm ) ).



-doc """
Displays the specified format string filled according to specified values on the
standard error output of the console, ensuring as much as possible this message
is output synchronously, so that it can be output on the console even if the
virtual machine is to crash just after.
""".
-spec display_error( format_string(), format_values() ) -> void().
display_error( Format, Values ) ->
	Message = text_utils:format( Format ++ "~n", Values ),
	display_error( Message ).


-doc """
Reports the specified error message in a dedicated file, named
`myriad-error-report.txt`, created in the current directory, whose absolute path
is returned.

Any prior file with the same name will be overwritten.

Useful to record error reports that would be inconveniently long on the console,
but whose extra information could be nevertheless useful for a post-mortem
analysis.
""".
-spec write_error_on_file( ustring() ) -> abs_file_path().
write_error_on_file( ErrorMsg ) ->
    write_error_on_file( ErrorMsg, _FilePath="myriad-error-report.txt" ).


-doc """
Reports the specified error message in a dedicated file, whose name or
(relative/absolute) path is specified, and whose absolute path is returned.

Any prior file with the same name will be overwritten.

Useful to record error reports that would be inconveniently long on the console,
but whose extra information could be nevertheless useful for a post-mortem
analysis.
""".
-spec write_error_on_file( ustring(), file_path() ) -> abs_file_path().
write_error_on_file( ErrorMsg, FilePath ) ->

    AbsFilePath = file_utils:ensure_path_is_absolute( FilePath ),

    Str = text_utils:format( "=== Myriad error report issued for OS process "
        "~ts on ~ts ===~n~n~ts~n~n=== End of Myriad error report ===",
        [ os:getpid(), time_utils:get_textual_timestamp(), ErrorMsg ] ),

    file_utils:write_whole( AbsFilePath, Str ),

    trace_utils:notice_fmt( "A Myriad error report has been written in '~ts'.",
                            [ AbsFilePath ] ),

    AbsFilePath.



-doc """
Displays, for debugging purposes, the specified string, ensuring as much as
possible this message is output synchronously, so that it can be output on the
console even if the virtual machine is to crash just after.
""".
-spec debug( ustring() ) -> void().
debug( Message ) ->
	trace_utils:debug( Message ).
	%system_utils:await_output_completion().
	%erlang:display( "## Debug: " ++ Message ).



-doc """
Displays, for debugging purposes, the specified format string filled according
tothe specified values, ensuring as much as possible this message is output
synchronously, so that it can be output on the console even if the virtual
machine is to crash just after.
""".
-spec debug( format_string(), format_values() ) -> void().
debug( Format, Values ) ->
	debug( text_utils:format( Format, Values ) ).



-doc """
Returns a string that should be unique for the current UNIX process (of the VM),
based on its operating system PID and on millisecond-precise time.

Reasonably unique (add the PID of the Erlang process if concurrent creations may
happen), and suitable for file creation.

For example `"myriad-596330--576460741437"` (negative monotonic time).
""".
-spec get_unix_process_specific_string() -> ustring().
get_unix_process_specific_string() ->
	text_utils:format( "myriad-~ts-~B",
					   [ os:getpid(), time_utils:get_monotonic_time() ] ).



-doc """
Returns a stable, reproducible value (a strictly positive integer) expected to
be as much as possible specific to the current (Erlang) process.

Refer to `get_process_specific_value/1` for further details, and to
`get_process_specific_value/1` to generate a value in a given range.
""".
-spec get_process_specific_value() -> pos_integer().
get_process_specific_value() ->
	get_process_specific_value( _Pid=self() ).



-doc """
Returns a stable, reproducible value (a strictly positive integer) expected to
be as much as possible specific to the specified (Erlang) PID.

This value is meant to be process-specific *and* reproducible, that is returning
always the same value for the same PID. This may be useful when having to
generate an integer identifier corresponding to a given process.

We could imagine taking into account as well the current time, the process
reductions, etc. or generating a reference.
""".
-spec get_process_specific_value( pid() ) -> pos_integer().
get_process_specific_value( Pid ) ->

	% PID are akin to <X.Y.Z>.

	PidAsText = lists:flatten( io_lib:format( "~w", [ Pid ] ) ),

	%io:format( "PID: ~w.~n", [ self() ] ) ,
	% For example ["<0","33","0>"]:
	[ [ $< | First ], Second, Third ] = string:tokens( PidAsText, "." ),

	% We add 1 to x and z as they might be null:
	{ F, [] } = string:to_integer( First ),
	{ S, [] } = string:to_integer( Second ),

	[ $> | ExtractedThird ] = lists:reverse( Third ),

	{ T, [] } = string:to_integer( ExtractedThird ),

	X = F+1,

	% Key part of PID, never null:
	Y = S,

	Z = T+1,

	% Hash part probably a bit overkill and, a lot more importantly, including a
	% reference would break reproducibility - much undesirable here:
	%
	%Res = X*Y*Z + erlang:phash2( erlang:make_ref(), _MaxRange=1 bsl 32 ),

	Res = X*Y*Z,

	%trace_utils:debug_fmt( "Process-specific value: ~B.", [ Res ] ),
	Res.



-doc """
Returns an (Erlang), non-reproducible, process-specific value in `[Min,Max[`.

Useful for example when a large number of similar processes try to access to the
same resource (e.g. a set of file descriptors) at the same time: they can rely
on some random waiting based on that process-specific value in order to smooth
the accesses over time. Reproducibility does not matter here.
""".
-spec get_process_specific_value( integer(), integer() ) -> integer().
get_process_specific_value( Min, Max ) ->
	Value = get_process_specific_value(),
	{ H, M, S } = erlang:time(),
	( ( ( H + M + S + 1 ) * Value ) rem ( Max - Min ) ) + Min.



-doc """
Returns the total size in (RAM) memory used by specified (local, alive) process,
in bytes: this includes call stack, heap, and internal structures.

See [https://erlang.org/doc/man/erlang.html#process_info-1] for more
information.
""".
-spec get_process_size( pid() ) -> byte_size().
get_process_size( Pid ) ->

	% 4 bytes is returned on a 32-bit architecture, and 8 is returned on a pure
	% 64-bit architecture:
	%
	%WordSize = erlang:system_info( { wordsize, internal } ),

	%ProcessPropList = erlang:process_info( Pid ),

	%trace_utils:debug_fmt( "Process info for ~w:~n~p",
	%                       [ Pid, ProcessPropList ] ),

	% Includes call stack, heap, and internal structures:
	% (apparentlyalready in bytes, not words:
	%
	%WordSize * list_table:get_value( memory, ProcessPropList ).
	%list_table:get_value( memory, ProcessPropList ).
	{ memory, Size } = get_process_info( Pid, memory ),
	Size.



-doc """
Tells whether the specified process, designated by its PID, by a textual
representation of it (like `<9092.61.0>`) or by a registered name (local
otherwise global) like `foobar_service` is still existing at the moment of this
call.

Note:
 - the process may run on the local node or not
 - generally not to be used, when relying on a good design
""".
-spec is_alive( pid() | ustring() | naming_utils:registration_name() ) ->
											boolean().
is_alive( TargetPid ) when is_pid( TargetPid ) ->
	is_alive( TargetPid, node( TargetPid ) );

is_alive( TargetPidString ) when is_list( TargetPidString ) ->
	TargetPid = list_to_pid( TargetPidString ),
	is_alive( TargetPid, node( TargetPid ) );

is_alive( TargetPidName ) when is_atom( TargetPidName ) ->

	TargetPid = naming_utils:get_registered_pid_for( TargetPidName,
		_RegistrationType=local_otherwise_global ),

	is_alive( TargetPid, node( TargetPid ) ).



-doc """
Tells whether the specified process (designated by its PID) supposed to run on
the target node (specified as an atom) was still existing at the moment of this
call.

Note: generally not to be used when relying on a good design; and `is_alive/1`
should be preferred.
""".
-spec is_alive( pid(), atom_node_name() ) -> boolean().
is_alive( TargetPid, Node )  ->
	is_alive( TargetPid, Node, _Verbose=true ).



-doc """
Tells whether the specified process (designated by its PID) supposed to run on
target node (specified as an atom) was still existing at the moment of this
call.

May emit trace warnings if told to be verbose.

Note: generally not to be used when relying on a good design; and `is_alive/1`
should be preferred.
""".
-spec is_alive( pid(), atom_node_name(), boolean() ) -> boolean().
is_alive( TargetPid, Node, Verbose ) when is_pid( TargetPid ) ->
	% erlang:is_process_alive/1 is more intended for debugging purposes...

	case node() of

		Node ->
			% Would fail with 'badarg' if the process ran on another node:
			erlang:is_process_alive( TargetPid );

		_OtherNode ->

			%trace_utils:debug_fmt( "Testing liveliness of process ~p "
			%   "on node ~p.", [ TargetPid, Node ] ),

			case rpc:call( Node, _Mod=erlang, _Fun=is_process_alive,
						   _Args=[ TargetPid ] ) of

				Res when is_boolean( Res ) ->
					Res;

				{ badrpc, nodedown } ->
					Verbose andalso
						trace_utils:warning_fmt( "Reporting that process "
							"of PID ~w is not alive as its node ('~ts') "
							"is reported as down.", [ TargetPid, Node ] );

				Other ->
					throw( { unexpected_liveliness_report, Other } )

			end

	end.



-doc """
Returns whether the (Myriad-enforced) debug mode is activated for the
compilation of this module.
""".

% Dispatched in actual clauses, otherwise Dialyzer will detect an
% underspecification:
%
%-spec is_debug_mode_enabled() -> boolean().

-ifdef(myriad_debug_mode).

-spec is_debug_mode_enabled() -> true.
is_debug_mode_enabled() ->
	true.

-else. % myriad_debug_mode

-spec is_debug_mode_enabled() -> false.
is_debug_mode_enabled() ->
	false.

-endif. % myriad_debug_mode



-doc "Applies the settings corresponding to the current execution target.".
-spec setup_execution_target() -> void().
setup_execution_target() ->
    setup_execution_target( get_execution_target() ).


-doc """
Applies the settings corresponding to the specified execution target, if wanting
to override this build-time setting.
""".
-spec setup_execution_target( execution_target() ) -> void().
setup_execution_target( _ExecTarget=development ) ->
    set_error_report_output( _ErrortReportOutput=standard_ellipsed );

setup_execution_target( _ExecTarget=production ) ->
    set_error_report_output( _ErrortReportOutput=standard_and_file_ellipsed ).



% Key to be used for a persistent term:
-define( error_report_output_key, myriad_error_report_output ).


-doc """
Returns a list of all valid settings in terms of general error reporting.
""".
-spec get_all_error_report_outputs() -> [ error_report_output() ].
get_all_error_report_outputs() ->
    [ standard_full, standard_ellipsed, standard_ellipsed_file_full,
      standard_and_file_ellipsed ].


-doc "Checks the specified setting in terms of general error reporting.".
-spec check_error_report_output( term() ) -> error_report_output().
check_error_report_output( T ) ->
    case lists:member( _Elem=T, _List=get_all_error_report_outputs() ) of

        true ->
            T;

        false ->
            throw( { invalid_error_report_output, T } )

    end.



-doc "Returns the current setting in terms of general error reporting.".
-spec get_error_report_output() -> error_report_output().
get_error_report_output() ->
    persistent_term:get( _K=?error_report_output_key,
                         _Default=standard_ellipsed ).


-doc """
Sets globally how error reporting shall be done in general.

Defines which kind of outputs should be used, and how, and returns information
about the number of persistent terms and the total amount of memory (in bytes)
that they use.

Various means of error reporting (in Myriad or in the layers above) should read
this setting and act accordingly.

Changing such a setting in the course of execution may be expensive if many
processes exist.
""".
-spec set_error_report_output( error_report_output() ) ->
                                            persistent_term:info().
set_error_report_output( ErrortReportOutput ) ->

    %io:format( "Setting error output setting to ~ts (was: ~ts).~n",
    %           [ ErrortReportOutput, get_error_report_output() ] ),

    persistent_term:put( _K=?error_report_output_key,
                         _V=check_error_report_output( ErrortReportOutput ) ).


-doc """
Describes the specified term in a controlled manner (shortened if needed).
""".
-spec describe_term( term() ) -> ustring().
describe_term( T ) ->
	text_utils:ellipse_fmt( "~p", [ T ] ).
