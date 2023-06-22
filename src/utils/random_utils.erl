% Copyright (C) 2007-2023 Olivier Boudeville
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


% @doc Gathering of various <b>random-related</b> facilities, based on
% probability distributions, either as Myriad built-in ones (uniform,
% exponential, gaussian, etc.) or user-defined, arbitrary ones.
%
% See random_utils_test.erl for the corresponding test.
%
-module(random_utils).



% Usage notes.

% The user specifies first a random law (see random_law_spec/0), from which the
% actual random law can be initialised (built, "compiled") so that any number of
% samples can then be obtained from it.

% Most laws come in their normal, canonical version (e.g. 'exponential'),
% typically yielding floating-point values, and also in one yielding only
% integer values, possibly even only positive ones
% (e.g. 'positive_integer_exponential').

% There were initially three basic classes of built-in random distributions:
% - uniform (a.k.a. white noise)
% - exponential
% - Gaussian (a.k.a. normal)
%
% Then various reliability-related distributions have been added (e.g. the
% Weibull family).
%
% Often distributions are generalised with parameters that allow specialising
% them into well-known distributions (which are thus special cases thereof).
%
% Upcoming built-in additions could be:
%
% - the Gamma distribution (see
% https://en.wikipedia.org/wiki/Gamma_distribution), which includes the
% exponential distribution, the (unrelated) Erlang distribution, and the
% chi-square distribution
%
% - the Dirac delta distribution (a.k.a the unit impulse)
%
% - some instantaneous distribution (to be determined)



% Functions for random management.

% As a general rule of thumb, if generating random values in an interval that
% are:
%  - integer: then both bounds are included
%  - floating point: then the lower bound is included and the upper one is
%  excluded

% In this module, "positive integer" must be understood as comprising zero
% (otherwise "strictly positive integer" shall be used).



% Design notes:
%
% Two methods are used to evaluate distribution functions, depending on their
% nature:
%
%  - for a few of them (uniform, and the ones that can be directly derived from
%  it, as exponential and gaussian), a direct sampling can be done, with
%  infinite precision (samples do not have to be gathered in buckets)
%
%  - for the rest of them (the majority; e.g. Weibull, arbitrary PDF, etc.),
%  their PDF has to be discretised; as a result, a fixed, relatively low number
%  of discretisation buckets is used
%
% In the first case (direct sampling), each sample can be seen as being its own
% bucket, and a floating-point sample, being a C double, is very unlikely to be
% drawn more than once; as a result, their probability-like will be constant,
% i.e. the inverse of the number of the drawn samples (and of course normalising
% would not change anything to that); only the density of drawing in an area
% will denote higher PDF values.
%
% Unless performing a colossal number of drawings, distributions that would be
% directly sampled cannot be represented in terms of frequency - unless they get
% discretised as well.
%
% Distributions able to be directly sampled do not require bounds to be
% specified or determined, as opposed to the ones whose PDF has to be
% discretised.



% Service usage, state and seeding:
-export([ start_random_source/3, start_random_source/1, can_be_seeded/0,
		  reset_random_source/1, stop_random_source/0,

		  get_random_module_name/0,
		  get_random_state/0, set_random_state/1,

		  get_random_seed/0, check_random_seed/1 ]).


% Functions related to random laws in general:
-export([ get_law_name/1, get_all_sample_pairs/1,
		  initialise_law/1, get_law_settings/1,
		  get_sampling_info/1, sampling_info_to_string/1,
		  law_spec_to_string/1, law_data_to_string/1 ]).


% Functions related to uniform sampling:
-export([ get_uniform_value/0, get_uniform_value/1, get_uniform_value/2,
		  get_uniform_values/2, get_uniform_values/3,

		  get_uniform_floating_point_value/1,
		  get_uniform_floating_point_value/2,

		  get_boolean/0, one_of/1, get_random_subset/2 ]).


% Functions related to exponential sampling:
-export([ get_exponential_value/1, get_positive_integer_exponential_value/1,
		  get_exponential_values/2, get_positive_integer_exponential_values/2
		]).


% Functions related to gaussian sampling:
-export([ get_gaussian_value/2, get_positive_integer_gaussian_value/2,
		  get_gaussian_values/3, get_positive_integer_gaussian_values/3 ]).



% Functions relative to other distributions:
-export([ exponential_pdf/2, gaussian_pdf/3,
		  weibull_2p_pdf/3, weibull_3p_pdf/4 ]).


% Silencing:
-export([ get_exponential_pdf/2 ]).


% Functions related to arbitrary, non-uniform sampling:
-export([ generate_alias_table_from/1,
		  get_sample_from/1, get_samples_from/2 ]).




-type seed_element() :: integer().
% Not future-proof enough (e.g. not compliant with other solutions like
% SIMD-oriented Fast Mersenne Twister).

-type seed() :: { seed_element(), seed_element(), seed_element() }.
% A type of seed for random generators.


% random:ran/0 does not seem exported, replaced by seed/0:
-type random_state() :: seed()
					  | rand:state()
					  | any().
% For simpler generators, the state is just a seed, for all the others the state
% may be much larger/more complex.
%
% Not to be mixed with the static data (random_law_data/0) corresponding to a
% random law, so that samples can be obtained from it.


-record( alias_table, {

	% Total number of sample entries:
	entry_count :: sample_count(),

	% Array referencing all declared samples:
	sample_values :: array( sample() ),

	% Array keeping track of the sample corresponding to each category:
	indexes :: array( positive_index() ),

	% Array of probability-like for each category:
	prob_likes :: array( probability_like() ) } ).


-opaque alias_table() :: #alias_table{}.
% The static information corresponding to a random law in charge of producing
% samples according to the specified discrete probability distribution, based on
% the alias method.



-type sample( T ) :: T.
% The type of a sample that can be drawn from a probability distribution; a
% probability may be indeed associated to any kind of samples (integer ones,
% strings, vectors, etc.).


-type sample() :: sample( any() ).
% Designates a sample value of unknown type.
%
% It may be a number or a symbol (for example: 'obverse', 'reverse').


-type float_sample() :: sample( float() ).
% Designates a floating-point sample value.

-type positive_float_sample() :: sample( float() ).
% Designates a positive floating-point sample value.


-type sample_count() :: count().
% A number of samples.


-type increment() :: float().
% The increment added between two discretisation steps.


-type discrete_sampling_info() :: sample_count().
% Information regarding a sampling among discrete values.


-type float_sampling_info() ::
	{ StartSample :: float_sample(), StopSample :: float_sample(),
	  sample_count() }.
% Information regarding a float-based sampling.

-type sampling_info() :: discrete_sampling_info() | float_sampling_info().
% Information regarding a (float-based) sampling.


-type sample_entry( T ) :: { sample( T ), probability_like() }.
% An entry corresponding to a sample of the specified type in a probability
% distribution.

-type sample_entry() :: sample_entry( any() ).
% An entry corresponding to a sample of unspecified type in a probability
% distribution.


-type rate() :: number().
% A rate parameter, typically for the Lambda parameter of the exponential law.


-type mean() :: number().
% An arithmetic mean of a list of numbers, that is the sum of all of the numbers
% divided by the number of numbers.
%
% See https://en.wikipedia.org/wiki/Mean#Arithmetic_mean_(AM)


-type standard_deviation() :: math_utils:standard_deviation().
% A measure of the amount of dispersion of a set of values.
%
% It is the square root of its variance.
%
% See https://en.wikipedia.org/wiki/Standard_deviation


% Section for the description of random laws (probability distributions).


-type law_name() :: atom().
% The name, as an atom, of a type of (unparametrised) law.
%
% For example, 'uniform' or 'weibull_3p'.
%
% Acts as an identifier of this type of laws.




% Subsection for law specifications.

-type random_law_spec() ::
	uniform_law_spec()
  | integer_uniform_law_spec()

  | exponential_law_spec()
  | positive_integer_exponential_law_spec()

  | gaussian_law_spec()
  | positive_integer_gaussian_law_spec()

  | weibull_2p_law_spec()
  | weibull_3p_law_spec()

  | arbitrary_law_spec().
% A user-level specification of a (parametrised) randow law, either natively
% supported or arbitrary.
%
% It is a tuple whose first element is a law identifier.



-type uniform_law_spec() :: { 'uniform', Min :: number(), Max :: number() }
						  | { 'uniform', Max :: number() }.
% A probability distribution with which all declared samples have the same
% probability of being drawn: will return random floats uniformly distributed
% in [Min,Max] if both bounds are specified, otherwise in [0,Max].


-type full_uniform_law_spec() :: { 'uniform', Min :: float(), Max :: float() }.
% Canonical, most complete uniform law specification.


-type integer_uniform_law_spec() ::
		{ 'integer_uniform', Nmin :: integer(), Nmax :: integer() }
	  |	{ 'integer_uniform', Nmax :: integer() }.
% A probability distribution with which all declared samples have the same
% probability of being drawn: will return random integers uniformly distributed
% in [Nmin,Nmax] if both bounds are specified, otherwise in [0,Nmax].


-type exponential_law_spec() :: { 'exponential', Lambda :: rate() }.
% An exponential law is fully determined when its single, "rate" parameter
% (Lambda>0) is given.
%
% The probability density function is p(x) = Lambda.exp(-Lambda.x), whose
% integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% Refer to https://en.wikipedia.org/wiki/Exponential_distribution.


-type positive_integer_exponential_law_spec() ::
		{ 'positive_integer_exponential', Lambda :: rate() }.
% An exponential law yielding only positive integer samples.
%
% May be useful for example if wanting to draw duration values.
%
% Refer to exponential_law_spec/0 for further details.



-type gaussian_law_spec() ::
		{ 'gaussian', Mu :: mean(), Sigma :: standard_deviation() }.
% A Gaussian (a.k.a. normal, bell curve) law is fully determined when its two
% parameters are given:
%
% - its mean (Mu; no value restriction), the average value of the samples
%
% - its standard deviation (Sigma>0), being expressed in the same unit as the
% samples (its square being the variance)
%
% About 68% of the samples are in [Mu-Sigma;Mu+Sigma].
% About 95.4% of the samples (i.e. almost all) are in [Mu-2.Sigma;Mu+2.Sigma].
%
% See also: https://en.wikipedia.org/wiki/Normal_distribution.


-type positive_integer_gaussian_law_spec() ::
		{ 'positive_integer_gaussian', Mu :: mean(),
		  Sigma :: standard_deviation() }.
% A Gaussian (a.k.a. normal, bell curve) law yielding only positive integer
% samples.
%
% May be useful for example if wanting to draw duration values.
%
% Refer to gaussian_law/0 for further details.


-type full_weibull_law_spec() ::
		full_weibull_2p_law_spec() | full_weibull_3p_law_spec().


-type weibull_2p_law_spec() ::
	{ 'weibull_2p', K :: positive_float(), Lambda :: positive_float() }
  | { 'weibull_2p', K :: positive_float(), Lambda :: positive_float(),
	  sample_count() }
  | full_weibull_2p_law_spec().
% The Weibull law with two parameters, whose K > 0 is the shape parameter
% (sometimes named beta, or slope) and Lambda > 0 is the scale parameter
% (sometimes named alpha, or eta, or characteristic life).
%
% A sample count and specific bounds can be specified.
%
% See also https://en.wikipedia.org/wiki/Weibull_distribution and
% https://reliawiki.org/index.php/The_Weibull_Distribution.


-type full_weibull_2p_law_spec() ::
	{ 'weibull_2p', K :: positive_float(), Lambda :: positive_float(),
	  sample_count(), bounds() }.
% Canonical, most complete Weibull law specification with two parameters.
% Refer to weibull_2p_law_spec/0 for further details.



-type weibull_3p_law_spec() ::
	{ 'weibull_3p', K :: positive_float(), Lambda :: positive_float(),
	  Gamma :: float() }
  | { 'weibull_3p', K :: positive_float(), Lambda :: positive_float(),
	  Gamma :: float(), sample_count() }
  | full_weibull_3p_law_spec().
% The Weibull law with three parameters, whose:
%
% - K > 0 is the shape parameter (sometimes named beta, or slope)
%
% - Lambda > 0 is the scale parameter (sometimes named alpha, or eta, or
% characteristic life)
%
% - Gamma (in R) is the location parameter (or failure free life)
%
% A sample count and specific bounds can be specified.
%
% See also https://en.wikipedia.org/wiki/Weibull_distribution and
% https://reliawiki.org/index.php/The_Weibull_Distribution.


-type full_weibull_3p_law_spec() ::
	{ 'weibull_3p', K :: positive_float(), Lambda :: positive_float(),
	  Gamma :: float(), sample_count(), bounds() }.
% Canonical, most complete Weibull law specification with three parameters.
% Refer to weibull_3p_law_spec/0 for further details.



-type pdf_info() ::
	pdf()
| { pdf(), sample_count() }
| full_pdf_info().
% Information regarding a PDF to be used in order to define an arbitrary law.


-type full_pdf_info() :: { pdf(), sample_count(), SampleBounds :: bounds() }.
% Most complete information regarding a PDF to be used in order to define an
% arbitrary law.



-type arbitrary_law_spec() ::
	{ 'arbitrary', Name :: any_string(),
	  discrete_probability_distribution() | full_pdf_info() }.
% User-level specification of an arbitrary random law.
%
% It includes its name, and one way of performing a reverse sampling of it:
%  - either directly from discrete, user-specified samples
%  - or through its Probability Density Function (which will then be
%  discretised) and any relevant parameters that will be applied to it (stored
%  so that they remain available afterwards); its intended sampling may be
%  specified



% Subsection regarding the runtime data of laws.
%
% They are often the same as their user specification, except for arbitrary
% laws, which embed additionally their precomputed alias table.




-type random_law_settings() :: tuple().
% The internal, law-specific settings of a law.
%
% The first element of the tuple should be the name of the law, whereas its last
% element shall be the sampling info: {law_name(), ..., sampling_info()}.


-type random_law_data() ::

  % The general rule:
  { random_law_settings(), maybe( alias_table() ) }

  | uniform_law_data()
  | integer_uniform_law_data()

  | exponential_law_data()
  | positive_integer_exponential_law_data()

  | gaussian_law_data()
  | positive_integer_gaussian_law_data()

  | weibull_law_data()

  | arbitrary_law_data().
% Static, runtime data associated to an actual random law, so that samples can
% be obtained from it.
%
% The runtime data of a law is often mostly the same as its user specification
% (except some transformations in some cases), except for laws obtained through
% discretisation of their PDF, which embed additionally their precomputed alias
% table.
%
% This is not a random state, which is a mutable internal state of a random
% generator (see random_state/0).


-type uniform_law_data() :: { uniform_law_spec(), 'undefined' }.

-type integer_uniform_law_data() :: { integer_uniform_law_spec(), 'undefined' }.

-type exponential_law_data() :: { exponential_law_spec(), 'undefined' }.

-type positive_integer_exponential_law_data() ::
		{ positive_integer_exponential_law_spec(), 'undefined' }.

-type gaussian_law_data() :: { gaussian_law_spec(), 'undefined' }.

-type positive_integer_gaussian_law_data() ::
		{ positive_integer_gaussian_law_spec(), 'undefined' }.


-type weibull_law_settings() :: { 'weibull', K :: positive_float(),
								  Lambda :: positive_float(), sampling_info() }.
% Internal settings of the Weibull law.

-type weibull_law_data() :: { weibull_law_settings(), alias_table() }.
% A bit like weibull_law_data/0, except that its precomputed alias table is
% stored as well.


-type arbitrary_law_pseudo_spec() ::
		{ 'arbitrary', Name :: bin_string(), sampling_info() }.
% A (pseudo) specification for an arbitrary law, to be stored in a law data.


-type arbitrary_law_data() :: { arbitrary_law_pseudo_spec(), alias_table() }.
% The definition of an arbitrary law.
%
% Quite like arbitrary_law_spec/0, except for the string type, the PDF that is
% dropped, any parameters that are kept for further reference, and an additional
% precomputed alias table stored.



-type discrete_probability_distribution( T ) :: [ sample_entry( T ) ].
% The specification of a discrete probability distribution (a.k.a. frequency
% distribution) whose samples are of the specified type.
%
% For example discrete_probability_distribution(integer()) or
% discrete_probability_distribution(vector3:vector3()).
%
% Samples of null probability are usually omitted, as such a probability is
% implicit and results in the corresponding sample never to be drawn.
%
% At least an entry with a non-null probability must be defined for any drawing
% to occur.
%
% Preferably a given sample value is specified only once, i.e. is declared in a
% single entry (otherwise the distribution will behave as if the probabilities
% for that sample were summed - yet the distribution will be less compact).
%
% Such a distribution can be obtained either directly or by sampling a fun(T ->
% probability_like()) function over at least a part of its domain.
%
% It may no be normalised.


-type discrete_probability_distribution() ::
		discrete_probability_distribution( any() ).
% The specification of a discrete probability distribution of unknown sample
% type.
%
% For example: [{'red', 0.1}, {'blue', 0.2}, {'green', 0.6}].


-type pdf( S ) :: fun( ( S ) -> probability_like() ).
% A Probability Density Function telling, for a given sample of type S, its
% corresponding probability-like value.
%
% See also the math_utils:sample* functions and get_samples_from/2.


-type pdf() :: pdf( any() ).
% A Probability Density Function for samples of unspecified type.


-type exponential_pdf() :: pdf().
% A PDF of the exponential distribution.

-type gaussian_pdf() :: pdf().
% A PDF of the exponential distribution.



-type weibull_pdf() :: weibull_2p_pdf() | weibull_3p_pdf().


-type weibull_2p_pdf() :: pdf().
% A PDF of the two-parameter Weibull distribution.

-type weibull_3p_pdf() :: pdf().
% A PDF of the three-parameter Weibull distribution.



-export_type([ seed_element/0, seed/0, random_state/0, alias_table/0,
			   sample/0, sample/1, float_sample/0, positive_float_sample/0,
			   sample_count/0,

			   increment/0,
			   discrete_sampling_info/0, float_sampling_info/0, sampling_info/0,

			   sample_entry/0, sample_entry/1,
			   rate/0, mean/0, standard_deviation/0,
			   law_name/0 ]).


-export_type([ discrete_probability_distribution/0,
			   discrete_probability_distribution/1,
			   pdf/0, pdf/1,
			   exponential_pdf/0, gaussian_pdf/0,
			   weibull_2p_pdf/0, weibull_3p_pdf/0 ]).


% Law specs:
-export_type([ random_law_spec/0,
			   uniform_law_spec/0, integer_uniform_law_spec/0,
			   exponential_law_spec/0, positive_integer_exponential_law_spec/0,
			   gaussian_law_spec/0, positive_integer_gaussian_law_spec/0,
			   weibull_2p_law_spec/0, weibull_3p_law_spec/0,
			   arbitrary_law_spec/0 ]).


% Law runtime data:
-export_type([ random_law_settings/0, random_law_data/0,
			   uniform_law_data/0, integer_uniform_law_data/0,
			   exponential_law_data/0, positive_integer_exponential_law_data/0,
			   gaussian_law_data/0, positive_integer_gaussian_law_data/0,
			   weibull_law_data/0,
			   arbitrary_law_data/0 ]).


% The default number of steps used to discretise a PDF:
-define( default_pdf_sample_count, 512 ).



% Implementation notes.

% About pseudorandom number generator (PRNG):
%
% If use_crypto_module is defined, the (now deprecated here, for this use)
% crypto module will be used, otherwise the rand module (which is the default
% now) will be used instead (the random module being now deprecated).
%
% Currently the crypto module is not used by default, as:
%
% - not all Erlang VMs can be built with the proper SSH support
%
% - it is unclear whether the crypto module can be seeded like the random module
% can be (probably it cannot be)
%
% - there is no crypto function returning a random float uniformly distributed
% between 0.0 and 1.0, and it may not be easy to implement it from what is
% available
%
% - we do not know whether the seed is per-node (most likely), or per-process
%
% Therefore crypto cannot be easily swapped with other random generators.
%
% Finally, the requirements of a Cryptographically-secure PRNG (CSPRNG) exceed
% the general PRNGs, and may be useless / less appropriate / of higher costs in
% other contexts. Refer to our hash_utils module for more information on that
% topic.
%
% The current module considered using TinyMT and/or SFMT, yet now they have been
% superseded by the xorshift, xoroshiro, and xoshiro algorithms by Sebastiano
% Vigna, and the rand module offers variations of the xorshift and xoroshiro
% algorithms, called exsss and exro928ss.
%
% Of course, switching random engines will generate different random series.
%
% They may also have different behaviours (e.g. with regards to processes not
% being explicitly seeded, inheriting from a seed that is constant or not - the
% shortest path to break reproducibility).
%
% Note that modules relying on an implicit state (e.g. for seeding) generally
% use the process dictionary to store it (e.g. 'rand').

%-define(use_crypto_module,).


% Shorthands:

-type array( T ) :: array:array( T ).

-type count() :: basic_utils:count().
-type positive_index() :: basic_utils:positive_index().


-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().


-type probability_like() :: math_utils:probability_like().
% Any number that can be interpreted ultimately as a probability, at least
% relatively to others.
%
% Notably they may not be normalised (with their sum differing from 1.0).


-type bounds() :: math_utils:bounds().

-type probability() :: math_utils:probability().


-type positive_float() :: type_utils:positive_float().
%-type non_negative_float() :: type_utils:non_negative_float().


-import( math, [ pow/2, exp/1 ] ).


% Apparently, as soon as functions are defined within preprocessor guards, their
% definition is ignored by edoc, resulting in edoc failing because of multiple
% 'doc' tags at the first function defined outside of these guards.
%
% As, if we copied their documentation there, this would not work either (the
% corresponding functions not being found), we just disabled their 'doc' tags
% ('at' doc being replaced with 'doc:').


% Specs gathered here, because of macro guards.
-spec start_random_source( seed_element(), seed_element(), seed_element() ) ->
								random_state().

-spec start_random_source( 'default_seed' | 'time_based_seed' | seed() ) ->
								void().

-spec can_be_seeded() -> boolean().

-spec reset_random_source( 'default_seed' | 'time_based_seed' | seed() ) ->
								void().

-spec stop_random_source() -> void().


-spec get_uniform_value() -> float().

-spec get_uniform_value( pos_integer() ) -> pos_integer().

-spec get_uniform_value( integer(), integer() ) -> integer().

-spec get_uniform_floating_point_value( number() ) -> float().
-spec get_uniform_floating_point_value( number(), number() ) -> float().

-spec get_random_state() -> maybe( random_state() ).
-spec set_random_state( random_state() ) -> void().



% @doc Generates a list of Count elements uniformly drawn in [1,N].
-spec get_uniform_values( pos_integer(), sample_count() ) -> [ pos_integer() ].
get_uniform_values( N, Count ) ->
	get_uniform_values_helper( N, Count, _Acc=[] ).


get_uniform_values_helper( _N, _Count=0, Acc ) ->
	Acc;

get_uniform_values_helper( N, Count, Acc ) ->
	get_uniform_values_helper( N, Count-1, [ get_uniform_value( N ) | Acc ] ).



% @doc Generates a list of Count elements uniformly drawn in [Nmin,Nmax].
-spec get_uniform_values( integer(), integer(), sample_count() ) ->
			[ integer() ].
get_uniform_values( Nmin, Nmax, Count ) ->
	get_uniform_values_helper( Nmin, Nmax, Count, _Acc=[] ).


% (helper)
get_uniform_values_helper( _Nmin, _Nmax, _Count=0, Acc ) ->
	Acc;

get_uniform_values_helper( Nmin, Nmax, Count, Acc ) ->
	get_uniform_values_helper( Nmin, Nmax, Count - 1,
							   [ get_uniform_value( Nmin, Nmax ) | Acc ] ).


% To test compilation:
%-define(use_crypto_module,).


% Now crypto is hardly used anymore; however these are the doc tags that edoc
% reads.
%
-ifdef(use_crypto_module).


% crypto module used here.
%
% Warning: the seed and state management are presumably global (not
% per-process).


% @doc Starts the random source with specified seeding.
start_random_source( _A, _B, _C ) ->
	throw( crypto_module_cannot_be_seeded ).



% @doc Starts the random source with specified seeding.
start_random_source( default_seed ) ->

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:info_fmt( "~w starting random source with crypto.",
							  [ self() ] ) ),

	ok = crypto:start();

start_random_source( time_based_seed ) ->
	throw( crypto_module_cannot_be_seeded ).


% @doc Tells whether this random source can be seeded.
%
% crypto cannot be seeded, but rand can.
%
can_be_seeded() ->
	false.


% @doc Resets the random source with a new seed.
reset_random_source( _Seed ) ->
	throw( crypto_module_cannot_be_reset ).


% @doc Stops the random source.
stop_random_source() ->
	ok = crypto:stop().



% @doc Returns a random float uniformly distributed between 0.0 (included) and
% 1.0 (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value() ->
	% Not available: crypto:rand_uniform( 0.0, 1.0 ).
	throw( not_available ).



% @doc Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value( N ) ->
	crypto:rand_uniform( 1, N+1 ).



% @doc Returns an integer random value generated from an uniform distribution in
% [Nmin,Nmax] (thus with both bounds included), updating the random state in the
% process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value( Nmin, Nmax ) when Nmin =< Nmax ->
	crypto:rand_uniform( Nmin, Nmax+1 ).


% @doc Returns a floating-point random value in [0.0,N[ generated from an
% uniform distribution.
%
% Given a number (integer or float) N (positive or not), returns a random
% floating-point value uniformly distributed between 0.0 (included) and N
% (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( N ) ->
	throw( not_available ).


% @doc Returns a floating-point random value in [Nmin, Nmax[ generated from an
% uniform distribution.
%
% Given two numbers (integer or float) Nmin and Nmax (each being positive or
% not), returns a random floating-point value uniformly distributed between Nmin
% (included) and Nmax (excluded), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( Nmin, Nmax ) ->
	throw( not_available ).



% @doc Returns the name of the module managing the random generation.
%
% Spec already specified, for all random settings.
%
-spec get_random_module_name() -> 'crypto'.
get_random_module_name() ->
	crypto.



% @doc Returns the random state of this process (it is useful for example for
% process serialisations).
%
% Spec already specified, for all random settings.
%
get_random_state() ->
	% At least: not implemented yet.
	throw( not_available ).



% @doc Sets the random state of this process (it is useful for example for
% process serialisations).
%
% Spec already specified, for all random settings.
%
set_random_state( _NewState ) ->
	% At least: not implemented yet.
	throw( not_available ).





-else. % use_crypto_module not defined below:


% Here we do not use the 'crypto' module; we use the 'rand' one (replacing the
% deprecated 'random' module), and this is the default, most common setting now.


% For the 'random' module, according to
% http://osdir.com/ml/erlang-questions-programming/2013-10/msg00235.html one
% must ensure that the initial values are large and different enough.
%
% Anyway, starting from OTP 18.0, we switched from the 'random' module to the
% new 'rand' module, offering better service and control.
%
% They have different semantics though: if a process is not explicitly seeded,
% 'random' will assign a constant seed while 'rand' will assign a non-constant
% one.

% Default rand module used here.
%
% The seed and state management is per-process. We prefer that. The process
% dictionary (based on the 'rand_seed' key) is used to store this random state
% (otherwise we stay away from any use of this impure dictionary).



% Refer to http://www.erlang.org/doc/man/rand.html for more information about
% algorithms:

% Xorshift116+, 58 bits precision and period of 2^116-1, 320 bytes per state, on
% 64-bit:
%
% (corrected version of exsplus, yet now superseded by exrop, see below)
%
%-define( rand_algorithm, exsp ).



% Xoroshiro116+, 58 bits precision and period of 2^116-1
%
% Jump function: equivalent to 2^64 calls.
%
% Default since OTP 20, to be used in most cases:
%
-define( rand_algorithm, exrop ).



% Xorshift64*, 64 bits precision and a period of 2^64-1, 336 bytes per state on
% 64-bit:
%
%-define( rand_algorithm, exs64 ).


% Xorshift1024*, 64 bits precision and a period of 2^1024-1 (most expensive of
% the built-in random algorithms, 856 bytes per state on 64-bit):
%
% (corrected version, to be used instead of exsplus)
%
%-define( rand_algorithm, exs1024s ).



% doc: Starts the random source with specified seeding.
%
% Note: if a process does not explicitly select a seed, with 'rand' a
% non-constant seed will be assigned. For reproducibility, start your random
% sources with a seed of your own.
%
start_random_source( A, B, C ) ->

	Seed = { A, B, C },

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:info_fmt( "~w starting random source with rand (~p), "
			"seeded with ~w.", [ self(), ?rand_algorithm, Seed ] ) ),

	%random:seed( A, B, C ).
	rand:seed( ?rand_algorithm, Seed ).



% doc: Seeds the random number generator, with specified seeding, or with a
% default seed (if wanting to obtain the same random series at each run) or with
% current time (if wanting "real", non-reproducible randomness).
%
% Spec already specified, for all random settings.
%
% Note: if a process does not explicitly select a seed, with 'rand' a
% non-constant seed will be assigned. For reproducibility, start explicitly your
% random sources.
%
start_random_source( _Seed={ A, B, C } ) ->
	start_random_source( A, B, C );


start_random_source( default_seed ) ->

	% Use default (fixed) values in the process dictionary:
	%random:seed();

	% random:seed/0 was using a constant seed, not rand:seed/1, so we have to
	% provide a constant seed by ourselves:
	%
	ConstantSeed = { _A=17, _B=79, _C=1111 },

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:info_fmt( "~w starting random source with rand (~p), "
			"using default constant seed ~w.",
			[ self(), ?rand_algorithm, ConstantSeed ] ) ),

	rand:seed( ?rand_algorithm, ConstantSeed );


start_random_source( time_based_seed ) ->

	% Each run will result in different random series (erlang:now/0 was
	% previously used):
	%
	% (refer to:
	% http://erlang.org/doc/apps/erts/time_correction.html#Erlang_System_Time)
	%
	T = { A, B, C } = { erlang:monotonic_time(), erlang:unique_integer(),
						erlang:time_offset() },

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:info_fmt( "~w forging time-based seed ~p.", [ self(), T ] ),
		basic_utils:ignore_unused( T ) ),

	% Directly inspired from third example in
	% http://osdir.com/ml/erlang-questions-programming/2013-10/msg00244.html:
	%
	start_random_source( A + erlang:phash2( C ), B, 690123 + 16384*C ).



% doc rand can be seeded.
can_be_seeded() ->
	true.



% doc: Resets the random source with a new seed.
reset_random_source( Seed ) ->
	% New seeding (stored in the process dictionary), as opposed to the setting
	% of a previously defined state:
	%
	start_random_source( Seed ).



% doc: Stops the random source.
stop_random_source() ->
	ok.



% doc: Returns a random float uniformly distributed between 0.0 (included) and
% 1.0 (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value() ->
	%random:uniform().
	rand:uniform().



% doc: Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value( N ) when is_integer( N ) ->
	%random:uniform( N ).
	rand:uniform( N );

get_uniform_value( N ) ->
	throw( { not_integer, N } ).



% doc: Returns an integer random value generated from an uniform distribution in
% [Nmin,Nmax] (i.e. both bounds included), updating the random state in the
% process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value( Nmin, Nmax ) when is_integer( Nmin )
					andalso is_integer( Nmax ) andalso Nmin =< Nmax ->

	% For example if Nmin = 3, Nmax = 5, we can draw value in [3, 4, 5], hence:
	%
	% N = 5 - 3 + 1 = 3.
	%
	N = Nmax - Nmin + 1,

	% Drawn in [1,N]:
	get_uniform_value( N ) + Nmin - 1;

get_uniform_value( Nmin, Nmax ) ->
	throw( { not_integer_bounds, { Nmin, Nmax } } ).



% doc: Returns a floating-point random value in [0.0,N[ generated from an
% uniform distribution.
%
% Given a number (integer or float) N (positive or not), returns a random
% floating-point value uniformly distributed between 0.0 (included) and N
% (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( N ) ->
	% Generated float in [0.0, 1.0[:
	N * rand:uniform().


% doc: Returns a floating-point random value in [Nmin, Nmax[ generated from an
% uniform distribution.
%
% Given two numbers (integer or float) Nmin and Nmax (each being positive or
% not), returns a random floating-point value uniformly distributed between Nmin
% (included) and Nmax (excluded), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( Nmin, Nmax ) ->

	%trace_utils:debug_fmt( "Generating uniform value in [~w,~w].",
	%						[ Nmin, Nmax ] ),

	% Generated float in [0.0, 1.0[:
	Nmin + ( Nmax - Nmin ) * rand:uniform().



% doc: Returns the name of the module managing the random generation.
%
% Spec already specified, for all random settings.
%
-spec get_random_module_name() -> 'rand'. %'random'.
get_random_module_name() ->
	%random.
	rand.


% doc: Returns the random state of the current process (it is useful for example
% for process serialisations).
%
% Spec already specified, for all random settings.
%
get_random_state() ->

	% Read from the process dictionary:

	% Actually, no state should not be considered as an error:
	%case erlang:get( random_seed ) of
	%
	%   undefined ->
	%       % Probably that there has been no prior seeding:
	%       throw( random_state_not_available );
	%
	%   S ->
	%       S
	%
	%end.

	% May return 'undefined', if not seeded yet:
	%erlang:get( random_seed ).
	%erlang:get( rand_seed ).
	% Best:
	rand:export_seed().



% doc: Sets the random state of this process (it is useful for example for
% process serialisations).
%
% Spec already specified, for all random settings.
%
set_random_state( RandomState ) ->

	% All are in the process dictionary (beware!):
	%erlang:put( random_seed, NewState ).
	%erlang:put( rand_seed, NewState ).
	rand:seed( RandomState ).


-endif. % use_crypto_module not defined






% Now sections that do not depend on defines.


% Seeding section.


% The upper bound for a seed element.
-define( seed_upper_bound, 65500 ).


% @doc Returns a seed obtained from the random source in use.
%
% This is a randomly-determined seed, meant to be used to create another random
% generator.
%
-spec get_random_seed() -> seed().
get_random_seed() ->
	{ get_uniform_value( ?seed_upper_bound ),
	  get_uniform_value( ?seed_upper_bound ),
	  get_uniform_value( ?seed_upper_bound ) }.



% @doc Checks that the specified seed is valid.
%
% For example, at least with some algorithms, {0, 0, 0} does not yield a correct
% random series.
%
-spec check_random_seed( seed() ) -> void().
check_random_seed( { A, B, C } ) when is_integer( A ) andalso is_integer( B )
		andalso is_integer( C ) andalso A > 0 andalso B > 0 andalso C > 0 ->
	ok;

check_random_seed( S ) ->
	throw( { invalid_random_seed, S } ).



% @doc Initialises the specified random law from its specification, so that as
% many samples as wanted can be drawn from its returned precomputed data
% afterwards.
%
-spec initialise_law( random_law_spec() ) -> random_law_data().
% First type of direct arbitrary spec: already with a probability distribution
% (as opposed to a PDF); easy cases:
%
initialise_law( _LS={ uniform, Max } ) ->
	CanonSpec = canonicalise_pdf_based_spec( { uniform, _Min=0, Max } ),
	{ CanonSpec, undefined };

initialise_law( LS={ uniform, _Min, _Max } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, undefined };


initialise_law( _LS={ integer_uniform, Max } ) ->
	CanonSpec = canonicalise_pdf_based_spec( { integer_uniform, _Min=0, Max } ),
	{ CanonSpec, undefined };

initialise_law( LS={ integer_uniform, _Min, _Max } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, undefined };


initialise_law( LS={ exponential, _Lambda } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, undefined };

initialise_law( LS={ positive_integer_exponential, _Lambda } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, undefined };


initialise_law( LS={ gaussian, _Mu, _Sigma } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, undefined };

initialise_law( LS={ positive_integer_gaussian, _Mu, _Sigma } ) ->
	CanonSpec = canonicalise_pdf_based_spec( LS ),
	{ CanonSpec, undefined };


initialise_law( _LS={ arbitrary, AnyName, DistProbLikes } )
									when is_list( DistProbLikes ) ->

	Name = text_utils:ensure_binary( AnyName ),
	SampleCount = length( DistProbLikes ),

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:info_fmt( "Initialising an arbitrary random law "
			"named '~ts', from a distribution comprising ~B probabilities.",
			[ Name, SampleCount ] ) ),

	DiscreteSamplingInfo = SampleCount,

	PseudoSpec = { arbitrary, Name, DiscreteSamplingInfo },

	AliasTable = generate_alias_table_from( DistProbLikes ),

	_ArbitraryLawData={ PseudoSpec, AliasTable };


% Second type of arbitrary spec: with a PDF (with variations); no relevant
% matching possible in head due to tuples of various sizes:
%
initialise_law( LS ) ->

	case canonicalise_pdf_based_spec( LS ) of

		{ _CanSpec={ weibull_2p, K, Lambda, SampleCount,
					 WbBounds={ Min, Max } }, Inc, WbPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Weibull-2P law of K=~f "
					"and Lambda=~f, discretised on interval ~ts "
					"with ~B points (increment: ~f).",
					[ K, Lambda, math_utils:bounds_to_string( WbBounds ),
					  SampleCount, Inc ] ),
				basic_utils:ignore_unused( WbBounds ) ),

			SampledPDFPairs = math_utils:sample_as_pairs( WbPDFFun, Min, Max,
														  Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),
			SamplingInfo = { Min, Max, SampleCount },
			WbLawSettings = { weibull_2p, K, Lambda, SamplingInfo },
			_ArbitraryLawData={ WbLawSettings, AliasTable };


		{ _CanSpec={ weibull_3p, K, Lambda, Gamma, SampleCount,
					 WbBounds={ Min, Max } }, Inc, WbPDFFun } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:debug_fmt( "Initialising a Weibull-3P law of K=~f, "
					"Lambda=~f and Gamma=~f, discretised on interval ~ts "
					"with ~B points (increment: ~f).",
					[ K, Lambda, Gamma, math_utils:bounds_to_string( WbBounds ),
					  SampleCount, Inc ] ),
				basic_utils:ignore_unused( WbBounds ) ),

			SampledPDFPairs = math_utils:sample_as_pairs( WbPDFFun, Min, Max,
														  Inc ),

			AliasTable = generate_alias_table_from( SampledPDFPairs ),
			SamplingInfo = { Min, Max, SampleCount },
			WbLawSettings = { weibull_3p, K, Lambda, Gamma, SamplingInfo },
			_ArbitraryLawData={ WbLawSettings, AliasTable };


		{ _CanSpec={ arbitrary, BinName,
			_CanonPDFInfo={ LFun, SampleCount, CanBounds={ Min, Max } } },
		  Inc } ->

			cond_utils:if_defined( myriad_debug_random,
				trace_utils:info_fmt( "Initialising an arbitrary random law "
					"named '~ts', whose PDF is ~p, sampled on interval ~ts "
					"with ~B points (increment: ~f).",
					[ BinName, LFun, math_utils:bounds_to_string( CanBounds ),
					  SampleCount, Inc ] ),
				basic_utils:ignore_unused( CanBounds ) ),

			SampledPDFPairs = math_utils:sample_as_pairs( LFun, Min, Max, Inc ),

			% Uniform sampling; not normalised:
			AliasTable = generate_alias_table_from( SampledPDFPairs ),
			SamplingInfo = { Min, Max, SampleCount },
			PseudoSpec = { arbitrary, BinName, SamplingInfo },
			_ArbitraryLawData={ PseudoSpec, AliasTable }

	end.



% @doc Returns the settings of the law specified by its data.
-spec get_law_settings( random_law_data() ) -> random_law_settings().
get_law_settings( _LawData={ Settings, _AliasTable } ) ->
	Settings.


% @doc Returns the name of the law corresponding to the specified law settings.
-spec get_law_name( random_law_settings() ) -> law_name().
get_law_name( LawSettings ) ->
	element( _Index=1, LawSettings ).


% @doc Returns the sampling information stored in the specified law settings.
-spec get_sampling_info( random_law_settings() ) -> sampling_info().
get_sampling_info( SettingsTuple ) ->
	type_utils:get_last_tuple_element( SettingsTuple ).



% Section for the generation of random samples according to a uniform law.


% @doc Returns a boolean random value generated from a uniform distribution.
%
% Therefore true and false are equally likely to be returned.
%
-spec get_boolean() -> boolean().
get_boolean() ->
	get_uniform_value( 0, 100 ) >= 49.



% @doc Returns a random element of the specified list, selected according to a
% uniform distribution.
%
-spec one_of( [ any() ] ) -> any().
one_of( ListOfThings ) ->
	Index = get_uniform_value( length( ListOfThings ) ),
	lists:nth( Index, ListOfThings ).



% @doc Returns a list of the specified number of unique elements drawn from the
% specified input list (so that there is no duplicate in the returned list).
%
% Note: defined to ease interface look-up; one should use directly
% list_utils:draw_elements_from/2 instead.
%
-spec get_random_subset( count(), list() ) -> list().
get_random_subset( ValueCount, InputList ) ->
	list_utils:draw_elements_from( InputList, ValueCount ).





% Section for the generation of random samples according to an exponential law.
%
% Note: each of the three forms comes in two versions, with floating-point or
% (positive) integer values being returned.



% @doc Returns an exponential floating-point random value, with Lambda being the
% rate parameter.
%
% As get_uniform_value/1 never returns 1.0, a strictly positive value is always
% returned.
%
% See exponential_law() for further details.
%
% Using ad-hoc inverse transform sampling here.
%
-spec get_exponential_value( rate() ) -> float().
get_exponential_value( Lambda ) ->

	%trace_utils:debug_fmt( "Lambda=~p", [ Lambda ] ),

	% Note: with Erlang, math:log(x) is ln(x):
	- math:log( get_uniform_value() ) / Lambda.



% @doc Returns an exponential (positive) integer random value, with Lambda being
% the rate parameter.
%
% See get_exponential_value/1 for further details.
%
-spec get_positive_integer_exponential_value( rate() ) -> non_neg_integer().
get_positive_integer_exponential_value( Lambda ) ->
	round( get_exponential_value( Lambda ) ).



% @doc Returns a list of Count exponential values according to the specified
% Lambda setting.
%
% See get_exponential_value/1 for further details.
%
-spec get_exponential_values( rate(), sample_count() ) -> [ float() ].
get_exponential_values( Lambda, Count ) ->
	generate_exponential_list( Lambda, Count, _Acc=[] ).



% The generate_*_list could rely on higher-order functions.


% @doc Generates a list of Count exponential random values.
%
% (helper)
-spec generate_exponential_list( rate(), sample_count(), [ float() ] ) ->
										[ float() ].
generate_exponential_list( _Lambda, _Count=0, Acc ) ->
	Acc;

generate_exponential_list( Lambda, Count, Acc ) ->
	generate_exponential_list( Lambda, Count-1,
							   [ get_exponential_value( Lambda ) | Acc ] ).



% @doc Returns a list of Count positive integer exponential values according to
% the specified Lambda setting.
%
% See get_exponential_value/1 for further details.
%
-spec get_positive_integer_exponential_values( rate(), sample_count() ) ->
											[ non_neg_integer() ].
get_positive_integer_exponential_values( Lambda, Count ) ->
	generate_positive_integer_exponential_list( Lambda, Count, _Acc=[] ).


% (helper)
generate_positive_integer_exponential_list( _Lambda, _Count=0, Acc ) ->
	Acc;

generate_positive_integer_exponential_list( Lambda, Count, Acc ) ->
	generate_positive_integer_exponential_list( Lambda, Count-1,
		[ get_positive_integer_exponential_value( Lambda ) | Acc ] ).






% Section for the generation of random samples according to a gaussian law.



% @doc Returns a random value generated from the normal (Gaussian) distribution
% with specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns a random
% floating-point value drawn according to the corresponding Gaussian law,
% updating the state in the process dictionary.
%
-spec get_gaussian_value( mean(), standard_deviation() ) -> float().
get_gaussian_value( Mu, Sigma ) ->
	sigma_loop( Mu, Sigma ).



% @doc Returns a non-negative integer random value generated from the
% normal (Gaussian) distribution with specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns random integers drawn
% according the corresponding Gaussian law, updating the state in the process
% dictionary.
%
% The result is a non-negative integer (not a float). Values will be drawn until
% they are non-negative.
%
-spec get_positive_integer_gaussian_value( mean(), standard_deviation() ) ->
											non_neg_integer().
get_positive_integer_gaussian_value( Mu, Sigma ) ->
	sigma_loop_positive_integer( Mu, Sigma ).




% @doc Generates a new gaussian value and updates the state.
%
% Mu is the mean, Sigma is the standard deviation (variance being its square).
%
% Returns the computed value.
%
% See also
% https://en.wikipedia.org/wiki/Normal_distribution#Computational_methods
%
-spec sigma_loop( mean(), standard_deviation() ) -> float().
sigma_loop( Mu, Sigma ) ->

	% Best (most efficient) implementation that could be used in the future:
	% https://en.wikipedia.org/wiki/Ziggurat_algorithm

	% Note: at least for (Mu=0, Sigma=1), rand:normal/0 could be used.

	% Using here the second best approach, the Marsaglia polar method (see
	% https://en.wikipedia.org/wiki/Marsaglia_polar_method); used for example by
	% C++11 GNU GCC libstdc++.

	% So V1 and V2 are in [-1.0;1.0[:
	V1 = 2.0 * get_uniform_value() - 1.0,

	% Supposedly independent from V1:
	V2 = 2.0 * get_uniform_value() - 1.0,

	S  = (V1 * V1) + (V2 * V2),

	% Loop until S in ]0,1[:
	case S >= 1.0 orelse S =:= 0.0 of

		% Rejected:
		true ->
			sigma_loop( Mu, Sigma );

		_False ->

			% Here S in ]0;1.0[ (note that 1.0 should be included, possibly by
			% transforming any 0.0 in a 1.0):
			%
			%trace_utils:debug_fmt( "Mu = ~p, Sigma = ~p, S = ~p.",
			%                       [ Mu, Sigma, S ] ),

			% math:log/1 is the Natural Log (base e log):
			Scale = math:sqrt( ( -2.0 * math:log( S ) ) / S ),

			% Adjust for standard deviation and mean:
			Mu + Sigma * Scale * V1

	end.



% @doc Generates a new integer non-negative gaussian value and updates the
% state.
%
% Returns the computed value.
%
-spec sigma_loop_positive_integer( mean(), standard_deviation() ) ->
												non_neg_integer().
sigma_loop_positive_integer( Mu, Sigma ) ->

	% Loops until a positive integer is found:
	case round( sigma_loop( Mu, Sigma ) ) of

		TriedValue when TriedValue < 0 ->
			sigma_loop_positive_integer( Mu, Sigma );

		NonNegativeValue ->
			NonNegativeValue

	end.



% @doc Returns a list of Count Gaussian values.
%
% Given a mean Mu and a standard deviation Sigma, returns random floating-point
% values drawn according the corresponding Gaussian law, updating the state in
% the process dictionary.
%
-spec get_gaussian_values( mean(), standard_deviation(), sample_count() ) ->
													[ float() ].
get_gaussian_values( Mu, Sigma, Count ) ->
	generate_gaussian_list( Mu, Sigma, Count, _Acc=[] ).



% @doc Generates a list of Count Gaussian random values.
%
% (helper)
generate_gaussian_list( _Mu, _Sigma, _Count=0, Acc ) ->
	Acc;

generate_gaussian_list( Mu, Sigma, Count, Acc ) ->
	generate_gaussian_list( Mu, Sigma, Count-1,
							[ sigma_loop( Mu, Sigma ) | Acc ] ).



% @doc Returns a list of Count positive integer Gaussian values.
%
% Given a mean Mu and a standard deviation Sigma, returns random integers drawn
% according the corresponding Gaussian law, updating the state in the process
% dictionary.
%
-spec get_positive_integer_gaussian_values( mean(), standard_deviation(),
						sample_count() ) -> [ non_neg_integer() ].
get_positive_integer_gaussian_values( Mu, Sigma, Count ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count ).



% @doc Generates a list of Count positive integer Gaussian random values.
-spec generate_positive_integer_gaussian_list( mean(), standard_deviation(),
			sample_count() ) -> [ non_neg_integer() ].
generate_positive_integer_gaussian_list( Mu, Sigma, Count ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count, [] ).


% (helper)
generate_positive_integer_gaussian_list( _Mu, _Sigma, _Count=0, Acc ) ->
	Acc;

generate_positive_integer_gaussian_list( Mu, Sigma, Count, Acc ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count-1,
		[ erlang:round( sigma_loop_positive_integer( Mu, Sigma ) ) | Acc ] ).





% Section for the generation of random samples, including from arbitrary,
% non-uniform distributions.
%
% Drawing from user-specified random laws (probability distributions / PDFs,
% i.e. Probability Density Functions) corresponds to operating inverse transform
% sampling (see https://en.wikipedia.org/wiki/Inverse_transform_sampling).

% Here, such sampling is done on discrete (as opposed to continuous)
% probabilities, which can be done efficiently thanks to the alias method (see
% https://en.wikipedia.org/wiki/Alias_method), also known as the Walker method.
%
% The current implementation derives directly notably from the one kindly shared
% (Apache License Version 2.0) by Sergey Prokhorov (email: me at seriyps dot
% ru), refer to https://gist.github.com/seriyps/5593193, which itself derived
% from a Python implementation.



% @doc Returns a precomputed alias table used in order to produce samples
% according to the specified discrete probability distribution.
%
% These samples do not have to be normalised first.
%
% For example MyDistribAliasTable = random_utils:generate_alias_table_from(
%   [{a,10}, {b,20}, {c,40}, {d,30}]).
%
% or MyDistribAliasTable = random_utils:generate_alias_table_from(
%   [{"hello",0.6}, {"goodbye",0.4}]).
%
% From this (const) table, computed once for all and whose construction does not
% depend on any other random state (it is deterministic), any number of samples
% respecting said distribution can be drawn.
%
% This preprocessing uses O(N) time, where N is the number of declared samples
% of the corresponding distribution, in order to generate its returned table.
%
-spec generate_alias_table_from( discrete_probability_distribution() ) ->
											alias_table().
generate_alias_table_from( DiscreteProbDist ) ->
	{ SampleValues, ProbLikes } = lists:unzip( DiscreteProbDist ),
	generate_alias_table_from( SampleValues, ProbLikes ).


% (helper)
-spec generate_alias_table_from( [ sample() ], [ probability_like() ] ) ->
										alias_table().
generate_alias_table_from( SampleValues, ProbLikes ) ->

	EntryCount = length( ProbLikes ),

	% Notations from https://en.wikipedia.org/wiki/Alias_method#Table_generation

	% Ui = n.pi:

	% Sum expected to be non-null (at least one non-null probability requested):
	Factor = EntryCount / lists:sum( ProbLikes ),

	% Normalised:
	ScaledProbs = [ Factor * PL || PL <- ProbLikes ],

	{ UnderFulls, OverFulls } = split_by_index_fullness( ScaledProbs ),

	%trace_utils:debug_fmt( "EntryCount = ~p, UnderFulls = ~p, OverFulls = ~p",
	%                       [ EntryCount, UnderFulls, OverFulls ] ),

	{ Indexes, UpdatedProbLikes } = fill_entries( UnderFulls, OverFulls,
		array:new( EntryCount ), array:from_list( ScaledProbs ) ),

	%trace_utils:debug_fmt( "Sample value array: ~p~nIndex array: ~p~n"
	%   "Prob-like array: ~p",
	%   [ SampleValues, array:to_list( Indexes ),
	%     array:to_list( UpdatedProbLikes ) ] ),

	SampleArray = array:from_list( SampleValues ),

	#alias_table{ entry_count=EntryCount,
				  sample_values=SampleArray,
				  indexes=Indexes,
				  prob_likes=UpdatedProbLikes }.



% Returns two index lists, the second corresponding to the "overfull" group
% (where Ui > 1), the first to the "underfull" group (where (Ui < 1 and Ki has
% not been initialised) or (where Ui = 1 or Ki has been initialised) - that is,
% respectively, the "strict underfull" subgroup and the "exactly full" one.
%
% (helper)
%
-spec split_by_index_fullness( [ probability_like() ] ) ->
					{ UnderFulls :: [ positive_index() ],
					  OverFulls  :: [ positive_index() ]  }.
split_by_index_fullness( ScaledProbLikes ) ->
	split_by_index_fullness( ScaledProbLikes, _UnderFulls=[],
							 _OverFulls=[], _Idx=0 ).


% (helper)
split_by_index_fullness( _ScaledProbLikes=[], UnderFulls, OverFulls, _Idx ) ->
	{ UnderFulls, OverFulls };

split_by_index_fullness( _ScaledProbLikes=[ PL | T ], UnderFulls, OverFulls,
						 Idx ) when PL < 1 ->
	split_by_index_fullness( T, [ Idx | UnderFulls ], OverFulls, Idx+1 );

split_by_index_fullness( _ScaledProbLikes=[ _PL | T ], UnderFulls, OverFulls,
						 Idx ) -> % Implicit: when PL >= 1 ->
	split_by_index_fullness( T,  UnderFulls, [ Idx | OverFulls ], Idx+1 ).



% Allocates the unused space in an underfull entry (U) to an overfull one (O),
% so that U becomes exactly full.
%
% (helper)
%
fill_entries( _UnderFulls=[ UIdx | TU ], _OverFulls=[ OIdx | TO ], IdxArray,
			  ProbLikeArray ) ->
	% Allocate the unused space in entry O to outcome U:
	NewIdxArray = array:set( _Idx=UIdx, _Value=OIdx, IdxArray ),
	UPL = array:get( UIdx, ProbLikeArray ),
	OPL = array:get( OIdx, ProbLikeArray ),

	% Remove the allocated space from entry U:
	NewOPL = OPL + UPL - 1,

	NewProbLikeArray = array:set( OIdx, NewOPL, ProbLikeArray ),

	% Place O in the right list:
	{ NewUnderFulls, NewOverFulls } = case NewOPL < 1 of

		true ->
			{ [ OIdx | TU ], TO };

		_ ->
			{ TU, [ OIdx | TO ] }

	end,

	fill_entries( NewUnderFulls, NewOverFulls, NewIdxArray, NewProbLikeArray );


% Implicitly UnderFulls and/or OverFulls is empty here:
fill_entries( _UnderFulls, _OverFulls, IdxArray, ProbLikeArray ) ->
	{ IdxArray, ProbLikeArray }.



% @doc Returns, for the specified law specification, all the sample pairs that
% correspond to it, along with extraneous information.
%
% Useful for testing, knowing that in the general case the input samples are not
% kept in the initialised random law.
%
-spec get_all_sample_pairs( random_law_spec() ) ->
									discrete_probability_distribution().
% Starting with the non-PDF law specs:
get_all_sample_pairs( _LS={ uniform, Min, Max } ) ->
	% Any increment would do:
	Inc = ( Max - Min ) / ?default_pdf_sample_count,

	% Constant (non-normalised) probability:
	LawFun = fun( _S ) -> 1.0 / ?default_pdf_sample_count end,

	% Returns SampledPDFPairs (uniform sampling; not normalised):
	math_utils:sample_as_pairs( LawFun, Min, Max, Inc );

get_all_sample_pairs( _LS={ integer_uniform, Min, Max } ) ->
	% Constant (non-normalised) probability:
	LawFun =
		fun( S ) ->

			case math_utils:are_relatively_close( S, round( S ) ) of

				true ->
					1.0;

				false ->
					0.0

			end

		end,

	% Returns SampledPDFPairs (uniform sampling; not normalised):
	math_utils:sample_as_pairs( LawFun, Min, Max, _Inc=1 );


get_all_sample_pairs( _LS={ exponential, Lambda } ) ->

	LawFun = fun( S ) -> exponential_pdf( S, Lambda ) end,

	{ Min, Max } = math_utils:compute_support( LawFun ),

	Inc = ( Max - Min ) / ?default_pdf_sample_count,

	math_utils:sample_as_pairs( LawFun, Min, Max, Inc );


get_all_sample_pairs( _LS={ positive_integer_exponential, Lambda } ) ->

	LawFun = fun( S ) ->

			case math_utils:are_relatively_close( S, round( S ) ) of

				true ->
					exponential_pdf( S, Lambda );

				false ->
					0.0

			end

		end,

	{ Min, Max } = math_utils:compute_integer_support( LawFun ),

	% Integer distribution:
	Inc = 1,

	math_utils:sample_as_pairs( LawFun, Min, Max, Inc );



get_all_sample_pairs( _LS={ gaussian, Mu, Sigma } ) ->

	LawFun = fun( S ) -> gaussian_pdf( S, Mu, Sigma ) end,

	{ Min, Max } = math_utils:compute_support( LawFun ),

	Inc = ( Max - Min ) / ?default_pdf_sample_count,

	math_utils:sample_as_pairs( LawFun, Min, Max, Inc );


get_all_sample_pairs( _LS={ positive_integer_gaussian, Mu, Sigma  } ) ->

	LawFun = fun( S ) ->

			case math_utils:are_relatively_close( S, round( S ) ) of

				true ->
					gaussian_pdf( S, Mu, Sigma );

				false ->
					0.0

			end

		end,

	{ Min, Max } = math_utils:compute_integer_support( LawFun, _Origin=Mu,
		_MaybeMin=undefined, _MaybeMax=undefined ),

	% Integer distribution:
	Inc = 1,

	math_utils:sample_as_pairs( LawFun, Min, Max, Inc );



% First type of arbitrary spec: already with a probability distribution (as
% opposed to a PDF).
%
get_all_sample_pairs( _LS={ arbitrary, _AnyName, DistProbLikes } )
								when is_list( DistProbLikes ) ->
	% The easy case:
	DistProbLikes;

% The last category is the PDF-based (no relevant matching possible in head, LS
% tuple tag checked by canonicalise_arbitrary_pdf_spec/1):
%
get_all_sample_pairs( LS ) ->

	{ LawFun, CanonBounds, Increment } =
			case canonicalise_pdf_based_spec( LS ) of

		{ _CanonSpec={ weibull_2p, _K, _Lambda, _SampleCount, WbBounds }, Inc,
		  WbPDFFun } ->
			{ WbPDFFun, WbBounds, Inc };

		{ _CanonSpec={ weibull_3p, _K, _Lambda, _Gamma, _SampleCount,
					   WbBounds }, Inc, WbPDFFun } ->
			{ WbPDFFun, WbBounds, Inc };

		{ _CanonSpec={ arbitrary, _BinName,
				_CanonPDFInfo={ LFun, _SampleCount, CanBounds } }, Inc } ->
			{ LFun, CanBounds, Inc }

	end,

	% Returns SampledPDFPairs (uniform sampling; not normalised):
	math_utils:sample_as_pairs( LawFun, _Min=pair:first( CanonBounds ),
		_Max=pair:second( CanonBounds ), Increment ).




% @doc Returns canonical settings for the specified PDF-based specification.
%
% Logic centralised, as fully common to initialise_law/1 and
% get_all_sample_pairs/1, so that they use exactly the same results.
%
-spec canonicalise_pdf_based_spec( random_law_spec() ) ->
	full_uniform_law_spec() | integer_uniform_law_spec()
  | { full_weibull_law_spec(), increment(), weibull_pdf() }
  | { arbitrary_law_spec(), pdf(),
	  { sample_count(), bounds(), increment() } }.
% First, uniform laws:
canonicalise_pdf_based_spec( _LS={ uniform, Min, Max } ) ->
	{ Minf, Maxf } = math_utils:canonicalise_bounds( { Min, Max } ),
	_UnifLawSpec={ uniform, Minf, Maxf };

canonicalise_pdf_based_spec( _LS={ integer_uniform, Min, Max } ) ->
	{ Mini, Maxi } = math_utils:canonicalise_integer_bounds( { Min, Max } ),
	_UnifLawSpec={ integer_uniform, Mini, Maxi };


% Then exponential laws:
canonicalise_pdf_based_spec( _LS={ exponential, Lambda } ) ->
	Lambdaf = check_exponential_lambda( Lambda ),
	_ExpLawSpec={ exponential, Lambdaf };

canonicalise_pdf_based_spec( _LS={ positive_integer_exponential, Lambda } ) ->
	Lambdaf = check_exponential_lambda( Lambda ),
	_ExpLawSpec={ positive_integer_exponential, Lambdaf };


% Then gaussian laws:
canonicalise_pdf_based_spec( _LS={ gaussian, Mu, Sigma } ) ->
	Muf = check_gaussian_mu( Mu ),
	Sigmaf = check_gaussian_sigma( Sigma ),
	_GausLawSpec={ gaussian, Muf, Sigmaf };

canonicalise_pdf_based_spec( _LS={ positive_integer_gaussian, Mu, Sigma } ) ->
	Muf = check_gaussian_mu( Mu ),
	Sigmaf = check_gaussian_sigma( Sigma ),
	_GausLawSpec={ positive_integer_gaussian, Muf, Sigmaf };


% Now, the Weibull section:
%
% Weibull-2P with no sample count:
canonicalise_pdf_based_spec( _LS={ weibull_2p, K, Lambda } ) ->
	canonicalise_pdf_based_spec(
		{ weibull_2p, K, Lambda, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec( LS={ weibull_2p, K, Lambda, SampleCount } ) ->
	% Needed to compute support (no a priori bounds):
	{ WbPDFFun, Kf, Lambdaf } = get_weibull_2p_pdf( K, Lambda, LS ),
	SampleBounds = math_utils:compute_support( WbPDFFun ),

	% To be shared with next clause:
	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_2p, Kf, Lambdaf, SampleCount, SampleBounds } );

% Full information available for 2P:
canonicalise_pdf_based_spec(
		LS={ weibull_2p, K, Lambda, SampleCount, SampleBounds } ) ->

	CanonBounds = math_utils:canonicalise_bounds( SampleBounds ),

	% Checks K and Lambda:
	{ WbPDFFun, Kf, Lambdaf } = get_weibull_2p_pdf( K, Lambda, LS ),

	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_2p, Kf, Lambdaf, SampleCount, CanonBounds } );


% Weibull-3P with no sample count:
canonicalise_pdf_based_spec( _LS={ weibull_3p, K, Lambda, Gamma } ) ->
	canonicalise_pdf_based_spec(
		{ weibull_3p, K, Lambda, Gamma, ?default_pdf_sample_count } );

% No support specified:
canonicalise_pdf_based_spec(
		LS={ weibull_3p, K, Lambda, Gamma, SampleCount } ) ->

	% Needed to compute support:
	{ WbPDFFun, Kf, Lambdaf, Gammaf } =
		get_weibull_3p_pdf( K, Lambda, Gamma, LS ),

	% As not defined (negative number exponentiated) below Gammaf:
	_SampleBounds={ SMin, SMax } = math_utils:compute_support( WbPDFFun,
		_MinBound=Gammaf, _MaxBound=undefined ),

	% Apparently samples shall be greater than Gamma (see
	% https://reliawiki.org/index.php/The_Weibull_Distribution).
	%
	Min = max( SMin, Gammaf ),

	BestBounds = { Min, SMax },

	% To be shared with next clause:
	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_3p, Kf, Lambdaf, Gammaf, SampleCount, BestBounds } );

% Full information available for 3P:
canonicalise_pdf_based_spec(
		LS={ weibull_3p, K, Lambda, Gamma, SampleCount, SampleBounds } ) ->

	CanonBounds = { SMin, _SMax } =
		math_utils:canonicalise_bounds( SampleBounds ),

	SMin >= Gamma orelse throw( { too_small_lower_bound, SMin, Gamma } ),

	{ WbPDFFun, Kf, Lambdaf, Gammaf } =
		get_weibull_3p_pdf( K, Lambda, Gamma, LS ),

	canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_3p, Kf, Lambdaf, Gammaf, SampleCount, CanonBounds } );


% Arbitrary laws:
%
% No sample count defined here, using default one:
canonicalise_pdf_based_spec( _LS={ arbitrary, AnyName, _PDFInfo=LawFun } )
									when is_function( LawFun ) ->
	CountPDFInfo = { LawFun, ?default_pdf_sample_count },
	canonicalise_pdf_based_spec( { arbitrary, AnyName, CountPDFInfo } );

% Here, not having bounds (support) specified; determining them:
canonicalise_pdf_based_spec( _LS={ arbitrary, AnyName,
								  _PDFInfo={ LawFun, SampleCount } } ) ->

	SampleBounds = math_utils:compute_support( LawFun ),

	canonicalise_pdf_based_spec( { arbitrary, AnyName,
								  { LawFun, SampleCount, SampleBounds } } );

% Main clause, full information available:
canonicalise_pdf_based_spec( _LS={ arbitrary, AnyName,
		_PDFInfo={ LawFun, SampleCount, SampleBounds } } )
			when is_function( LawFun ) ->

	check_sample_count( SampleCount ),

	CanonBounds = { CMinBound, CMaxBound } =
		math_utils:canonicalise_bounds( SampleBounds ),

	CanonPDFInfo = { LawFun, SampleCount, CanonBounds },

	CanonSpec = { arbitrary, text_utils:ensure_binary( AnyName ),
				  CanonPDFInfo },

	Increment = ( CMaxBound - CMinBound ) / SampleCount,

	% Increment returned for direct reuse:
	{ CanonSpec, Increment };

canonicalise_pdf_based_spec( Other ) ->
	throw( { unsupported_random_law_spec, Other } ).



% @doc Checks that the specified term is a suitable lambda for an exponential
% law and returns it.
%
-spec check_exponential_lambda( term() ) -> rate().
check_exponential_lambda( Lambda ) when Lambda > 0 ->
	float( Lambda );

check_exponential_lambda( Other ) ->
	throw( { invalid_exponential_lambda, Other } ).



% @doc Checks that the specified term is a suitable mu (hence mean) for a
% gaussian law and returns it.
%
-spec check_gaussian_mu( term() ) -> mean().
check_gaussian_mu( Mu ) when is_number( Mu ) ->
	float( Mu );

check_gaussian_mu( Other ) ->
	throw( { invalid_mu_gaussian, Other } ).



% @doc Checks that the specified term is a suitable sigma (hence standard
% deviation) for a gaussian law and returns it.
%
-spec check_gaussian_sigma( term() ) -> standard_deviation().
check_gaussian_sigma( Sigma ) when Sigma >= 0 ->
	float( Sigma );

check_gaussian_sigma( Other ) ->
	throw( { invalid_sigma_gaussian, Other } ).



% @doc Returns the corresponding Exponential PDF, after having checked
% user-supplied parameters.
%
-spec get_exponential_pdf( term(), term() ) -> { exponential_pdf(), rate() }.
get_exponential_pdf( Lambda, _LS ) ->

	Lambdaf = check_exponential_lambda( Lambda ),

	ExpPDFFun = fun( S ) -> exponential_pdf( S, Lambdaf ) end,

	{ ExpPDFFun, Lambdaf }.



% @doc Returns the corresponding Weibull-2P PDF, after having checked
% user-supplied parameters.
%
-spec get_weibull_2p_pdf( term(), term(), term() ) ->
		{ weibull_2p_pdf(), positive_float(), positive_float() }.
get_weibull_2p_pdf( K, Lambda, LS ) ->

	K > 0.0 orelse throw( { invalid_k, K, LS } ),
	Lambda > 0.0 orelse throw( { invalid_lambda, Lambda, LS } ),

	Kf = float( K ),
	Lambdaf = float( Lambda ),

	WbPDFFun = fun( S ) -> weibull_2p_pdf( S, Kf, Lambdaf ) end,

	{ WbPDFFun, Kf, Lambdaf }.



% @doc Returns the corresponding Weibull-3P PDF, after having checked
% user-supplied parameters.
%
-spec get_weibull_3p_pdf( term(), term(), term(), term() ) ->
		{ weibull_3p_pdf(), positive_float(), positive_float(), float() }.
get_weibull_3p_pdf( K, Lambda, Gamma, LS ) ->

	K > 0.0 orelse throw( { invalid_k, K, LS } ),
	Lambda > 0.0 orelse throw( { invalid_lambda, Lambda, LS } ),

	Kf = float( K ),
	Lambdaf = float( Lambda ),
	Gammaf = float( Gamma ),

	WbPDFFun = fun( S ) -> weibull_3p_pdf( S, Kf, Lambdaf, Gammaf ) end,

	{ WbPDFFun, Kf, Lambdaf, Gammaf }.



% Common for all Weibull functions.
%
% Bounds supposed to be already canonic.
%
-spec canonicalise_weibull_spec_with( weibull_pdf(), tuple() ) ->
			{ full_weibull_law_spec(), increment(), weibull_pdf() }.
canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_2p, Kf, Lambdaf, SampleCount, WbBounds={ WbMin, WbMax } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( WbMax - WbMin ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Weibull-2P law of K=~f "
			"and Lambda=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Kf, Lambdaf, math_utils:bounds_to_string( WbBounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { weibull_2p, Kf, Lambdaf, SampleCount, WbBounds },

	{ CanonSpec, Inc, WbPDFFun };

canonicalise_weibull_spec_with( WbPDFFun,
		{ weibull_3p, Kf, Lambdaf, Gammaf, SampleCount,
		  WbBounds={ WbMin, WbMax } } ) ->

	check_sample_count( SampleCount ),

	Inc = ( WbMax - WbMin ) / SampleCount,

	% No need felt for normalisation.

	cond_utils:if_defined( myriad_debug_random,
		trace_utils:debug_fmt( "Canonicalising a Weibull-3P law of K=~f, "
			"Lambda=~f and Gamma=~f, discretised on interval ~ts "
			"with ~B points (increment: ~f).",
			[ Kf, Lambdaf, Gammaf, math_utils:bounds_to_string( WbBounds ),
			  SampleCount, Inc ] ) ),

	CanonSpec = { weibull_3p, Kf, Lambdaf, Gammaf, SampleCount, WbBounds },

	{ CanonSpec, Inc, WbPDFFun }.



% @doc Returns a new sample drawn from the discrete probability distribution
% specified through its (constant) law data (which is thus not returned),
% this table having been obtained initially (and once for all) from
% its random specification (see initialise_law/1).
%
% Each sample is generated in constant time O(1) time with regard to the number
% of samples declared in the corresponding distribution.
%
% Such a generation depends (and modifies) the state of the underlying uniform
% random generator (e.g. see start_random_source/0); precisely each non-uniform
% sampling results in two underlying uniform samples to be drawn.
%
-spec get_sample_from( random_law_data() ) -> sample().
get_sample_from( { _LawData={ uniform, Min, Max }, undefined } ) ->
	get_uniform_floating_point_value( Min, Max );

get_sample_from( { _LawData={ integer_uniform, Nmin, Nmax }, undefined } ) ->
	get_uniform_value( Nmin, Nmax );

get_sample_from( { _LawData={ exponential, Lambda }, undefined } ) ->
	get_exponential_value( Lambda );

get_sample_from( { _LawData={ positive_integer_exponential, Lambda },
				   undefined } ) ->
	get_positive_integer_exponential_value( Lambda );

get_sample_from( { _LawData={ gaussian, Mu, Sigma }, undefined } ) ->
	get_gaussian_value( Mu, Sigma );

get_sample_from( { _LawData={ positive_integer_gaussian, Mu, Sigma } ,
				   undefined } ) ->
	get_positive_integer_gaussian_value( Mu, Sigma );

get_sample_from( { _AnyRandomLawData, AliasTable } ) ->
	get_sample_from_table( AliasTable ).



% @doc Returns the specified number of samples drawn according to the specified
% law data.
%
% Refer to get_sample_from/1 for more details.
%
-spec get_samples_from( sample_count(), random_law_data() ) -> [ sample() ].
get_samples_from( Count, LawData ) ->

	trace_utils:debug_fmt( "Drawing ~B samples from ~ts.",
						   [ Count, law_data_to_string( LawData ) ] ),

	% Laws are static:
	[ get_sample_from( LawData ) || _ <- lists:seq( 1, Count ) ].



% @doc Returns a new sample drawn from the discrete probability distribution
% specified through its (constant) alias table (which is thus not returned),
% this table having been obtained initially (and once for all) from
% generate_alias_table_from/1.
%
% Each sample is generated in constant time O(1) time with regard to the number
% of samples declared in the corresponding distribution.
%
% Such a generation depends (and modifies) the state of the underlying uniform
% random generator (e.g. see start_random_source/0); precisely each non-uniform
% sampling results in two underlying uniform samples to be drawn.
%
-spec get_sample_from_table( alias_table() ) -> sample().
get_sample_from_table( #alias_table{ entry_count=EntryCount,
									 sample_values=SampleValueArray,
									 indexes=IndexArray,
									 prob_likes=ProbLikeArray } ) ->

	% Thus uniform in [0, EntryCount-1]:
	PLIdx = get_uniform_value( EntryCount ) - 1,

	% Thus uniform in [0.0, 1.0[:
	P = get_uniform_value(),

	SampleIdx = case P =< array:get( PLIdx, ProbLikeArray ) of

		true ->
			PLIdx;

		_ ->
			array:get( PLIdx, IndexArray )

	end,

	array:get( SampleIdx, SampleValueArray ).



% Extra PDFs, which are notably useful for reliability-related computations.
%
% See also:
% https://reliability.readthedocs.io/en/latest/Equations%20of%20supported%20distributions.html
%
% Each PDF could be implemented according to either of these two approaches:
% A: it is simply defined analytically, and then sampled
% B: it is defined directly in terms of a another, more fundamental PDF, with no
% sampling



% Exponential distribution:
%
% See https://en.wikipedia.org/wiki/Exponential_distribution.
%
% Lambda > 0 is the parameter of the distribution, often called the rate
% parameter.
%
-spec exponential_pdf( positive_float_sample(), rate() ) -> probability().
exponential_pdf( S, Lambda ) when S >= 0.0 ->
	Lambda * exp( - S * Lambda );

exponential_pdf( _S, _Lambda ) -> % when S < 0.0 ->
	0.0.



% Gaussian (normal) distribution:
%
% See https://en.wikipedia.org/wiki/Normal_distribution.
%
% Mu is the mean or expectation of the distribution (and also its median and
% mode), while Sigma is its standard deviation.
%
-spec gaussian_pdf( positive_float_sample(), mean(), standard_deviation() ) ->
								probability().
gaussian_pdf( S, Mu, Sigma ) ->
	1.0 / ( Sigma * math:sqrt( 2*math:pi() ) )
		* math:exp( - math:pow( ( S - Mu ) / Sigma, 2 ) / 2 ).



% Weibull-2P distribution:
%
% See https://en.wikipedia.org/wiki/Weibull_distribution.
%
% Its support is for a sample S>=0.0.
%
% Determined by 2 parameters:
% - K > 0 is the shape parameter (sometimes named beta)
% - Lambda > 0 is the scale parameter (sometimes named alpha)
%
% Being quite flexible, its proper parametrisation can cover many laws,
% including the exponential law (K=1) and the Rayleigh law (K=2 and
% Lambda=sqrt(2).Sigma).
%
-spec weibull_2p_pdf( positive_float_sample(), positive_float(),
				   positive_float() ) -> probability().
weibull_2p_pdf( S, K, Lambda ) when S >= 0.0 ->
	A = S / Lambda,
	K/Lambda * pow( A, K-1 ) * exp( -pow( A, K ) );

weibull_2p_pdf( _S, _K, _Lambda ) -> % when S < 0.0 ->
	0.0.


% Weibull-3P distribution:
%
% See https://en.wikipedia.org/wiki/Weibull_distribution.
%
% Its support is for a sample S>=Gamma.
%
% Determined by 2 parameters:
% - K > 0 is the shape parameter (sometimes named beta)
% - Lambda > 0 is the scale parameter (sometimes named alpha)
% - Gamma (in R) is the location parameter (or failure free life)
%
% Being quite flexible, its proper parametrisation can cover many laws,
% including the Weibull-2P ones (with Gamma=0).
%
-spec weibull_3p_pdf( positive_float_sample(), positive_float(),
					  positive_float(), float() ) -> probability().
weibull_3p_pdf( S, K, Lambda, Gamma ) when S >= 0.0 ->
	A = ( S - Gamma ) / Lambda,
	K/Lambda * pow( A, K-1 ) * exp( -pow( A, K ) );

weibull_3p_pdf( _S, _K, _Lambda, _Gamma ) -> % when S < 0.0 ->
	0.0.




% @doc Checks that the specified term is a sample count (and returns it).
-spec check_sample_count( term() ) -> sample_count().
check_sample_count( C ) when is_integer( C ) andalso C > 0 ->
	C;

check_sample_count( C ) ->
	throw( { invalid_sample_count, C } ).



% @doc Returns a textual representation of the specified random law
% specification.
%
-spec law_spec_to_string( random_law_spec() ) -> ustring().
% In specs, non-canonical types (e.g. integers instead of floats) may be
% encountered, so ~w/~p are more appropriate:
%
law_spec_to_string( { uniform, Max } ) ->
	law_spec_to_string( { uniform, _Min=0.0, Max } );

law_spec_to_string( { uniform, Min, Max } ) ->
	% They may be numbers:
	text_utils:format( "uniform law in [~w,~w]", [ Min, Max ] );

law_spec_to_string( { integer_uniform, Nmax } ) ->
	law_spec_to_string( { integer_uniform, _Nmin=0, Nmax } );

law_spec_to_string( { integer_uniform, Nmin, Nmax } ) ->
	text_utils:format( "integer uniform law in [~w,~w]", [ Nmin, Nmax ] );


law_spec_to_string( { exponential, Lambda } ) ->
	text_utils:format( "exponential law of rate lamba=~w", [ Lambda ] );

law_spec_to_string( { positive_integer_exponential, Lambda } ) ->
	text_utils:format( "integer exponential law of rate lamba=~w",
					   [ Lambda ] );


law_spec_to_string( { gaussian, Mu, Sigma } ) ->
	text_utils:format( "gaussian law of mean mu=~w and standard deviation "
					   "sigma=~w", [ Mu, Sigma ] );

law_spec_to_string( { positive_integer_gaussian, Mu, Sigma } ) ->
	text_utils:format( "positive integer gaussian law of mean mu=~w and "
					   "standard deviation sigma=~w", [ Mu, Sigma ] );


law_spec_to_string( { weibull_2p, K, Lambda } ) ->
	text_utils:format( "Weibull-2P law of shape parameter k=~w and "
		"scale parameter lambda=~w", [ K, Lambda ] );

law_spec_to_string( { weibull_2p, K, Lambda, SampleCount } ) ->
	text_utils:format( "Weibull-2P law of shape parameter k=~w and "
		"scale parameter lambda=~w (sample count: ~B)",
		[ K, Lambda, SampleCount ] );


law_spec_to_string( { weibull_3p, K, Lambda, Gamma } ) ->
	text_utils:format( "Weibull-3P law of shape parameter k=~w, "
		"scale parameter lambda=~w and location parameter gamma=~w",
		[ K, Lambda, Gamma ] );

law_spec_to_string( { weibull_3p, K, Lambda, Gamma, SampleCount } ) ->
	text_utils:format( "Weibull-3P law of shape parameter k=~w, "
		"scale parameter lambda=~w and location parameter gamma=~w"
		"(sample count: ~B)", [ K, Lambda, Gamma, SampleCount ] );


law_spec_to_string( { arbitrary, Name, PDFInfo } ) when is_tuple( PDFInfo ) ->
	text_utils:format( "arbitrary law named '~ts', an ~ts",
					   [ Name, pdf_info_to_string( PDFInfo ) ] );

law_spec_to_string( { arbitrary, Name, ProbDist } ) when is_list( ProbDist ) ->
	text_utils:format( "arbitrary law named '~ts', based on a distribution "
					   "of ~B samples", [ Name, length( ProbDist ) ] ).



% @doc Returns a textual representation of the specified random law data.
-spec law_data_to_string( random_law_data() ) -> ustring().
law_data_to_string( { _LawSettings={ uniform, Min, Max },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "uniform law in [~f, ~f]", [ Min, Max ] );

law_data_to_string( { _LawSettings={ integer_uniform, NMin, NMax },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "integer uniform law in [~B, ~B]", [ NMin, NMax ] );


law_data_to_string( { _LawSettings={ exponential, Lambda },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "exponential law of Lambda=~f", [ Lambda ] );

law_data_to_string( { _LawSettings={ positive_integer_exponential, Lambda },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "integer exponential law of Lambda=~f", [ Lambda ] );


law_data_to_string( { _LawSettings={ gaussian, Mu, Sigma },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "gaussian law of Mu=~f and Sigma=~f",
					   [ Mu, Sigma ] );

law_data_to_string( { _LawSettings={ positive_integer_gaussian, Mu, Sigma },
					  _MaybeAliasTable=undefined } ) ->
	text_utils:format( "integer gaussian law of Mu=~f and Sigma=~f",
					   [ Mu, Sigma ] );


law_data_to_string( { _LawSettings={ weibull_2p, K, Lambda, SamplingInfo },
					  _MaybeAliasTable } ) ->
	text_utils:format( "Weibull-2P law of K=~f and Lambda=~f, ~ts",
		[ K, Lambda, sampling_info_to_string( SamplingInfo ) ] );

law_data_to_string( { _LawSettings={ weibull_3p, K, Lambda, Gamma,
									 SamplingInfo }, _MaybeAliasTable } ) ->
	text_utils:format( "Weibull-3P law of K=~f, Lambda=~f and Gamma=~f, ~ts",
		[ K, Lambda, Gamma, sampling_info_to_string( SamplingInfo ) ] );


% For basic laws:
law_data_to_string( { _LawSettings={ arbitrary, BinName, SamplingInfo },
					  _MaybeAliasTable } ) ->
	text_utils:format( "arbitrary law named '~ts', ~ts",
					   [ BinName, sampling_info_to_string( SamplingInfo ) ] ).


% @doc Returns a textual representation of the specified PDF information.
-spec pdf_info_to_string( pdf_info() ) -> ustring().
pdf_info_to_string( { _PDF, SampleCount, SampleBounds } ) ->
	text_utils:format( "arbitrary distribution, based on ~B samples, on ~ts",
		[ SampleCount, math_utils:bounds_to_string( SampleBounds ) ] );

pdf_info_to_string( { _PDF, SampleCount } ) ->
	text_utils:format( "arbitrary distribution, based on ~B samples",
					   [ SampleCount ] );

pdf_info_to_string( _PDF ) ->
	"arbitrary distribution".



% @doc Returns a textual representation of the specified sampling information.
-spec sampling_info_to_string( sampling_info() ) -> ustring().
sampling_info_to_string(
		_SamplingInfo={ StartSample, StopSample, SampleCount } ) ->

	Inc = ( StopSample - StartSample ) / SampleCount,

	text_utils:format( "sampled on ~ts with ~B points "
		"(corresponding increment of ~f)",
		[ math_utils:bounds_to_string( { StartSample, StopSample } ),
		  SampleCount, Inc ] );

sampling_info_to_string( SampleCount ) ->
	text_utils:format( "sampled on ~B points", [ SampleCount ] ).
