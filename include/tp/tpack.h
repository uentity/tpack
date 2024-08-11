#pragma once

#include <cstddef>
#include <type_traits>
#include <utility>

namespace tp {

	template<typename... Ts>
	struct tpack {};

	template<typename T>
	struct tpack<T> {
		using type = T;
	};

	template<typename... Ts>
	inline constexpr auto tpack_v = tpack<Ts...>{};

	// `unit` is an alias to `tpack<T>` with single element
	template<typename T>
	using unit = tpack<T>;

	template<typename T>
	inline constexpr auto unit_v = unit<T>{};

	// `nil` is empty tpack
	using nil_tpack = tpack<>;
	inline constexpr auto nil_v = nil_tpack{};
	// [NOTE] `tpack_v<nil_tpack>` is not the same as `nil_v`:
	// tpack<tpack<>> != tpack<>

	// operators
	template<typename... Ts, typename... Us>
	constexpr bool operator==(tpack<Ts...>, tpack<Us...>) { return false; }

	template<typename... Ts>
	constexpr bool operator==(tpack<Ts...>, tpack<Ts...>) { return true; }

	template<typename... Ts, typename... Us>
	constexpr bool operator!=(tpack<Ts...> x, tpack<Us...> y) { return !(x == y); }

	template<typename... Ts, typename... Us>
	constexpr auto operator+(tpack<Ts...>, tpack<Us...>) -> tpack<Ts..., Us...> { return {}; }

	// test if type is tpack
	template<typename T>
	struct is_tpack : std::false_type {};

	template<typename... Ts>
	struct is_tpack<tpack<Ts...>> : std::true_type {};

	template<typename T>
	using is_tpack_t = is_tpack<std::remove_cvref_t<T>>::type;

	template<typename T>
	inline constexpr bool is_tpack_v = is_tpack<std::remove_cvref_t<T>>::value;

	// helper to suppress unused expression result compiler warnings
	struct ignore_t {
		constexpr ignore_t() {}

		template<typename T>
		constexpr auto& operator=(T&&) const { return *this; }
	};

	inline constexpr auto ignore = ignore_t{};

	// utilities for working with meta-functions (aka struct specializations)
	namespace meta {

		template<typename... Ts>
		inline constexpr bool static_false = false;

		// adapters for meta functions -> constexpr functions taking type packs (typically `unit`)
		template<template<typename...> typename F, typename... Ts>
		struct value_adapter {
			template<typename... Us>
			static constexpr auto call(tpack<Us...>) {
				return F<Ts..., Us...>::value;
			}

			template<typename... Us>
			constexpr auto operator()(tpack<Us...> x) const { return call(x); }
		};

		template<template<typename...> typename F, typename... Ts>
		inline constexpr auto value_adapter_v = value_adapter<F, Ts...>{};

		template<template<std::size_t...> typename F, std::size_t... Is>
		struct idx_value_adapter {
			template<std::size_t... Js>
			static constexpr auto call(std::index_sequence<Js...>) {
				return F<Is..., Js...>::value;
			}

			template<std::size_t... Js>
			constexpr auto operator()(std::index_sequence<Js...> js) const { return call(js); }
		};

		template<template<std::size_t...> typename F, std::size_t... Is>
		inline constexpr auto idx_value_adapter_v = idx_value_adapter<F, Is...>{};

		template<template<typename...> typename F, typename... Ts>
		struct type_adapter {
			template<typename... Us>
			using type = unit<typename F<Ts..., Us...>::type>;

			template<typename... Us>
			static constexpr auto call(tpack<Us...>) -> type<Us...> { return {}; }

			template<typename... Us>
			constexpr auto operator()(tpack<Us...>) const -> type<Us...> { return {}; }
		};

		template<template<typename...> typename F, typename... Ts>
		inline constexpr auto type_adapter_v = type_adapter<F, Ts...>{};

		template<template<std::size_t...> typename F, std::size_t... Is>
		struct idx_type_adapter {
			// had to go through additional instantiation context, otherwise don't compile
			// with template aliases like `detail::gen_identity`
			template<std::size_t... Js>
			struct build {
				using type = unit<typename F<Is..., Js...>::type>;
			};

			template<std::size_t... Js>
			using type = typename build<Js...>::type;

			template<std::size_t... Js>
			static constexpr auto call(std::index_sequence<Js...>) -> type<Js...> { return {}; }

			template<std::size_t... Js>
			constexpr auto operator()(std::index_sequence<Js...>) const -> type<Js...> { return {}; }
		};

		template<template<std::size_t...> typename F, std::size_t... Is>
		inline constexpr auto idx_type_adapter_v = idx_type_adapter<F, Is...>{};

		template<template<typename...> typename F, typename... Ts>
		struct apply {
			template<typename... Us>
			using type = unit<F<Ts..., Us...>>;

			template<typename... Us>
			static constexpr auto call(tpack<Us...>) -> type<Us...> { return {}; }

			template<typename... Us>
			constexpr auto operator()(tpack<Us...>) const -> type<Us...> { return {}; }
		};

		template<typename F, typename... Args>
		using fn_result_t = decltype(std::declval<F>()(std::declval<Args>()...))::type;

	} // namespace ddv::meta

	// apply - only makes sense for metafunctions
	template<template<typename...> typename F, typename... Ts>
	constexpr auto apply(tpack<Ts...>) -> meta::type_adapter<F>::template type<Ts...> {
		return {};
	}

	template<template<typename...> typename F, typename... Ts>
	constexpr auto apply_v(tpack<Ts...> tp) {
		return meta::value_adapter<F>::call(tp);
	}

	template<template<typename...> typename F, typename... Ts>
	constexpr auto make_v(tpack<Ts...>) -> meta::apply<F>::template type<Ts...> {
		return {};
	}

	template<template<typename...> typename F, typename TP>
	using make = decltype(make_v<F>(std::declval<TP>()))::type;

	// basic API
	template<typename... Ts>
	constexpr auto size(tpack<Ts...>) { return sizeof...(Ts); }

	template<typename... Ts>
	constexpr bool empty(tpack<Ts...> tp) { return size(tp) == 0; }

	template<typename T, typename... Ts>
	constexpr auto head(tpack<T, Ts...>) -> unit<T> { return {}; }

	constexpr auto head(nil_tpack) -> nil_tpack { return {}; }

	template<typename... Ts>
	using head_t = decltype(head(tpack_v<Ts...>))::type;

	template<typename T, typename... Ts>
	constexpr auto tail(tpack<T, Ts...>) -> tpack<Ts...> { return {}; }

	constexpr auto tail(nil_tpack) -> nil_tpack { return {}; }

	template<typename... TPs>
	constexpr auto concat(TPs... tps) {
		return (tps + ...);
	}

	// unwrap: strips `unit<unit<...tpack<Ts...>...>>` -> tpack<Ts...>
	template<typename T, typename... Ts>
	constexpr auto strip(tp::tpack<T, Ts...> tp) {
		if constexpr (sizeof...(Ts) == 0 && is_tpack_v<T>)
			return strip(T{});
		else
			return tp;
	}

	constexpr auto strip(nil_tpack) -> nil_tpack { return {}; }

	// push front/back
	template<typename T, typename... Ts>
	constexpr auto push_front(tpack<Ts...>) -> tpack<T, Ts...> { return {}; }

	template<typename... Ts, typename T>
	constexpr auto push_front(tpack<Ts...>, unit<T>) -> tpack<T, Ts...> { return {}; }

	template<typename T, typename... Ts>
	constexpr auto pop_front(tpack<T, Ts...>) -> tpack<Ts...> { return {}; }

	template<typename T, typename... Ts>
	constexpr auto push_back(tpack<Ts...>) -> tpack<Ts..., T> { return {}; }

	template<typename... Ts, typename T>
	constexpr auto push_back(tpack<Ts...>, unit<T>) -> tpack<Ts..., T> { return {}; }

	// get
	namespace detail {

		// Algorithm based on fold inheritance and then casting to one of the bases
		// demonstrates the best performance among alternatives
		template<std::size_t I, typename T>
		struct indexed_type {};

		template<typename Is, typename... Ts>
		struct indexed_types;

		template<std::size_t... Is, typename... Ts>
		struct indexed_types<std::index_sequence<Is...>, Ts...> : indexed_type<Is, Ts>... {};

		template<typename... Ts>
		using indexed_types_for = indexed_types<std::index_sequence_for<Ts...>, Ts...>;

		template<std::size_t I, typename T>
		constexpr unit<T> get_indexed_type(indexed_type<I, T>*);

	} // namespace detail

	template<std::size_t I, typename... Ts>
	constexpr auto get(tpack<Ts...>)
	-> decltype( detail::get_indexed_type<I>(std::declval<detail::indexed_types_for<Ts...>*>()) ) {
		return {};
	}

	// back
	template<typename... Ts>
	constexpr auto back(tpack<Ts...> tp) {
		if constexpr(empty(tp))
			return nil_v;
		else
			return get<size(tp) - 1>(tp);
	}

	template<typename... Ts>
	using back_t = decltype(back(tpack_v<Ts...>))::type;

	// pop_back
	namespace detail {

		template<std::size_t... Is, typename TP>
		constexpr auto get_first(std::index_sequence<Is...>, TP tp) {
			return tpack_v<typename decltype(get<Is>(tp))::type...>;
		}

	} // namespace detail

	template<typename... Ts>
	constexpr auto pop_back(tpack<Ts...> tp) {
		if constexpr(empty(tp))
			return tp;
		else
			return detail::get_first(std::make_index_sequence<size(tp) - 1>{}, tp);
	}

	// reverse
	namespace detail {

		template<std::size_t... Is, typename TP>
		constexpr auto do_reverse(std::index_sequence<Is...>, TP tp) {
			return tpack_v<typename decltype(get<size(tp) - Is - 1>(tp))::type...>;
		}

	} // namespace detail

	template<typename... Ts>
	constexpr auto reverse(tpack<Ts...> tp) {
		return detail::do_reverse(std::index_sequence_for<Ts...>{}, tp);
	}

	// contains
	template<typename T, typename... Ts>
	constexpr bool contains(tpack<Ts...>) {
		return (std::is_same_v<T, Ts> || ...);
	}

	template<typename... Ts, typename T>
	constexpr bool contains(tpack<Ts...> x, unit<T>) { return contains<T>(x); }

	// find
	template<std::size_t From, typename T, typename... Ts>
	constexpr auto find(tpack<Ts...>) -> std::size_t {
		std::size_t res = 0;
		(void)((res < From ? ++res, false : (std::is_same_v<T, Ts> ? true : (++res, false))) || ...);
		return res;
	}

	template<typename T, typename... Ts>
	constexpr auto find(tpack<Ts...> tp) -> std::size_t { return find<0, T>(tp); }

	template<std::size_t From, typename... Ts, typename T>
	constexpr auto find(tpack<Ts...> x, unit<T>) { return find<From, T>(x); }

	template<typename... Ts, typename T>
	constexpr auto find(tpack<Ts...> x, unit<T>) { return find<T>(x); }

	// find_if
	template<typename... Ts, typename Pred>
	constexpr auto find_if(tpack<Ts...>, Pred f) -> std::size_t {
		std::size_t res = 0;
		(void)( (f(unit_v<Ts>) ? true : (++res, false)) || ... );
		return res;
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto find_if(tpack<Ts...> x) {
		return find_if(x, meta::value_adapter_v<Pred>);
	}

	// all_of
	template<typename... Ts, typename Pred>
	constexpr bool all_of(tpack<Ts...>, Pred f) {
		return (f(unit_v<Ts>) && ...);
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto all_of(tpack<Ts...> x) {
		return all_of(x, meta::value_adapter_v<Pred>);
	}

	// any_of
	template<typename... Ts, typename Pred>
	constexpr bool any_of(tpack<Ts...>, Pred pred) {
		return (pred(unit_v<Ts>) || ...);
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto any_of(tpack<Ts...> x) {
		return any_of(x, meta::value_adapter_v<Pred>);
	}

	// none_of
	template<typename... Ts, typename Pred>
	constexpr bool none_of(tpack<Ts...> x, Pred f) {
		return !any_of(x, f);
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto none_of(tpack<Ts...> x) {
		return none_of(x, meta::value_adapter_v<Pred>);
	}

	// transform
	template<typename... Ts, typename F>
	constexpr auto transform(tpack<Ts...>, F) -> tpack<meta::fn_result_t<F, unit<Ts>>...> {
		return {};
	}

	template<template<typename...> typename F, typename... Ts>
	constexpr auto transform(tpack<Ts...>) {
		return tpack_v<typename F<Ts>::type...>;
	}

	// generate
	namespace detail {

		template<typename F, std::size_t... Is>
		constexpr auto do_generate(F, std::index_sequence<Is...>) {
			return tpack<meta::fn_result_t<F, std::index_sequence<Is>>...>{};
		}

		template<typename T>
		struct gen_identity {
			template<std::size_t> using type = unit<T>;
		};

	} // namespace detail

	template<std::size_t N, typename F>
	constexpr auto generate(F generator) {
		return detail::do_generate(generator, std::make_index_sequence<N>{});
	}

	template<std::size_t N, template<std::size_t> typename F>
	constexpr auto generate() {
		return generate<N>(meta::idx_type_adapter_v<F>);
	}

	template<std::size_t N, typename T>
	constexpr auto generate() {
		using detail::gen_identity;
		return generate<N, gen_identity<T>::template type>();
	}

	// filter
	namespace detail {
		
		template<typename T, typename F>
		constexpr auto filter_one(unit<T> t, F f) {
			if constexpr (f(t))
				return t;
			else
				return nil_v;
		}

	} // namespace detail

	template<typename... Ts, typename Pred>
	constexpr auto filter(tpack<Ts...>, Pred f) {
		return (detail::filter_one(unit_v<Ts>, f) + ... + nil_v);
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto filter(tpack<Ts...> tp) {
		return filter(tp, meta::value_adapter_v<Pred>);
	}

	// distinct
	namespace detail {

		template<typename... Ts, typename... Rs>
		constexpr auto do_distinct(tpack<Ts...> src, tpack<Rs...> dst) {
			if constexpr (empty(src))
				return dst;
			else {
				const auto first = head(src);
				if constexpr (find(dst, first) < size(dst))
					return do_distinct(pop_front(src), dst);
				else
				 	return do_distinct(pop_front(src), push_back(dst, first));
			}
		}

	} // namespace detail

	template<typename... Ts>
	constexpr auto distinct(tp::tpack<Ts...> tp) {
		return detail::do_distinct(tp, nil_v);
	}

	// fold_left, fold_right
	namespace detail {

		template<typename T, typename R, typename F>
		constexpr auto do_fold_left(T, R, F) {
			static_assert(false, "Folding meta-function `F` must produce `F::value` of type `unit<T>`");
		}

		template<typename... Ts, typename... Rs, typename F>
		constexpr auto do_fold_left(tpack<Ts...> tp, tpack<Rs...> res, F f) {
			if constexpr (empty(tp))
				return res;
			else
				//return do_fold_left(pop_front(tp), f(res + head(tp)), f);
				return do_fold_left(pop_front(tp), f(tpack_v<Rs..., typename decltype(head(tp))::type>), f);
		}

	} // namespace detail

	template<typename... Ts, typename F, typename D = nil_tpack>
	constexpr auto fold_left(tpack<Ts...> tp, F f, D seed = nil_v) {
		if constexpr (empty(seed) && size(tp) > 0)
			return detail::do_fold_left(pop_front(tp), head(tp), f);
		else
			return detail::do_fold_left(tp, seed, f);
	}

	// If `F` is a classic meta-function that calculates `F::type`,
	// use `fold_left(tp, meta::type_adapter_v<F>, seed)` call
	template<template<typename L, typename R> typename F, typename... Ts, typename D = nil_tpack>
	constexpr auto fold_left(tpack<Ts...> tp, D seed = nil_v) {
		return fold_left(tp, meta::value_adapter_v<F>, seed);
	}

	template<typename... Ts, typename F, typename D = nil_tpack>
	constexpr auto fold_right(tpack<Ts...> tp, F f, D seed = nil_v) {
		return fold_left(reverse(tp), f, seed);
	}

	// If `F` is a classic meta-function that calculates `F::type`,
	// use `fold_right(tp, meta::type_adapter_v<F>, seed)` call
	template<template<typename L, typename R> typename F, typename... Ts, typename D = nil_tpack>
	constexpr auto fold_right(tpack<Ts...> tp, D seed = nil_v) {
		return fold_right(tp, meta::value_adapter_v<F>, seed);
	}

} // namespace tp
