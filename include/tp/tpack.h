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

	template<typename... Ts>
	using tail_t = decltype(tail(tpack_v<Ts...>));

	template<typename... TPs>
	constexpr auto concat(TPs... tps) {
		return (tps + ... + nil_v);
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

		template<typename IS, typename TS>
		struct indexed_types;

		template<std::size_t I, typename T>
		constexpr auto get_indexed_type(const indexed_type<I, T>*) -> unit<T>;

		template<std::size_t... Is, typename... Ts>
		struct indexed_types<std::index_sequence<Is...>, tpack<Ts...>> : indexed_type<Is, Ts>... {
			// extract arbitrary types given their indexes Js
			template<std::size_t... Js>
			constexpr auto get(std::index_sequence<Js...>) const
			-> tpack<typename decltype(get_indexed_type<Js>(this))::type...> { return {}; }
		};

		template<typename... Ts>
		using indexed_types_for = indexed_types<std::index_sequence_for<Ts...>, tpack<Ts...>>;

	} // namespace detail

	template<std::size_t I, typename... Ts>
	constexpr auto get(tpack<Ts...>) -> decltype(
		detail::get_indexed_type<I>(std::declval<detail::indexed_types_for<Ts...>*>())
	) {
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
			constexpr auto N = size(tp);
			constexpr auto itp = indexed_types<std::index_sequence<Is...>, TP>{};
			return itp.get(std::index_sequence<(N - Is - 1)...>{});
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

	// find_if
	template<std::size_t From, std::size_t To, typename... Ts, typename Pred>
	constexpr auto find_if(tpack<Ts...>, Pred f) -> std::size_t {
		(void)f;
		constexpr auto N = sizeof...(Ts);
		if constexpr (To == 0 || From >= To)
			return N;
		else {
			std::size_t res = 0;
			const auto pred = [&]<typename U>(unit<U>) -> bool {
				if constexpr (To < N) if (res >= To) {
					res = N;
					return true;
				}
				if constexpr (From > 0) if (res < From) {
					++res;
					return false;
				}
				return f(unit_v<U>) ? true : (++res, false);
			};
			(void)(pred(unit_v<Ts>) || ...);
			return res;
		}
	}

	template<std::size_t From, std::size_t To, template<typename...> typename Pred, typename... Ts>
	constexpr auto find_if(tpack<Ts...> x) {
		return find_if<From, To>(x, meta::value_adapter_v<Pred>);
	}

	template<typename... Ts, typename Pred>
	constexpr auto find_if(tpack<Ts...>, Pred f) -> std::size_t {
		(void)f;
		std::size_t res = 0;
		(void)((f(unit_v<Ts>) ? true : (++res, false)) || ...);
		return res;
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto find_if(tpack<Ts...> x) {
		return find_if(x, meta::value_adapter_v<Pred>);
	}

	// find
	template<std::size_t From, std::size_t To, typename T, typename... Ts>
	constexpr auto find(tpack<Ts...> tp) -> std::size_t {
		return find_if<From, To>(tp, meta::value_adapter_v<std::is_same, T>);
	}

	template<std::size_t From, std::size_t To, typename... Ts, typename T>
	constexpr auto find(tpack<Ts...> x, unit<T>) { return find<From, To, T>(x); }

	template<typename T, typename... Ts>
	constexpr auto find(tpack<Ts...> tp) -> std::size_t {
		return find_if(tp, meta::value_adapter_v<std::is_same, T>);
	}

	template<typename... Ts, typename T>
	constexpr auto find(tpack<Ts...> x, unit<T>) { return find<T>(x); }

	// all_of
	template<typename... Ts, typename Pred>
	constexpr bool all_of(tpack<Ts...>, Pred pred) {
		(void)pred;
		return (pred(unit_v<Ts>) && ...);
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto all_of(tpack<Ts...> x) {
		return all_of(x, meta::value_adapter_v<Pred>);
	}

	// any_of
	template<typename... Ts, typename Pred>
	constexpr bool any_of(tpack<Ts...>, Pred pred) {
		(void)pred;
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
			return tpack_v<meta::fn_result_t<F, std::index_sequence<Is>>...>;
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
		(void)f;
		return (detail::filter_one(unit_v<Ts>, f) + ... + nil_v);
	}

	template<template<typename...> typename Pred, typename... Ts>
	constexpr auto filter(tpack<Ts...> tp) {
		return filter(tp, meta::value_adapter_v<Pred>);
	}

	// distinct
	namespace detail {

		template<typename... Ts>
		struct distinct_chain {
			using type = tpack<Ts...>;

			template<typename U>
				requires (find<U, Ts...>({}) == sizeof...(Ts))
			constexpr auto operator+(distinct_chain<U>) const -> distinct_chain<Ts..., U> { return {}; }

			template<typename U>
			constexpr auto operator+(distinct_chain<U>) const -> distinct_chain { return {}; }
		};

	} // namespace detail

	template<typename... Ts>
	constexpr auto distinct(tpack<Ts...>) {
		using namespace detail;
		return typename decltype((distinct_chain<>{} + ... + distinct_chain<Ts>{}))::type{};
	}

	// fold_left, fold_right
	namespace detail {

		template<typename F, typename RS>
		struct folder;
		
		template<typename F, typename... Rs>
		struct folder<F, tpack<Rs...>> {
			using type = tpack<Rs...>;
		};

		template<typename T, typename F, typename... Rs>
		constexpr auto operator+(unit<T>, folder<F, tpack<Rs...>>) {
			if constexpr (sizeof...(Rs))
				return folder<F, decltype(std::declval<F>()(tpack_v<T, Rs...>))>{};
			else
				return folder<F, unit<T>>{};
		}

		template<typename F, typename... Rs, typename T>
		constexpr auto operator+(folder<F, tpack<Rs...>>, unit<T>) {
			if constexpr (sizeof...(Rs))
				return folder<F, decltype(std::declval<F>()(tpack_v<Rs..., T>))>{};
			else
				return folder<F, unit<T>>{};
		}

		template<typename... Ts, typename F>
		constexpr auto do_fold_left(tpack<Ts...>, F) {
			return typename decltype((folder<F, nil_tpack>{} + ... + unit_v<Ts>))::type{};
		}

		template<typename... Ts, typename... Rs, typename F>
		constexpr auto do_fold_right(tpack<Ts...>, F) {
			return typename decltype((unit_v<Ts> + ... + folder<F, nil_tpack>{}))::type{};
		}

	} // namespace detail

	template<typename... Ts, typename F, typename D = nil_tpack>
	constexpr auto fold_left(tpack<Ts...> tp, F f, D seed = nil_v) {
		return detail::do_fold_left(seed + tp, f);
	}

	template<template<typename L, typename R> typename F, typename... Ts, typename D = nil_tpack>
	constexpr auto fold_left(tpack<Ts...> tp, D seed = nil_v) {
		return fold_left(tp, meta::type_adapter_v<F>, seed);
	}

	template<typename... Ts, typename F, typename D = nil_tpack>
	constexpr auto fold_right(tpack<Ts...> tp, F f, D seed = nil_v) {
		return detail::do_fold_right(tp + seed, f);
	}

	template<template<typename L, typename R> typename F, typename... Ts, typename D = nil_tpack>
	constexpr auto fold_right(tpack<Ts...> tp, D seed = nil_v) {
		return fold_right(tp, meta::type_adapter_v<F>, seed);
	}

} // namespace tp
